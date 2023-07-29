; ----------------------------------------------------------------------------------------------------

.define sdc_transferbuffer	$0200
.define sdc_sectorbuffer	$7e00

sdc_bytecounterlo	.byte 0
sdc_bytecounterhi	.byte 0
sdc_sectorcount		.byte 0

sdc_filedescriptor	.byte 0

; ----------------------------------------------------------------------------------------------------

sdc_opendir

		lda #$00
		sta $d640
		nop
		ldz #$00

		lda #$12										; hyppo_opendir - open the current working directory
		sta $d640
		clv
		bcc sdc_opendir_error

		tax												; transfer the directory file descriptor into X
		stx sdc_filedescriptor
		ldy #>sdc_transferbuffer						; set Y to the MSB of the transfer area

sdcod1	lda #$14										; hyppo_readdir - read the directory entry
		sta $d640
		clv
		bcc sdcod2

		phx
		phy
sdc_processdirentryptr		
		jsr $babe										; call function that handles the retrieved filename
		ply
		plx
		bra sdcod1

sdcod2	cmp #$85										; if the error code in A is $85 we have reached the end of the directory otherwise there’s been an error
		bne sdc_opendir_error
		lda #$16										; close the directory file descriptor in X
		sta $d640
		clv
		bcc sdc_opendir_error
		rts

sdc_opendir_error
		lda #$38
		sta $d640
		clv

:		lda #$06
		sta $d020
		lda #$87
		sta $d020
		jmp :-

; ----------------------------------------------------------------------------------------------------

sdc_chdir

		ldy #>sdc_transferbuffer						; set the hyppo filename from transferbuffer
		lda #$2e
		sta $d640
		clv
		bcc :+
		lda #$34										; find the FAT dir entry
		sta $d640
		clv
		bcc :+
		lda #$0c										; chdir into the directory
		sta $d640
		clv
		rts

:		;inc $d020
		;jmp :-
		rts

; ----------------------------------------------------------------------------------------------------

sdc_loadfile

		ldy #>sdc_transferbuffer						; set the hyppo filename from transferbuffer
		lda #$2e
		sta $d640
		clv
		bcc :+

		ldx #<$00020000
		ldy #>$00020000
		ldz #($00020000 & $ff0000) >> 16

		lda #$36										; $36 for chip RAM at $00ZZYYXX
		sta $d640										; Mega65.HTRAP00
		clv												; Wasted instruction slot required following hyper trap instruction
		bcc :+

		rts

:		;inc $d020
		;jmp :-
		rts


; ----------------------------------------------------------------------------------------------------

sdc_openfile

		lda #$00
		sta sdc_bytecounterlo
		sta sdc_bytecounterhi

		lda #$ff
		sta sdc_sectorcount

		ldy #>sdc_transferbuffer						; set the hyppo filename from transferbuffer
		lda #$2e
		sta $d640
		clv
		bcc :+

		lda #$34
		sta $D640
		clv
		bcc sdc_openfile_error

		lda #$18
		sta $d640
		clv
		bcc sdc_openfile_error
		rts

sdc_openfile_error
:		lda #$04
		sta $d020
		lda #$05
		sta $d020
		jmp :-				

; ----------------------------------------------------------------------------------------------------

sdc_closefile

		ldx sdc_filedescriptor
		lda #$20										; Preconditions: The file descriptor given in the X register was opened using hyppo_openfile.
		sta $d640
		clv
		rts

; ----------------------------------------------------------------------------------------------------

sdc_readsector

		inc sdc_sectorcount
														; assume the file is already open.		
		lda $d030										; unmap the colour RAM from $dc00 because that will prevent us from mapping in the sector buffer
		pha
		and #%11111110
		sta $d030

sdc_readsector_loop

		lda #$1a										; read the next sector
		sta $d640
		clv
		bcc sdc_readsector_error
		
		lda #$81										; map the sector buffer to $de00
		sta $d680

		ldx #$00										; copy sector to sectorbuffer
:		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		inx
		bne :-

		ldx #$00
:		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		bne :-
		
		lda #$82										; unmap the sector buffer from $de00
		sta $d680

		pla												; map the colour RAM at $dc00 if it was previously mapped
		sta $d030
		rts

sdc_readsector_error

		cmp #$ff										; if the error code in A is $ff we have reached the end of the file otherwise there’s been an error
		bne sdc_readsector_fatalerror

		pla												; map the colour RAM at $dc00 if it was previously mapped
		sta $d030
		rts

sdc_readsector_fatalerror

:		lda #$0b
		sta $d020
		lda #$0c
		sta $d020
		jmp :-

; ----------------------------------------------------------------------------------------------------

sdc_cwd

		rts

; ----------------------------------------------------------------------------------------------------

sdc_d81attach0

		lda #$00
		sta sdc_transferbuffer,y

		ldy #>sdc_transferbuffer						; set the hyppo filename from transferbuffer
		lda #$2e
		sta $d640
		clv
		bcc sdc_d81attach0_error

		lda #$40										; Attach the disk image
		sta $d640
		clv
		bcc sdc_d81attach0_error
		rts

sdc_d81attach0_error

:		lda #$02
		sta $d020
		lda #$03
		sta $d020
		jmp :-

; ----------------------------------------------------------------------------------------------------

sdc_getbyte

		; LV TODO - this can be optimized a LOT

		lda sdc_bytecounterhi
		cmp #$02
		bne :+

		jsr sdc_readsector

		lda #$00
		sta sdc_bytecounterlo
		sta sdc_bytecounterhi

:		lda #<sdc_sectorbuffer
		sta sdcgb+1
		lda #>sdc_sectorbuffer
		sta sdcgb+2

		clc
		lda sdcgb+1
		adc sdc_bytecounterlo
		sta sdcgb+1
		lda sdcgb+2
		adc sdc_bytecounterhi
		sta sdcgb+2

sdcgb	lda $babe

		pha
		clc
		lda sdc_bytecounterlo
		adc #$01
		sta sdc_bytecounterlo
		lda sdc_bytecounterhi
		adc #$00
		sta sdc_bytecounterhi
		pla

		clc

		rts

; ----------------------------------------------------------------------------------------------------
