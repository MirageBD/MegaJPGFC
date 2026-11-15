.define jpg_render_rptr		$b0
.define jpg_render_gptr		$b4
.define jpg_render_bptr		$b8
.define jpg_render_src		$bc
.define jpg_render_add64	$c0
.define jpg_render_add256	$c4
.define jpg_render_addgreen	$c8
.define jpg_render_addblue	$cc

.macro JPGR_QCOPY source, destination
		ldq source
		stq destination
.endmacro

.macro JPGR_QADD pointer, increment
		ldq pointer
		cld
		clc
		adcq increment
		stq pointer
.endmacro

.macro JPGR_REVERSE_NIBBLE
		asl a
		adc #$80
		rol a
		asl a
		adc #$80
		rol a
.endmacro

screencolumn	.byte 0
screenrow		.byte 0

jpg_fcblock
		.repeat 8
			.byte 0, 1, 2, 3, 4, 5, 6, 7
		.endrepeat

jpg_misccounter
		.byte 0
jpg_render_rowcount
		.byte 0

jpg_rowchars
		.byte 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,1,2,3,4,5,6,7,8,9,10

jpg_rendinit

		lda #$00
		sta $d015

;		lda $d070										; select mapped bank with the upper 2 bits of $d070
;		and #%00111111
;		ora #%01000000									; select palette 1
;		sta $d070

;		ldx #$00										; set bitmap palette
;:		lda #$00
;		sta $d100,x
;		sta $d200,x
;		sta $d300,x
;		inx
;		bne :-

		lda $d070
		and #%11111100									; bank to alternative palette
		ora #%00000010
		sta $d070

		DMA_RUN_JOB jpgrender_clearbitmapjob
		DMA_RUN_JOB jpgrender_clearcolorramjob

		lda #<.loword(SAFE_COLOR_RAM_PLUS_ONE)
		sta uidraw_colptr+0
		lda #>.loword(SAFE_COLOR_RAM_PLUS_ONE)
		sta uidraw_colptr+1
		lda #<.hiword(SAFE_COLOR_RAM_PLUS_ONE)
		sta uidraw_colptr+2
		lda #>.hiword(SAFE_COLOR_RAM_PLUS_ONE)
		sta uidraw_colptr+3

		lda #0
		sta jpg_misccounter

jpg_fillsetaltpalbits

		ldz #30*2
		lda #%01101111
:		sta [uidraw_colptr],z
		inz
		inz
		cpz #40*2
		bne :-

		clc
		lda uidraw_colptr+0
		adc #80
		sta uidraw_colptr+0
		lda uidraw_colptr+1
		adc #0
		sta uidraw_colptr+1
		lda uidraw_colptr+2
		adc #0
		sta uidraw_colptr+2
		lda uidraw_colptr+3
		adc #0
		sta uidraw_colptr+3

		inc jpg_misccounter
		lda jpg_misccounter
		cmp #25
		bne jpg_fillsetaltpalbits


		; fill full colour char pattern ($f000)

		lda #<jpgchars
		sta jpgri1+1
		lda #>jpgchars
		sta jpgri1+2

		lda #$00
		sta jpg_misccounter

		ldx #$00
		ldy #$00
:		tya
		adc jpg_fcblock,x
jpgri1	sta jpgchars,x
		inx
		cpx #64
		bne :-
		clc
		lda jpgri1+1
		adc #64
		sta jpgri1+1
		lda jpgri1+2
		adc #0
		sta jpgri1+2
		clc
		tya
		adc #$08
		tay
		inc jpg_misccounter
		lda jpg_misccounter
		cmp #42
		bne :-

		; fill screen ($e000)

		lda #$00
		sta screencolumn
		sta screenrow

		lda #<(screen+0)
		sta put0+1
		lda #>(screen+0)
		sta put0+2
		lda #<(screen+1)
		sta put1+1
		lda #>(screen+1)
		sta put1+2

		; jpgchars = $f000
		; jpgchars/64 = $0800

putstart
		ldx screencolumn
		clc
		lda jpg_rowchars,x								; char to plot
		adc #<(jpgchars/64)
put0	sta screen+0									; plot left of 2 chars

		lda #>(jpgchars/64)
put1	sta screen+1									; plot right of 2 chars

		clc												; add 2 to screenpos low
		lda put0+1
		adc #02
		sta put0+1
		lda put0+2
		adc #0
		sta put0+2

		clc												; add 2 to screenpos high
		lda put1+1
		adc #02
		sta put1+1
		lda put1+2
		adc #0
		sta put1+2

		inc screencolumn								; increase screen column until 40
		lda screencolumn
		cmp #jpg_bufwidth
		bne putstart

		lda #0											; reset screencolumn to 0, increase row until 25
		sta screencolumn
		inc screenrow
		lda screenrow
		cmp #25
		beq endscreenplot

		jmp putstart

endscreenplot

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #%00100000									; set H320, V200, ATTR
		sta $d031

		lda #$00
		sta $d016

/*
		; bigger screen
		lda #$12										; Y Position Where Character Display Starts ($D04E LSB, 0–3 of $D04F MSB)
		sta $d04e
		lda #35											; set number of rows
		sta $d07b

		lda #$00										; reposition start of top border to what's juuuuust visible on my monitor
		sta $d048
		lda #$50										; reposition start of bottom border to what's juuuuust visible on my monitor
		sta $d04a
		lda #$02
		sta $d04b

		lda #$18										; X position of the left side border edge
		sta $d05c

		lda #$27										; X Position Where Character Display Starts ($D04C LSB, bits 0 – 3 of $D04D MSB)
		sta $d04c
		lda #$00
		sta $d04d
		;end bigger screen
*/

		lda #$50		; set TEXTXPOS to same as SDBDRWDLSB
		lda $d04c
		lda #$42
		sta $d05c

		lda #$01
		sta $d05b										; Set display to V200
		lda #25
		sta $d07b										; Display 25 rows of text

		; initialize multiply units

		lda #$00
		sta $d770
		sta $d771
		sta $d772
		sta $d773

		lda #$00
		sta $d774
		sta $d775
		sta $d776
		sta $d777

		lda #<.loword(jpgdata)
		sta uidraw_scrptr+0
		lda #>.loword(jpgdata)
		sta uidraw_scrptr+1
		lda #<.hiword(jpgdata)
		sta uidraw_scrptr+2
		lda #>.hiword(jpgdata)
		sta uidraw_scrptr+3

		QSTORE32 $0040, jpg_render_add64
		QSTORE32 $0100, jpg_render_add256
		QSTORE32 jpg_channelbufsize, jpg_render_addgreen
		QSTORE32 (2*jpg_channelbufsize), jpg_render_addblue

        rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_render
		sta jpg_render_rptr+0
		sty jpg_render_rptr+1
		lda #<.hiword(jpg_imgbuf)
		sta jpg_render_rptr+2
		lda #>.hiword(jpg_imgbuf)
		sta jpg_render_rptr+3

		JPGR_QCOPY jpg_render_rptr, jpg_render_gptr
		JPGR_QCOPY jpg_render_rptr, jpg_render_bptr
		JPGR_QADD jpg_render_gptr, jpg_render_addgreen
		JPGR_QADD jpg_render_bptr, jpg_render_addblue

		lda #$08
		sta jpg_render_rowcount
jpg_render_rows
		JPGR_QCOPY jpg_render_rptr, jpg_render_src
		jsr jpg_render_copy320
		JPGR_QCOPY jpg_render_src, jpg_render_rptr

		JPGR_QCOPY jpg_render_gptr, jpg_render_src
		jsr jpg_render_copy320
		JPGR_QCOPY jpg_render_src, jpg_render_gptr

		JPGR_QCOPY jpg_render_bptr, jpg_render_src
		jsr jpg_render_copy320
		JPGR_QCOPY jpg_render_src, jpg_render_bptr
		dec jpg_render_rowcount
		bne jpg_render_rows
		rts

; Copy 320 bytes from the long source pointer into the next RGB output plane.
jpg_render_copy320
		ldz #$00
jpg_render_copy256
:		lda [jpg_render_src],z
		JPGR_REVERSE_NIBBLE
		sta [uidraw_scrptr],z
		inz
		bne jpg_render_copy256
		JPGR_QADD jpg_render_src, jpg_render_add256
		JPGR_QADD uidraw_scrptr, jpg_render_add256
		ldz #$00
		ldx #$40
jpg_render_copy64
:		lda [jpg_render_src],z
		JPGR_REVERSE_NIBBLE
		sta [uidraw_scrptr],z
		inz
		dex
		bne jpg_render_copy64
		JPGR_QADD jpg_render_src, jpg_render_add64
		JPGR_QADD uidraw_scrptr, jpg_render_add64
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_load_irq

		php
		pha
		phx
		phy
		phz

;		ldx #$00
;:		inc $d020
;		inx
;		bne :-

		jsr mouse_update
		jsr keyboard_update

		lda main_event
		cmp #$02
		beq set_jpg_render_irq

		lda palntscscreenstart
		sta $d012

		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

set_jpg_render_irq
		lda #<jpg_render_irq
		sta $fffe
		lda #>jpg_render_irq
		sta $ffff
		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_render_irq

		php
		pha
		phx
		phy
		phz

		lda #<.loword(jpgdata + 0*320)
		sta jpgrucr+0
		lda #>.loword(jpgdata + 0*320)
		sta jpgrucr+1
		lda #<.hiword(jpgdata + 0*320)
		sta jpgrucr+2

		lda #<.loword(jpgdata + 1*320)
		sta jpgrucg+0
		lda #>.loword(jpgdata + 1*320)
		sta jpgrucg+1
		lda #<.hiword(jpgdata + 1*320)
		sta jpgrucg+2

		lda #<.loword(jpgdata + 2*320)
		sta jpgrucb+0
		lda #>.loword(jpgdata + 2*320)
		sta jpgrucb+1
		lda #<.hiword(jpgdata + 2*320)
		sta jpgrucb+2

		lda #<.loword(jpgdata + 0*320 + 240)
		sta jpgrucrr+0
		lda #>.loword(jpgdata + 0*320 + 240)
		sta jpgrucrr+1
		lda #<.hiword(jpgdata + 0*320 + 240)
		sta jpgrucrr+2

		lda #<.loword(jpgdata + 1*320 + 240)
		sta jpgrucgr+0
		lda #>.loword(jpgdata + 1*320 + 240)
		sta jpgrucgr+1
		lda #<.hiword(jpgdata + 1*320 + 240)
		sta jpgrucgr+2

		lda #<.loword(jpgdata + 2*320 + 240)
		sta jpgrucbr+0
		lda #>.loword(jpgdata + 2*320 + 240)
		sta jpgrucbr+1
		lda #<.hiword(jpgdata + 2*320 + 240)
		sta jpgrucbr+2

		ldy palntscscreenstart
		iny
:		cpy $d012
		bne :-

		lda #$00
		sta $d020

		ldx #$00
jpg_render_irq_loop

		lda $d070										; BANK IN BITMAP PALETTE - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
jpgrucr		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d108										; dst
			.byte (($d108 >> 16) & $0f) | %10000000			; dst bank and flags ; turn on bit 7 for I/O when writing to $d100
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
jpgrucg		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d208										; dst
			.byte (($d208 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 240										; count
jpgrucb		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d308										; dst
			.byte (($d308 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

		lda $d070										; BANK IN BITMAP PALETTE - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%10000000
		sta $d070

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
jpgrucrr	.word $0000										; src
			.byte $00										; src bank and flags
			.word $d108										; dst
			.byte (($d108 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
jpgrucgr	.word $0000										; src
			.byte $00										; src bank and flags
			.word $d208										; dst
			.byte (($d208 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 80										; count
jpgrucbr	.word $0000										; src
			.byte $00										; src bank and flags
			.word $d308										; dst
			.byte (($d308 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

		clc
		lda jpgrucr+0									; add 3*320 to DMA copy
		adc #<.loword(3*320)
		sta jpgrucr+0
		lda jpgrucr+1
		adc #>.loword(3*320)
		sta jpgrucr+1
		lda jpgrucr+2
		adc #<.hiword(3*320)
		sta jpgrucr+2

		clc
		lda jpgrucg+0									; add 3*320 to DMA copy
		adc #<.loword(3*320)
		sta jpgrucg+0
		lda jpgrucg+1
		adc #>.loword(3*320)
		sta jpgrucg+1
		lda jpgrucg+2
		adc #<.hiword(3*320)
		sta jpgrucg+2

		clc
		lda jpgrucb+0									; add 3*320 to DMA copy
		adc #<.loword(3*320)
		sta jpgrucb+0
		lda jpgrucb+1
		adc #>.loword(3*320)
		sta jpgrucb+1
		lda jpgrucb+2
		adc #<.hiword(3*320)
		sta jpgrucb+2

		clc
		lda jpgrucrr+0									; add 3*320 to DMA copy
		adc #<.loword(3*320)
		sta jpgrucrr+0
		lda jpgrucrr+1
		adc #>.loword(3*320)
		sta jpgrucrr+1
		lda jpgrucrr+2
		adc #<.hiword(3*320)
		sta jpgrucrr+2

		clc
		lda jpgrucgr+0									; add 3*320 to DMA copy
		adc #<.loword(3*320)
		sta jpgrucgr+0
		lda jpgrucgr+1
		adc #>.loword(3*320)
		sta jpgrucgr+1
		lda jpgrucgr+2
		adc #<.hiword(3*320)
		sta jpgrucgr+2

		clc
		lda jpgrucbr+0									; add 3*320 to DMA copy
		adc #<.loword(3*320)
		sta jpgrucbr+0
		lda jpgrucbr+1
		adc #>.loword(3*320)
		sta jpgrucbr+1
		lda jpgrucbr+2
		adc #<.hiword(3*320)
		sta jpgrucbr+2

		iny
:		cpy $d012
		bne :-

		inx
		cpx #200
		lbne jpg_render_irq_loop

		jsr mouse_update
		jsr keyboard_update

		lda mouse_released
		beq :+

		lda #$03										; trigger main restart event
		sta main_event
		bra :++

:		lda keyboard_shouldsendreleaseevent
		beq :+

		lda #$03
		sta main_event

:		lda #$32
		sta palntscscreenstart

		bit $d06f
		bpl palirq

ntscirq	lda #$18
		sta palntscscreenstart
palirq	lda palntscscreenstart
		sta $d012

		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

palntscscreenstart
		.byte $32

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (SAFE_COLOR_RAM) >> 20				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*50										; Count LSB + Count MSB

				.byte %00000000										; this is normally the source addres, but contains the fill value now
				.byte 0
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM) & $ffff					; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000									; Command MSB

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*50										; Count LSB + Count MSB

				.word $000f										; ff = red = transparency. this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM+1) & $ffff				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000								; Command MSB

				.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_clearbitmapjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (jpgdata) >> 20						; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 320*200									; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (jpgdata) & $ffff							; Destination Address LSB + Destination Address MSB
				.byte (((jpgdata) >> 16) & $0f)					; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				.byte %00000000									; Command MSB

				.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------
