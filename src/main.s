; ----------------------------------------------------------------------------------------------------

.define screen					$e000	; size = 80*50*2 = $1f40

.define uipal					$c700	; size = $0300
.define spritepal				$ca00
.define sprptrs					$cd00
.define sprites					$ce00
.define kbsprites				$cf00
.define jpgchars				$f000	; 40 * 64 = $0a00s
.define emptychar				$ff80	; size = 64

.define uichars					$10000	; $10000 - $14000     size = $4000
.define glchars					$14000	; $14000 - $1d000     size = $9000

.define jpgdata					$20000

; ----------------------------------------------------------------------------------------------------

.segment "MAIN"

entry_main

		sei

		lda #$35
		sta $01

		lda #%10000000									; Clear bit 7 - HOTREG
		trb $d05d

		lda #$00										; unmap
		tax
		tay
		taz
		map
		eom

		lda #$47										; enable C65GS/VIC-IV IO registers
		sta $d02f
		lda #$53
		sta $d02f
		eom

		lda #%10000000									; force PAL mode, because I can't be bothered with fixing it for NTSC
		trb $d06f										; clear bit 7 for PAL ; trb $d06f 
		;tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

		lda #$41										; enable 40MHz
		sta $00

		lda #$70										; Disable C65 rom protection using hypervisor trap (see mega65 manual)
		sta $d640
		eom

		lda #%11111000									; unmap c65 roms $d030 by clearing bits 3-7
		trb $d030
		lda #%00000100									; PAL - Use PALETTE ROM (0) or RAM (1) entries for colours 0 - 15
		tsb $d030

		lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		sta $d054

		lda #%10100000									; CLEAR bit7=40 column, bit5=Enable extended attributes and 8 bit colour entries
		trb $d031

		lda #80											; set to 80 for etherload
		sta $d05e

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		ldx #$00
		lda #$00
:		sta emptychar,x
		inx
		cpx #64
		bne :-

		ldx #$00
:		lda #<(emptychar/64)
		sta screen+0*$0100+0,x
		sta screen+1*$0100+0,x
		sta screen+2*$0100+0,x
		sta screen+3*$0100+0,x
		sta screen+4*$0100+0,x
		sta screen+5*$0100+0,x
		sta screen+6*$0100+0,x
		sta screen+7*$0100+0,x
		lda #>(emptychar/64)
		sta screen+0*$0100+1,x
		sta screen+1*$0100+1,x
		sta screen+2*$0100+1,x
		sta screen+3*$0100+1,x
		sta screen+4*$0100+1,x
		sta screen+5*$0100+1,x
		sta screen+6*$0100+1,x
		sta screen+7*$0100+1,x
		inx
		inx
		bne :-

		DMA_RUN_JOB clearcolorramjob

		lda #<screen									; set pointer to screen ram
		sta $d060
		lda #>screen
		sta $d061
		lda #(screen & $ff0000) >> 16
		sta $d062
		lda #$00
		sta $d063

		lda #<$0800										; set (offset!) pointer to colour ram
		sta $d064
		lda #>$0800
		sta $d065

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$00
		sta $d012
		lda #<fastload_irq_handler
		sta $fffe
		lda #>fastload_irq_handler
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli

.if useetherload = 0

		jsr fl_init
		jsr fl_waiting
		FLOPPY_FAST_LOAD uichars,			$30, $30
		FLOPPY_FAST_LOAD glchars,			$30, $31
		FLOPPY_FAST_LOAD uipal,				$30, $32
		FLOPPY_FAST_LOAD sprites,			$30, $33
		FLOPPY_FAST_LOAD kbsprites,			$30, $34
		FLOPPY_FAST_LOAD spritepal,			$30, $35
		FLOPPY_FAST_LOAD $0400,				$30, $36		; jpg_negmlo
		FLOPPY_FAST_LOAD $0a00,				$30, $37
		FLOPPY_FAST_LOAD $8100,				$30, $38
		jsr fl_exit

.endif		

main_restart
		sei

		lda #$35
		sta $01

		lda #$02
		sta $d020
		lda #$10
		sta $d021

		jsr mouse_init									; initialise drivers
		jsr ui_init										; initialise UI
		jsr ui_setup

		jsr keyboard_update

		lda #<fa1filebox
		sta uikeyboard_focuselement+0
		lda #>fa1filebox
		sta uikeyboard_focuselement+1

		lda filebox1_stored_startpos+0
		sta fa1scrollbar_data+2
		lda filebox1_stored_startpos+1
		sta fa1scrollbar_data+3

		lda filebox1_stored_selection+0
		sta fa1scrollbar_data+4
		lda filebox1_stored_selection+1
		sta fa1scrollbar_data+5

		UICORE_CALLELEMENTFUNCTION fa1filebox, uifilebox_draw

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a
		sta main_event
		
		lda #$ff										; setup IRQ interrupt
		sta $d012
		lda #<irq1
		sta $fffe
		lda #>irq1
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli
		
loop

		lda main_event
		cmp #$01
		beq load_image
		cmp #$03
		beq main_restart
		jmp loop

load_image
		jsr sdc_openfile
		jsr sdc_readsector

		jsr jpg_process

		jsr sdc_closefile

		lda #2
		sta main_event
		jmp loop

main_event
		.byte 0

; ----------------------------------------------------------------------------------------------------

irq1
		php
		pha
		phx
		phy
		phz

		jsr ui_update
		jsr ui_user_update

.if megabuild = 1
		lda #$ff
.else
		lda #$00
.endif
		sta $d012

		lda main_event
		cmp #$01
		beq set_jpg_load_irq
		bra continueirq

set_jpg_load_irq
		lda #<jpg_load_irq
		sta $fffe
		lda #>jpg_load_irq
		sta $ffff
		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

continueirq
		lda #<irq1
		sta $fffe
		lda #>irq1
		sta $ffff
		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------