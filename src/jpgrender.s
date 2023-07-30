screencolumn	.byte 0
screenrow		.byte 0

jpg_fcblock
		.repeat 8
			.byte 0, 1, 2, 3, 4, 5, 6, 7
		.endrepeat

jpg_misccounter
		.byte 0

jpg_rowchars
		.byte 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,1,2,3,4,5,6,7,8,9,10

jpg_rendinit

		lda #$00
		sta $d015

		; WHY THE HELL DO I NEED TO FILL 2 PALETTES HERE???

		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%10000000									; select palette 02
		sta $d070

		ldx #$00										; set bitmap palette
:		lda #$00
		sta $d100,x
		sta $d200,x
		sta $d300,x
		inx
		bne :-

		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%11000000									; select palette 03
		sta $d070

		ldx #$00										; set bitmap palette
:		lda #$00
		sta $d100,x
		sta $d200,x
		sta $d300,x
		inx
		bne :-








		lda $d070
		and #%11111100									; set alt palette to 2
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

		lda #$50		; set TEXTXPOS to same as SDBDRWDLSB
		lda $d04c
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

        rts

; ----------------------------------------------------------------------------------------------------------------------------------------

reversenibble	; 	(byte)((b >> 4) | ((b & 15) << 4));

	pha
	lsr
	lsr
	lsr
	lsr
	sta highnibble
	pla
	asl
	asl
	asl
	asl
	ora highnibble
	rts

highnibble
.byte 0

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_render

		sta jpgrnd_red+1
		sta jpgrnd_redright+1
		sta jpgrnd_green+1
		sta jpgrnd_greenright+1
		sta jpgrnd_blue+1
		sta jpgrnd_blueright+1
		tya
		clc
		adc #>(0*jpg_channelbufsize)					; $8400 + 0 * $1400
		sta jpgrnd_red+2
		sta jpgrnd_redright+2
		clc
		adc #>(1*jpg_channelbufsize)					; $8400 + 1 * $1400
		sta jpgrnd_green+2
		sta jpgrnd_greenright+2
		clc
		adc #>(1*jpg_channelbufsize)					; $8400 + 2 * $1400
		sta jpgrnd_blue+2
		sta jpgrnd_blueright+2

		inc jpgrnd_redright+2
		inc jpgrnd_greenright+2
		inc jpgrnd_blueright+2

		ldy #$00

jpgrend_getrgb

		ldx #$00
		ldz #$00
jpgrnd_red
:		lda $babe,x										; copy 256 red colours
		jsr reversenibble
		sta [uidraw_scrptr],z
		inx
		inz
		cpz #$00
		bne :-

		clc
		lda uidraw_scrptr+0
		adc #$00
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #$01
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2

		ldx #$00
		ldz #$00
jpgrnd_redright
:		lda $babe,x										; copy 64 red colours
		jsr reversenibble
		sta [uidraw_scrptr],z
		inx
		inz
		cpz #64
		bne :-

		clc
		lda uidraw_scrptr+0
		adc #$40
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #$00
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2

		ldx #$00
		ldz #$00
jpgrnd_green
:		lda $babe,x
		jsr reversenibble
		sta [uidraw_scrptr],z
		inx
		inz
		cpz #$00
		bne :-

		clc
		lda uidraw_scrptr+0
		adc #$00
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #$01
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2

		ldx #$00
		ldz #$00
jpgrnd_greenright
:		lda $babe,x										; copy 64 green colours
		jsr reversenibble
		sta [uidraw_scrptr],z
		inx
		inz
		cpz #64
		bne :-

		clc
		lda uidraw_scrptr+0
		adc #$40
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #$00
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2

		ldx #$00
		ldz #$00
jpgrnd_blue
:		lda $babe,x
		jsr reversenibble
		sta [uidraw_scrptr],z
		inx
		inz
		cpz #$00
		bne :-

		clc
		lda uidraw_scrptr+0
		adc #$00
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #$01
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2

		ldx #$00
		ldz #$00
jpgrnd_blueright
:		lda $babe,x										; copy 64 blue colours
		jsr reversenibble
		sta [uidraw_scrptr],z
		inx
		inz
		cpz #64
		bne :-

		clc
		lda uidraw_scrptr+0
		adc #$40
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #$00
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2


		clc
		lda jpgrnd_red+1
		adc #<(jpg_bufwidth*8)							; add 8*bufwidth to get to next row in jpg data
		sta jpgrnd_red+1
		lda jpgrnd_red+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_red+2

		clc
		lda jpgrnd_green+1
		adc #<(jpg_bufwidth*8)
		sta jpgrnd_green+1
		lda jpgrnd_green+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_green+2

		clc
		lda jpgrnd_blue+1
		adc #<(jpg_bufwidth*8)
		sta jpgrnd_blue+1
		lda jpgrnd_blue+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_blue+2

		clc
		lda jpgrnd_redright+1
		adc #<(jpg_bufwidth*8)
		sta jpgrnd_redright+1
		lda jpgrnd_redright+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_redright+2

		clc
		lda jpgrnd_greenright+1
		adc #<(jpg_bufwidth*8)
		sta jpgrnd_greenright+1
		lda jpgrnd_greenright+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_greenright+2

		clc
		lda jpgrnd_blueright+1
		adc #<(jpg_bufwidth*8)
		sta jpgrnd_blueright+1
		lda jpgrnd_blueright+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_blueright+2

		iny
		cpy #8
		beq :+
		
		jmp jpgrend_getrgb

:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_load_irq

		php
		pha
		phx
		phy
		phz

		ldx #$00
:		inc $d020
		inx
		bne :-

		jsr mouse_update
		jsr keyboard_update

		lda main_event
		cmp #$02
		beq set_jpg_render_irq

		lda #$33
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

		lda #$00
		sta $d020

		ldx #$00
jpg_render_irq_loop		
		lda $d070										; BANK IN BITMAP PALETTE - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070
		DMA_RUN_JOB jpgrender_updatecoloursred
		DMA_RUN_JOB jpgrender_updatecoloursgreen
		DMA_RUN_JOB jpgrender_updatecoloursblue
		lda $d070										; BANK IN BITMAP PALETTE - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%10000000
		sta $d070
		DMA_RUN_JOB jpgrender_updatecoloursredright
		DMA_RUN_JOB jpgrender_updatecoloursgreenright
		DMA_RUN_JOB jpgrender_updatecoloursblueright

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

		lda $d012
		clc
		adc #$01
:		cmp $d012
		bne :-

		inx
		cpx #200
		beq :+
		jmp jpg_render_irq_loop

:
		lda #$80
		sta $d020

		jsr mouse_update
		jsr keyboard_update

		lda mouse_released
		beq :+

		lda #$03										; trigger main restart event
		sta main_event
		bra :++

:
		lda keyboard_shouldsendreleaseevent
		beq :+

		lda #$03
		sta main_event

:		
		lda #$33
		sta $d012

		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

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
				;.byte %00000000									; Command MSB

				.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecoloursred
			.byte $0a, $80, ($20000 >> 20), $81, ($d108 >> 20)
			.byte $82, 0, $83, 1, $84, 0, $85, 1, 0, 0
			.word 240
jpgrucr		.byte 0, 0, 0
			.word $d108 & $ffff
			.byte (($d108 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100
			.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecoloursgreen
			.byte $0a, $80, ($20000 >> 20), $81, ($d208 >> 20)
			.byte $82, 0, $83, 1, $84, 0, $85, 1, 0, 0
			.word 240
jpgrucg		.byte 0, 0, 0
			.word $d208 & $ffff
			.byte (($d208 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100
			.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecoloursblue
			.byte $0a, $80, ($20000 >> 20), $81, ($d308 >> 20)
			.byte $82, 0, $83, 1, $84, 0, $85, 1, 0, 0
			.word 240
jpgrucb		.byte 0, 0, 0
			.word $d308 & $ffff
			.byte (($d308 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100
			.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecoloursredright
			.byte $0a, $80, ($20000 >> 20), $81, ($d108 >> 20)
			.byte $82, 0, $83, 1, $84, 0, $85, 1, 0, 0
			.word 80
jpgrucrr	.byte 0, 0, 0
			.word $d108 & $ffff
			.byte (($d108 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100
			.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecoloursgreenright
			.byte $0a, $80, ($20000 >> 20), $81, ($d208 >> 20)
			.byte $82, 0, $83, 1, $84, 0, $85, 1, 0, 0
			.word 80
jpgrucgr	.byte 0, 0, 0
			.word $d208 & $ffff
			.byte (($d208 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100
			.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecoloursblueright
			.byte $0a, $80, ($20000 >> 20), $81, ($d308 >> 20)
			.byte $82, 0, $83, 1, $84, 0, $85, 1, 0, 0
			.word 80
jpgrucbr	.byte 0, 0, 0
			.word $d308 & $ffff
			.byte (($d308 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100
			.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------
