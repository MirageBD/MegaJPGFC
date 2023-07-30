screencolumn	.byte 0
screenrow		.byte 0

reversenibble_tmp	.byte 0
reversenibble_tmp2	.byte 0

jpg_fcblock
		.repeat 8
			.byte 0, 1, 2, 3, 4, 5, 6, 7
		.endrepeat

jpg_rend_foo
		.repeat 42
			.byte 0, 51, 102, 153, 204, 255
		.endrepeat

jpg_misccounter
		.byte 0

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

jpg_fillscreenright

		ldz #32*2
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
		bne jpg_fillscreenright


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
		cmp #40
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

		ldx #<(jpgchars/64)								; char to plot
		ldy #>(jpgchars/64)

put0	stx screen+0									; plot left of 2 chars
put1	sty screen+1									; plot right of 2 chars

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

		clc												; add 1 to char to plot
		txa
		adc #$01
		tax
		tya
		adc #$00
		tay

		inc screencolumn								; increase screen column until 40
		lda screencolumn
		cmp #jpg_bufwidth
		bne put0

		ldx #<(jpgchars/64)								; char to plot
		ldy #>(jpgchars/64)

		lda #0											; reset screencolumn to 0, increase row until 25
		sta screencolumn
		inc screenrow
		lda screenrow
		cmp #25
		beq endscreenplot

		jmp put0

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


jpg_rend_red		.byte 0
jpg_rend_green		.byte 0
jpg_rend_blue		.byte 0

jpg_rend_column		.byte 0

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

/*
		lda #$00
		sta jpg_rend_column

jpgrnd_column_loop
		ldy #$00
		ldz #$00

jpgrnd_scan_loop
		ldx #$00

jpgrend_getrgb

		phy

jpgrnd_red
		lda $babe,x
		tay
		lda jpg_snaptable,y
		sta $d770										; math multiplier A register
		lda #36
		sta $d774
		lda $d778+0
		sta jpg_rend_red

jpgrnd_green
		lda $babe,x
		tay
		lda jpg_snaptable,y
		sta $d770
		lda #6
		sta $d774
		lda $d778+0
		sta jpg_rend_green

jpgrnd_blue
		lda $babe,x
		tay
		lda jpg_snaptable,y
		clc
		adc jpg_rend_green
		adc jpg_rend_red
		adc #$27										; add 27 to get to front of 216 web palette

		ply

		sta [uidraw_scrptr],z

		inz
		inx
		cpx #8											; add 8 to get to next row in this char
		bne jpgrend_getrgb

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

		iny
		cpy #8
		bne jpgrnd_scan_loop

		clc											; add 64 to get to the next character
		lda uidraw_scrptr+0
		adc #64
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #$00
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2
		lda uidraw_scrptr+3
		adc #$00
		sta uidraw_scrptr+3

		sec
		lda jpgrnd_red+1
		sbc #<(jpg_bufwidth*8*8 - 8)				; subtract 'enough' to get to the next char in the jpg data
		sta jpgrnd_red+1
		lda jpgrnd_red+2
		sbc #>(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_red+2

		sec
		lda jpgrnd_green+1
		sbc #<(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_green+1
		lda jpgrnd_green+2
		sbc #>(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_green+2

		sec
		lda jpgrnd_blue+1
		sbc #<(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_blue+1
		lda jpgrnd_blue+2
		sbc #>(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_blue+2

		inc jpg_rend_column
		lda jpg_rend_column
		cmp #jpg_bufwidth
		beq :+
		jmp jpgrnd_column_loop

:		rts
*/

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_bayer_matrix
/*
		.byte  37, 139,  63, 165,  37, 139,  63, 165
		.byte 190,  88, 216, 114, 190,  88, 216, 114
		.byte  63, 165,  37, 139,  63, 165,  37, 139
		.byte 216, 114, 190,  88, 216, 114, 190,  88
		.byte  37, 139,  63, 165,  37, 139,  63, 165
		.byte 190,  88, 216, 114, 190,  88, 216, 114
		.byte  63, 165,  37, 139,  63, 165,  37, 139
		.byte 216, 114, 190,  88, 216, 114, 190,  88
*/
		.byte 128, 237, 156, 255, 128, 237, 156, 255
		.byte 255, 182, 255, 210, 255, 182, 255, 210
		.byte 156, 255, 128, 237, 156, 255, 128, 237
		.byte 255, 210, 255, 182, 255, 210, 255, 182
		.byte 128, 237, 156, 255, 128, 237, 156, 255
		.byte 255, 182, 255, 210, 255, 182, 255, 210
		.byte 156, 255, 128, 237, 156, 255, 128, 237
		.byte 255, 210, 255, 182, 255, 210, 255, 182

jpg_snaptable
		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		.byte 0,0,0,0,0,0,0,0,0,0
		.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
		.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
		.byte 1,1,1,1,1,1,1,1,1,1
		.byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
		.byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
		.byte 2,2,2,2,2,2,2,2,2,2
		.byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
		.byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
		.byte 3,3,3,3,3,3,3,3,3,3
		.byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
		.byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
		.byte 4,4,4,4,4,4,4,4,4,4
		.byte 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
		.byte 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
		.byte 5,5,5,5,5,5,5,5,5,5

		.byte 5,5,5,5

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_render_irq

		php
		pha
		phx
		phy
		phz

		lda #0											; set up DMA copy to start at $20000
		sta jpgruc2+0
		lda #0
		sta jpgruc2+1
		lda #2
		sta jpgruc3
		sta jpgruc2r3
		sta jpgruc2g3
		sta jpgruc2b3

		lda #<$0300
		sta jpgruc2r2+0
		lda #>$0300
		sta jpgruc2r2+1

		lda #<$0340
		sta jpgruc2g2+0
		lda #>$0340
		sta jpgruc2g2+1

		lda #<$0380
		sta jpgruc2b2+0
		lda #>$0380
		sta jpgruc2b2+1

		lda #$00
		sta $d020

		ldx #$00
jpg_render_irq_loop		
		lda $d070										; BANK IN BITMAP PALETTE - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070
		DMA_RUN_JOB jpgrender_updatecolours
		lda $d070										; BANK IN BITMAP PALETTE - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%10000000
		sta $d070
		DMA_RUN_JOB jpgrender_updatecolours2red
		DMA_RUN_JOB jpgrender_updatecolours2green
		DMA_RUN_JOB jpgrender_updatecolours2blue

		clc
		lda jpgruc2+0									; add 3*320 to DMA copy
		adc #$c0
		sta jpgruc2+0
		lda jpgruc2+1
		adc #$03
		sta jpgruc2+1
		lda jpgruc3+0
		adc #$00
		sta jpgruc3+0

		clc
		lda jpgruc2r2+0									; add 3*320 to DMA copy
		adc #$c0
		sta jpgruc2r2+0
		lda jpgruc2r2+1
		adc #$03
		sta jpgruc2r2+1
		lda jpgruc2r3+0
		adc #$00
		sta jpgruc2r3+0

		clc
		lda jpgruc2g2+0									; add 3*320 to DMA copy
		adc #$c0
		sta jpgruc2g2+0
		lda jpgruc2g2+1
		adc #$03
		sta jpgruc2g2+1
		lda jpgruc2g3+0
		adc #$00
		sta jpgruc2g3+0

		clc
		lda jpgruc2b2+0									; add 3*320 to DMA copy
		adc #$c0
		sta jpgruc2b2+0
		lda jpgruc2b2+1
		adc #$03
		sta jpgruc2b2+1
		lda jpgruc2b3+0
		adc #$00
		sta jpgruc2b3+0

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
		; shouldn't have to do this and just leave the bitmap palette banked in?
		;lda $d070
		;and #%11001111									; clear bits 4 and 5 (BTPALSEL) so bitmap uses palette 0
		;sta $d070

		lda #$80
		sta $d020

		jsr mouse_update
		jsr keyboard_update

		lda mouse_released
		beq :+

		lda #$02
		sta main_event
		bra :++

:		lda keyboard_shouldsendreleaseevent
		beq :+

		lda #$02
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

jpgrender_updatecolours

		;DMA_HEADER $20000 >> 20, $30000 >> 20
		; f018a = 11 bytes, f018b is 12 bytes
		.byte $0a ; Request format is F018A
		.byte $80, ($20000 >> 20) ; sourcebank
		.byte $81, ($d100 >> 20) ; destbank

		.byte $82, 0 ; Source skip rate (256ths of bytes)
		.byte $83, 1 ; Source skip rate (whole bytes)

		.byte $84, 0 ; Destination skip rate (256ths of bytes)
		.byte $85, 1 ; Destination skip rate (whole bytes)

		.byte $00 ; No more options

		.byte $00 ; Copy and last request
		.word 3*256 ; Size of Copy

jpgruc2
		.word 0											; $20000 & $ffff
jpgruc3
		.byte 2											; ($20000 >> 16)

		.word $d100 & $ffff
		.byte (($d100 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100

		.word $0000

		; $0300*200 = $25800

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecolours2red

		;DMA_HEADER $20000 >> 20, $30000 >> 20
		; f018a = 11 bytes, f018b is 12 bytes
		.byte $0a ; Request format is F018A
		.byte $80, ($20000 >> 20) ; sourcebank
		.byte $81, ($d100 >> 20) ; destbank

		.byte $82, 0 ; Source skip rate (256ths of bytes)
		.byte $83, 1 ; Source skip rate (whole bytes)

		.byte $84, 0 ; Destination skip rate (256ths of bytes)
		.byte $85, 1 ; Destination skip rate (whole bytes)

		.byte $00 ; No more options

		.byte $00 ; Copy and last request
		.word 64 ; Size of Copy

jpgruc2r2
		.word $0000										; $20000 & $ffff
jpgruc2r3
		.byte 2											; ($20000 >> 16)

		.word $d100 & $ffff
		.byte (($d100 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100

		.word $0000

		; $0300*200 = $25800

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecolours2green

		;DMA_HEADER $20000 >> 20, $30000 >> 20
		; f018a = 11 bytes, f018b is 12 bytes
		.byte $0a ; Request format is F018A
		.byte $80, ($20000 >> 20) ; sourcebank
		.byte $81, ($d200 >> 20) ; destbank

		.byte $82, 0 ; Source skip rate (256ths of bytes)
		.byte $83, 1 ; Source skip rate (whole bytes)

		.byte $84, 0 ; Destination skip rate (256ths of bytes)
		.byte $85, 1 ; Destination skip rate (whole bytes)

		.byte $00 ; No more options

		.byte $00 ; Copy and last request
		.word 64 ; Size of Copy

jpgruc2g2
		.word $0100										; $20000 & $ffff
jpgruc2g3
		.byte 2											; ($20000 >> 16)

		.word $d200 & $ffff
		.byte (($d200 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100

		.word $0000

		; $0300*200 = $25800

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_updatecolours2blue

		;DMA_HEADER $20000 >> 20, $30000 >> 20
		; f018a = 11 bytes, f018b is 12 bytes
		.byte $0a ; Request format is F018A
		.byte $80, ($20000 >> 20) ; sourcebank
		.byte $81, ($d300 >> 20) ; destbank

		.byte $82, 0 ; Source skip rate (256ths of bytes)
		.byte $83, 1 ; Source skip rate (whole bytes)

		.byte $84, 0 ; Destination skip rate (256ths of bytes)
		.byte $85, 1 ; Destination skip rate (whole bytes)

		.byte $00 ; No more options

		.byte $00 ; Copy and last request
		.word 64 ; Size of Copy

jpgruc2b2
		.word $0200										; $20000 & $ffff
jpgruc2b3
		.byte 2											; ($20000 >> 16)

		.word $d300 & $ffff
		.byte (($d300 >> 16) & $0f) | %10000000			; turn on bit 7 for I/O when writing to $d100

		.word $0000

		; $0300*200 = $25800

; ----------------------------------------------------------------------------------------------------------------------------------------
