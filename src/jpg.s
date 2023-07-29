
; https://web.archive.org/web/20120403212223/http://class.ee.iastate.edu/ee528/Reading%20material/JPEG_File_Format.pdf

jpg_error				.byte 0
jpg_eof					.byte 0
jpg_skipff				.byte 0

jpg_filepos				.byte 0, 0, 0				; can probably get rid of this
jpg_reslen				.word 0						; lame restart markers

; ang = PI/8;
; a1 = cos(2 * ang)					= cos(0.250 * PI)					= 0.7071067811865 * 65536			= 46340.95001184158
; a2 = cos(    ang) - cos(3 * ang)	= cos(0.125 * PI) - cos(0.375 * PI)	= 0.5411961001462 * 65536			= 35467.82761918116
; a3 = cos(2 * ang)					= cos(0.250 * PI)					= 0.7071067811865 * 65536			= 46340.95001184158
; a4 = cos(    ang) + cos(3 * ang)	= cos(0.125 * PI) + cos(0.375 * PI)	= 1.3065629648763 * 65536 - 65536	= 20090.910466138215
; a5 = cos(3 * ang)					= cos(0.375 * PI)					= 0.3826834323650 * 65536			= 25079.541423478528

.define jpg_a1216		46341
.define jpg_a2216		35468
.define jpg_a3216		jpg_a1216
.define jpg_a4216		20091
.define jpg_a5216		25080

; f = { 10, 9.24, 7.07, 3.826, 0, -3.826, -7.07, -9.24 }
; F = {  0,    0,    0,     0, 0,      0,     0,   256 }
; S = {  0,    0,    0,     0, 0,      0,     0,   256 }

.define jpg_negmlo		$0400						; $0A00-$1000 DATA BLOCK!!! = $0600
.define jpg_posmlo		$0500						; mult tables
.define jpg_negmhi		$0700
.define jpg_posmhi		$0800						; 2 pages

/*
.define jpg_a1lo		$2900						; cos(2a), a=pi/8
.define jpg_a1hi		$2a00
.define jpg_a2lo		$2b00						; cos(a) - cos(3a)
.define jpg_a2hi		$2c00
.define jpg_a3lo		jpg_a1lo					; cos(2a)
.define jpg_a3hi		jpg_a1hi
.define jpg_a4lo		$2d00						; cos(a) + cos(3a)
.define jpg_a4hi		$2e00
.define jpg_a4gh		$2f00
.define jpg_a5lo		$3000						; cos(3a)
.define jpg_a5hi		$3100

.define jpg_sec1		$3200
.define jpg_sec2		$3400
.define jpg_sec3		$3600
.define jpg_sec4		$3800
.define jpg_sec5		$3a00
.define jpg_sec6		$3c00
.define jpg_sec7		$3e00						; ends $4000
*/

.define jpg_a1lo		$0a00	; $0000				; cos(2a), a=pi/8
.define jpg_a1hi		$0b00	; $0100

.define jpg_a2lo		$0c00	; $0200				; cos(a) - cos(3a)
.define jpg_a2hi		$0d00	; $0300

.define jpg_a3lo		jpg_a1lo					; cos(2a)
.define jpg_a3hi		jpg_a1hi

.define jpg_a4lo		$0e00	; $0400				; cos(a) + cos(3a)
.define jpg_a4hi		$0f00	; $0500
.define jpg_a4gh		$1000	; $0600

.define jpg_a5lo		$1100	; $0700				; cos(3a)
.define jpg_a5hi		$1200	; $0800

; since the algorithm is really an FFT converted into a DCT, the coefficients need a little massaging before tranformation.
;     f(i) = s(i) / (2cos(i * pi / 16))      i = 0..7
;        with
;     f(0) = f(0) * 2 / sqrt(2) , which can be combined with the first step using the table for i=4.

.define jpg_sec1		$1300	; $0900
.define jpg_sec2		$1500
.define jpg_sec3		$1700
.define jpg_sec4		$1900
.define jpg_sec5		$1b00
.define jpg_sec6		$1d00
.define jpg_sec7		$1f00						; ends $2100

.define jpg_crtab1		$8100						; rgb conversion
.define jpg_crtab2		$8180
.define jpg_cbtab1		$8200
.define jpg_cbtab2		$8280

.define jpg_trans		$8300						; transform

.define jpg_veclo		$8380						; vec to be quantized
.define jpg_vechi		$83c0

.define jpg_imgbuf		$8400						; image data buffer $8700+3*$1300=$c000

.define jpg_huffmem		$c000						; huffman trees ($c000-$c570)

.define jpg_bufwidth	40							; was 40
.define jpg_bufheight	2

.define jpg_channelbufsize	jpg_bufwidth*8*8*jpg_bufheight

.define jpg_imgbufsize	3*jpg_channelbufsize

.define jpg_ybuf		jpg_imgbuf+0*jpg_channelbufsize			; $8700
.define jpg_cbbuf		jpg_imgbuf+1*jpg_channelbufsize			; $9a00
.define jpg_crbuf		jpg_imgbuf+2*jpg_channelbufsize			; $ad00

.define jpg_point		$02
.define jpg_dest		$04
.define jpg_bitslo		$06							; and dequantize
.define jpg_bitshi		$07

.define jpg_mult1lo		$08							; multiplication tables
.define jpg_mult1hi		$0a
.define jpg_mult2lo		$0c
.define jpg_mult2hi		$0e

.define jpg_dct			$10
.define jpg_f0			jpg_dct+0
.define jpg_f1			jpg_dct+2
.define jpg_f2			jpg_dct+4
.define jpg_f3			jpg_dct+6
.define jpg_f4			jpg_dct+8
.define jpg_f5			jpg_dct+10
.define jpg_f6			jpg_dct+12
.define jpg_f7			jpg_dct+14

.define jpg_index		$20							; idct stuff

.define jpg_t1			$22
.define jpg_t2			$24
.define jpg_t3			$26

.define jpg_vsamp		$28							; desample
.define jpg_hsamp		$29


.define jpg_coeff		$02a7						; 8 coefficients
.define jpg_c0			jpg_coeff+0
.define jpg_c1			jpg_coeff+2
.define jpg_c2			jpg_coeff+4
.define jpg_c3			jpg_coeff+6
.define jpg_c4			jpg_coeff+8
.define jpg_c5			jpg_coeff+10
.define jpg_c6			jpg_coeff+12
.define jpg_c7			jpg_coeff+14


.define jpg_count		$f8							; used by getbits, addnode and decodeac
.define jpg_temp2		$f9							; used for huffman nodes
.define jpg_huff		$fa							; huffman pointers
.define jpg_quantp		$fc							; quant table
.define jpg_temp		$fe


.define jpg_notjpg		1							; errors
.define jpg_readerr		2
.define jpg_badqt		3
.define jpg_badht		4
.define jpg_headerr		5
.define jpg_hufferr		6

.define jpg_qt0			$0340						; quantization tables
.define jpg_qt1			jpg_qt0+64					; $0380
.define jpg_qt2			jpg_qt0+128					; $03c0 - only use 3

jpg_process

		lda #00
		sta jpg_error
		sta jpg_eof
		sta jpg_skipff
		sta jpg_nbits

		lda #$ff
		sta jpg_filepos+0
		sta jpg_filepos+1
		sta jpg_filepos+2
		sta jpg_reslen+0
		sta jpg_reslen+1

		lda #>jpg_posmlo
		sta jpg_mult1lo+1
		lda #>jpg_negmlo
		sta jpg_mult2lo+1
		lda #>jpg_posmhi
		sta jpg_mult1hi+1
		lda #>jpg_negmhi
		sta jpg_mult2hi+1

		jsr sdc_getbyte								; check jpeg soi
		cmp #$ff									; marker = $ffd8
		bne jpg_err1
		jsr sdc_getbyte
		cmp #$d8
		bne jpg_err1

		jsr jpg_inithuff
		jsr jpg_initbuff

		;UICORE_CALLELEMENTFUNCTION la1listbox, uilistbox_startaddentries
		;UICORE_SETLISTBOXTEXT la1listbox, uitxt_jpgstart

		jsr jpg_getapp0

:		lda jpg_error
		bne jpg_err1
		jsr jpg_domarker
		lda jpg_eof
		beq :-

		;UICORE_CALLELEMENTFUNCTION la1listbox, uilistbox_draw

		;jmp jpg_idct2d	; handle last row

		rts

jpg_err1
		;UICORE_CALLELEMENTFUNCTION la1listbox, uilistbox_draw

		rts											 ; simply back out if not a jpg

		lda #$00
		sta $d020
		ldx #$00
:		dex
		bne :-
		lda #$e0
		sta $d020
		ldx #$00
:		dex
		bne :-
		jmp jpg_err1

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_initbuff

		lda #<jpg_imgbuf
		sta jpg_point+0
		lda #>jpg_imgbuf
		sta jpg_point+1
		ldx #>(jpg_imgbufsize)
		lda #$80
		ldy #$00
jpgibloop
		sta (jpg_point),y
		dey
		bne jpgibloop
		inc jpg_point+1
		dex
		bne jpgibloop
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_getbyte

		jsr sdc_getbyte
		sta jpg_header+0
		cmp #$ff
		bne :+
		ldx jpg_skipff
		beq :+
		jsr sdc_getbyte
		sta jpg_header+1
		cmp #$ff
		beq jpg_getbyte								;$ffff -> skip
		cmp #00										;$ff00 -> $ff
		bne :+
		lda #$ff
:		;ldx $90
		;stx jpg_eof
		;cpx #64
		rts											; C set -> error

jpg_nbits	.byte 0           ;# of bits left
jpg_byte	.byte 0

jpg_getbit
		dec jpg_nbits
		bpl :+
		lda #7
		sta jpg_nbits
		jsr jpg_getbyte
		sta jpg_byte
:		asl jpg_byte
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_getapp0											; read jfif header

		jsr jpg_getheader
		bcs :+
		lda jpg_header+1
		cmp #$e0									; app0 marker
		beq jpg_ignoresegment

		jmp jpg_domarker2							; doesn't hit?
:		jmp jpg_domarker							; doesn't hit?

jpg_ignoresegment									; ignore rest of segment
		jsr jpg_getbyte
		bcs :+
		lda jpg_eof
		bne :+
		jsr jpg_decrease_headerlength
		bne jpg_ignoresegment
:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; domarker -- read marker and call appropriate routine.

jpg_marker_unknown
		jsr jpg_ignoresegment
		; fall through and get next marker

jpg_domarker
		lda jpg_eof
		beq :+
		rts

:		jsr jpg_getheader							; find next
		bcs jpg_domarker

jpg_domarker2
		lda jpg_header+1
		cmp #$dd
		beq jpg_jmpmarker_dri
		cmp #$db
		beq jpg_jmpmarker_dqt
		cmp #$c4
		beq jpg_jmpmarker_dht
		cmp #$c0
		beq jpg_jmpmarker_sof
		cmp #$da
		bne jpg_marker_unknown

jpg_jmpmarker_sos
		jmp jpg_marker_sos
jpg_jmpmarker_dri
		jmp jpg_marker_dri
jpg_jmpmarker_dqt
		jmp jpg_marker_dqt
jpg_jmpmarker_dht
		jmp jpg_marker_dht
jpg_jmpmarker_sof
		jmp jpg_marker_sof

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_row		.byte 0
jpg_rowoff	.byte 0				; row offset

jpg_marker_sos
		;UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_sos

		dec jpg_skipff								; skip $ff bytes
		
		; -------------------------------
		jsr jpg_rendinit
		; -------------------------------

		jsr sof_get									; LV TODO - BETTER NAMING
		sta jpg_temp								; # of components
		sta jpg_ncomps
:		jsr sof_get									; LV TODO - BETTER NAMING
		sta jpg_temp+1								; component id    01 00     02 11     03 11
		jsr sof_get									; LV TODO - BETTER NAMING
		ldx jpg_temp+1

		pha
		and #$0f
		sta jpg_achuff,x
		pla
		lsr
		lsr
		lsr
		lsr
		sta jpg_dchuff,x
		dec jpg_temp
		bne :-
		jsr sof_get									; scan parameters		00
		jsr sof_get									; (progressive)			3F
		jsr sof_get									; (ignore)				00

; image data begins here							$02BB in pup.jpg file

		lda #00
		sta jpg_row
		sta jpg_col
		jsr jpg_restartdecoder

jpg_sos_readcomponents

		ldx #1										; luma/intensity
		lda #<(jpg_ybuf)
		ldy #>(jpg_ybuf)
		jsr jpg_readdataunit

		ldx jpg_ncomps
		dex
		beq jpg_sos_readdone
		ldx #2										; read chroma
		lda #<(jpg_cbbuf)
		ldy #>(jpg_cbbuf)
		jsr jpg_readdataunit

		ldx jpg_ncomps
		dex
		beq jpg_sos_readdone
		ldx #3										; read chroma
		lda #<(jpg_crbuf)
		ldy #>(jpg_crbuf)
		jsr jpg_readdataunit

jpg_sos_readdone

		jsr decres

		lda jpg_eof
		bne jpg_sos_done
		lda jpg_csamph								; max sample
		clc
		adc jpg_col
		sta jpg_col
		cmp jpg_numcols
		bcc jpg_sos_readcomponents

		jsr jpg_torgb

		lda #00
		sta jpg_col

		lda #<jpg_imgbuf
		ldy #>jpg_imgbuf
		ldx jpg_csampv
		stx jpg_temp2

jpg_sos_rend
		sta jpg_temp+0
		sty jpg_temp+1

		ldx jpg_row									; don't render anything if we haven't reached the start row yet
		cpx jpg_rowoff
		bcc jpg_sos_norend

		; -----------------------------
		jsr jpg_render
		; -----------------------------

jpg_sos_norend
		inc jpg_row
		lda jpg_row
		cmp jpg_numrows
		bcs jpg_sos_done
		sec
		sbc jpg_rowoff
		bcc :+
		cmp #25
		bcs jpg_sos_done

:		lda jpg_temp								; next buffer (i.e. render second char row if vertical sampling is 2)
		clc
		adc jpg_buflen+0
		sta jpg_temp+0
		lda jpg_temp+1
		adc jpg_buflen+1
		tay
		lda jpg_temp
		dec jpg_temp2
		bne jpg_sos_rend

		jmp jpg_sos_readcomponents

jpg_sos_done
		inc jpg_eof
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_cres	.word 0

jpg_marker_dri										; lame restart interval markers
		;UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_dri

		jsr sdc_getbyte
		sta jpg_reslen+1
		sta jpg_cres+1
		jsr sdc_getbyte
		sta jpg_reslen+0
		sta jpg_cres+0
		rts

decres	lda jpg_reslen+1
		cmp #$ff
		beq :+

		dec jpg_cres+0
		bne :+
		lda jpg_cres+1
		beq :++
		dec jpg_cres+1
:		rts

:		sta jpg_nbits								; skip bits
		jsr jpg_getbyte								; read $ffxx
		lda jpg_reslen+0
		sta jpg_cres+0
		lda jpg_reslen+1
		sta jpg_cres+1

jpg_restartdecoder
		ldx #5
:		sta jpg_dclo,x
		sta jpg_dchi,x
		dex
		bpl :-
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_marker_dqt
		;UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_dqt

jpg_marker_dqt_start
		jsr jpg_decrease_headerlength
		beq jpg_marker_dqt_end
		jsr jpg_getbyte

		bcs jpg_marker_dqt_err
		tay
		and #$0f									; number of qt
		bne :+
		ldx #<(jpg_qt0)
		lda #>(jpg_qt0)
		bne dqt_ok
:		cmp #1
		bne :+
		ldx #<(jpg_qt1)
		lda #>(jpg_qt1)
		bne dqt_ok
:		cmp #2
		bne jpg_marker_dqt_err
		ldx #<(jpg_qt2)
		lda #>(jpg_qt2)

dqt_ok	stx jpg_point+0								; qt addr
		sta jpg_point+1
		tya
		and #$f0
		bne jpg_marker_dqt_err						; 0 = 8-bit
		ldy #00
dqt_loop
		sty jpg_temp								; counter
		lda jpg_headerlength+0
		ora jpg_headerlength+1
		beq jpg_marker_dqt_err
		jsr jpg_getbyte
		bcs jpg_marker_dqt_err
		ldy jpg_temp
		sta (jpg_point),y
		jsr jpg_decrease_headerlength

		iny
		cpy #64
		bne dqt_loop
		jmp jpg_marker_dqt_start					; multiple qt's allowed

jpg_marker_dqt_err
		lda #jpg_badqt								; only 0-3 allowed
		sta jpg_error

jpg_marker_dqt_end
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_huff_symbols	.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
jpg_hufflen			.byte 0


jpg_marker_dht
		;UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_dht

jpg_marker_dht_start
		jsr jpg_decrease_headerlength
		beq dhtjerr

dhtgetb	jsr jpg_getbyte
		bcc dhtcont
dhtjerr	jmp jpg_marker_dht_error

dhtcont
		tay											; info byte
		and #$0f
		cmp #$04
		bcs dhtjerr
		asl
		tax											; table num 0-3
		tya
		and #$f0
		beq dht_ok									; dc table
		cmp #$10
		bne dhtjerr
		txa											; ac table
		ora #$08									; +8
		tax

dht_ok		
		lda jpg_hufftop
		sta jpg_dchuff0,x
		sta jpg_huff+0
		lda jpg_hufftop+1
		sta jpg_dchuff0+1,x
		sta jpg_huff+1
		stx jpg_temp2
		ldy #01										; right node
		jsr jpg_newnode								; root node

		ldx #01
:		stx jpg_temp
		lda jpg_headerlength+0
		ora jpg_headerlength+1
		beq jpg_marker_dht_error
		jsr jpg_getbyte
		bcs jpg_marker_dht_error
		ldx jpg_temp
		sta jpg_huff_symbols-1,x
		jsr jpg_decrease_headerlength
		inx
		cpx #17
		bne :-

		lda #$ff
		sta jpg_huffbits+0
		sta jpg_huffbits+1
		lda #1
		sta jpg_hufflen

dhtloop	inc jpg_huffbits+1							; hi,lo!
		bne :+
		inc jpg_huffbits

:		ldx jpg_hufflen
		dec jpg_huff_symbols-1,x
		bpl :+
		cpx #16
		beq dhtnext
		asl jpg_huffbits+1
		rol jpg_huffbits
		inc jpg_hufflen
		bne :-

:		ldx jpg_temp2
		lda jpg_dchuff0,x
		sta jpg_huff+0
		lda jpg_dchuff0+1,x
		sta jpg_huff+1
		jsr jpg_getbyte
		bcs jpg_marker_dht_error
		ldx jpg_hufflen
		jsr jpg_addnode
		bcs jpg_marker_dht_end
		jsr jpg_decrease_headerlength
		jmp dhtloop
dhtnext	jsr jpg_decrease_headerlength
		beq jpg_marker_dht_end
		jmp dhtgetb									; multiple hts

jpg_marker_dht_error
		lda #jpg_badht
		sta jpg_error

jpg_marker_dht_end		
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_height	.word 0
jpg_width	.word 0
jpg_numrows	.byte 0
jpg_numcols	.byte 0
jpg_ncomps	.byte 0									; num components
jpg_csampv	.byte 0, 0, 0, 0, 0, 0					; sampling factors
jpg_csamph	.byte 0, 0, 0, 0, 0, 0					; (horizontal)
jpg_cquant	.byte 0, 0, 0, 0, 0, 0					; quantization table

jpg_marker_sof
		;UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_sof

jpg_marker_sof_start
		ldx #5
		lda #00
:		sta jpg_csampv,x
		sta jpg_csamph,x
		dex
		bpl :-

		jsr sof_get									; get bit depth - should be 8
		cmp #8
		beq sof_ok
		lda #jpg_badqt
		sta jpg_error
		rts

sof_ok	jsr sof_get									; get height - $00e8
		sta jpg_height+1
		jsr sof_get
		sta jpg_height+0
		sec
		sbc #1										; 0..7 instead of 1..8
		sta jpg_numrows
		lda jpg_height+1
		sbc #00
		lsr
		ror jpg_numrows
		lsr
		ror jpg_numrows
		lsr
		ror jpg_numrows
		inc jpg_numrows

		jsr sof_get									; get width - 40122
		sta jpg_width+1
		jsr sof_get
		sta jpg_width+0
		sec
		sbc #1										; 0..7 instead of 1..8
		sta jpg_numcols
		lda jpg_width+1
		sbc #00
		lsr
		ror jpg_numcols
		lsr
		ror jpg_numcols
		lsr
		ror jpg_numcols
		inc jpg_numcols								; 0..7 => 1 col, etc.

		jsr sof_get									; get components - 3
		sta jpg_ncomps
		sta jpg_temp+0

sof_loop
		; 01 22 00 Y
		; 02 11 01 Cb
		; 03 11 01 Cr
		jsr sof_get									; read component ID
		sta jpg_temp+1								;
		jsr sof_get									; read 22, 11, 11
		ldx jpg_temp+1
		pha
		and #$0f
		sta jpg_csampv,x
		pla
		lsr
		lsr
		lsr
		lsr
		sta jpg_csamph,x
		jsr sof_get									; read 0 (Y) or 1 (Cb, Cr)
		ldx jpg_temp+1
		sta jpg_cquant,x
		dec jpg_temp
		bne sof_loop

		; csampv = 0 2 1 1 0 0
		; csamph = 0 2 1 1 0 0
		; cquant = 0 0 1 1 0 0

		ldx #5										; find max sample
		lda #00
:		cmp jpg_csamph,x
		bcs :+
		lda jpg_csamph,x
:		dex
		bne :--
		sta jpg_csamph								; store in +0

		; csamph = 2 2 1 1 0 0

		ldx #5										; find max sample
		lda #00
:		cmp jpg_csampv,x
		bcs :+
		lda jpg_csampv,x
:		dex
		bne :--
		sta jpg_csampv								; store in +0

		; csampv = 2 2 1 1 0 0

		rts

; ----------------------------------

sof_get
		lda jpg_headerlength+0
		ora jpg_headerlength+1
		beq sof_err2
		jsr jpg_decrease_headerlength
		jsr jpg_getbyte
		bcc sof_end
sof_err2
		pla
		pla

sof_err
		lda #jpg_readerr
		sta jpg_error
sof_end
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_decrease_headerlength
		lda jpg_headerlength+0						; lo byte 0?
		bne :+										; nope, decrease
		ora jpg_headerlength+1
		beq :++
		dec jpg_headerlength+1
:		dec jpg_headerlength+0
		lda jpg_headerlength+0
		ora jpg_headerlength+1
:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_header			.word 0							; hi, lo
jpg_headerlength	.word 0							; lo, hi

; getheader -- read in header bytes.
; on exit:
;   c set -> error
;   z set -> end of file

jpg_getheader
		;UICORE_SETLISTBOXTEXT la1listbox, uitxt_jpgheader

		lda #00
		sta jpg_header+0
		sta jpg_header+1

		jsr jpg_getbyte
		cmp #$ff									; expect to find $ff marker
		bne jpg_getheader_error

		jsr jpg_getbyte
		sta jpg_header+1

		cmp #$d8									; start of jpeg
		beq jpg_getheader_ok						; lame photoshop
		cmp #$d9									; end of file
		bne :+
		sta jpg_eof
		beq jpg_getheader_ok

:		jsr jpg_getbyte								; get high byte of length
		bcs jpg_getheader_end
		sta jpg_headerlength+1
		jsr jpg_getbyte								; get lo byte of length
		bcs jpg_getheader_end
		sec
		sbc #2
		sta jpg_headerlength+0						; subtract 2 from length (to get rid of first two bytes included in the length)

		bcs :+
		dec jpg_headerlength+1
:		ora jpg_headerlength+1						; empty segment
		beq jpg_getheader

jpg_getheader_ok		
		clc
		rts

jpg_getheader_error
		sec
		;fallthrough

jpg_getheader_end
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; huffman tree routines.
;
; the huffman tree is implemented as a series of 2-byte nodes.
; left nodes are at huff+2
; right nodes are at (huff) if link < $8000.

; link   = $80xx means xx=leaf value,
; link   = $ffxx means no right link,
; link+2 = hufftop -> no left link.

jpg_dchuff0		.word 0								; addresses
jpg_dchuff1		.word 0
jpg_dchuff2		.word 0
jpg_dchuff3		.word 0
jpg_achuff0		.word 0
jpg_achuff1		.word 0
jpg_achuff2		.word 0
jpg_achuff3		.word 0

jpg_hufftop		.word 0								; end of huffman tree
jpg_hufftx		.byte 0
jpg_huffty		.byte 0

jpg_huffbits	.word 0								; hi,lo
jpg_huffval		.byte 0

jpg_bitp		.byte $00									; bit patterns (masks)
				.byte $01, $02, $04,$08, $10, $20, $40, $80
				.byte $01, $02, $04,$08, $10, $20, $40, $80

jpg_inithuff

		lda #<jpg_huffmem
		sta jpg_hufftop+0
		lda #>jpg_huffmem
		sta jpg_hufftop+1
		rts

; create new node; make current node point to it.
; on entry: .y = 0 -> right node, otherwise left node

jpg_newnode
		sty jpg_huffty
		stx jpg_hufftx

		tya
		bne jpg_nnskip
		lda jpg_hufftop
		sec
		sbc jpg_huff+0
		sta (jpg_huff),y							; point -> new node
		iny
		lda jpg_hufftop+1
		sbc jpg_huff+1
		sta (jpg_huff),y

jpg_nnskip
		lda jpg_hufftop
		sta jpg_point+0
		clc
		adc #2
		sta jpg_hufftop
		lda jpg_hufftop+1
		sta jpg_point+1
		adc #00
		sta jpg_hufftop+1

		ldy #01
		lda #$ff
		sta (jpg_point),y							; init new node

		ldx jpg_hufftx
		ldy jpg_huffty
		clc
		rts

;:err	lda #badht
;		sta error
;		sec
;		rts

; add a new node; .x = length
; (huff) -> tree root

jpg_addnode
		sta jpg_huffval

jpg_anloop

		ldy #1
		cpx #9
		bcc :+
		dey
:		lda jpg_bitp,x
		and jpg_huffbits,y
		bne jpg_anright

jpg_anleft
		lda jpg_huff+0								; check if at end
		clc
		adc #2
		pha
		tay
		lda jpg_huff+1
		adc #00
		pha
		cpy jpg_hufftop+0
		sbc jpg_hufftop+1
		bcc :+										; not a new node
		ldy #$80									; create left node
		jsr jpg_newnode
:		pla
		sta jpg_huff+1
		pla
		sta jpg_huff+0
		jmp jpg_ancontinue

jpg_anright
		ldy #1
		lda (jpg_huff),y							; check for rt ptr
		bpl :+
		dey											; .y=0 -> rt node
		jsr jpg_newnode
:		ldy #00
		lda (jpg_huff),y
		clc
		adc jpg_huff+0
		pha
		iny
		lda (jpg_huff),y
		adc jpg_huff+1
		sta jpg_huff+1
		pla
		sta jpg_huff+0

jpg_ancontinue
		dex
		bne jpg_anloop
		lda #$80
		ldy #01
		sta (jpg_huff),y							; store value
		lda jpg_huffval
		dey
		sta (jpg_huff),y							; $80xx
		clc
		rts

; ----------------------------------------------------------------

; decode DC coeffs

jpg_decodedc
		ldx jpg_curcomp								; set huffman
		lda jpg_dchuff,x
		asl
		tax
		lda jpg_dchuff0,x
		sta jpg_huff+0
		lda jpg_dchuff0+1,x
		sta jpg_huff+1

		jsr jpg_gethuff								; get category A = 8
		ldx jpg_error
		bne :+

		jsr jpg_getbits								; get the bits

		ldx jpg_curcomp
		lda jpg_bitslo
		clc
		adc jpg_dclo,x
		sta jpg_dclo,x
		sta jpg_veclo
		lda jpg_dchi,x
		adc jpg_bitshi
		sta jpg_dchi,x
		sta jpg_vechi
:		rts

; ----------------------------------------------------------------

; decode AC coeffs

jpg_tmphuf	.byte 0
jpg_achuff	.byte 0, 0, 0, 0, 0, 0					; ac table to use
jpg_dchuff	.byte 0, 0, 0, 0, 0, 0					; dc table to use
jpg_dclo	.byte 0, 0, 0, 0, 0, 0					; dc coeffs
jpg_dchi	.byte 0, 0, 0, 0, 0, 0

jpg_curcomp	.byte 0

jpg_decodeac
		ldx jpg_curcomp								; set huffman
		lda jpg_achuff,x
		asl
		tax
		stx jpg_tmphuf

		ldy #1
decacloop
		sty jpg_temp2								; index
		ldx jpg_tmphuf
		lda jpg_achuff0+0,x
		sta jpg_huff+0
		lda jpg_achuff0+1,x
		sta jpg_huff+1

		jsr jpg_gethuff								; get rle len
		beq decacfill
		ldx jpg_error
		bne jpg_decodeac_end
		sta jpg_count								; temp
		lsr
		lsr
		lsr
		lsr											; # of zeros
		beq :++
decacfill		
		tax
		lda #00
		ldy jpg_temp2
:		sta jpg_veclo,y
		sta jpg_vechi,y
		iny
		cpy #64
		bcs jpg_decodeac_end
		dex
		bne :-
		sty jpg_temp2
:		lda jpg_count
		and #$0f									; category
		jsr jpg_getbits
		ldy jpg_temp2
		lda jpg_bitslo
		sta jpg_veclo,y
		lda jpg_bitshi
		sta jpg_vechi,y
		iny
		cpy #64
		bcc decacloop

jpg_decodeac_end		
		rts

; ----------------------------------------------------------------

; gethuff -- get valid huffman code from (huff)

jpg_gethuff
		ldy #01
		lda (jpg_huff),y
		cmp #$80
		beq jpgghfound

		jsr jpg_getbit
		bcs jpgghright
		lda jpg_huff+0
		adc #2										; c clear
		tax
		lda jpg_huff+1
		adc #00
		tay
		cpx jpg_hufftop+0
		sbc jpg_hufftop+1
		bcs jpg_gethuff_error
		sty jpg_huff+1
		stx jpg_huff+0
		bcc jpg_gethuff

jpgghright
		ldy #01
		lda (jpg_huff),y
		bmi jpg_gethuff_error
		pha
		dey
		lda (jpg_huff),y
		clc
		adc jpg_huff+0
		sta jpg_huff+0
		pla
		adc jpg_huff+1
		sta jpg_huff+1
		bne jpg_gethuff

jpgghfound
		dey
		lda (jpg_huff),y
		rts

jpg_gethuff_error
		inc $d020
		jmp *-3

		lda #jpg_hufferr
		sta jpg_error
		rts

; ----------------------------------------------------------------

; retrieve .a bits and convert to signed number in (bitslo, bitshi)

jpg_sign	.byte 0

jpg_getbits

		sta jpg_count
		tax
		beq jpg_getbits_zero
		jsr jpg_getbit
		lda #00
		bcs :+
		lda #$ff									; 0-> negative
:		sta jpg_bitshi
		rol
		sta jpg_bitslo
		sta jpg_sign
		dec jpg_count
		beq jpggbsdone
jpggbsloop
		jsr jpg_getbit
		rol jpg_bitslo
		rol jpg_bitshi
		dec jpg_count
		bne jpggbsloop
jpggbsdone
		lda jpg_sign
		bpl jpggbsrts
		inc jpg_bitslo								; make two's complement
		bne jpggbsrts
		inc jpg_bitshi
jpggbsrts		
		rts

jpg_getbits_zero
		sta jpg_bitslo
		sta jpg_bitshi
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; since the algorithm is really an FFT converted into a DCT, the coefficients need a little massaging before tranformation.
;
; specifically,
;     f(i) = s(i) / (2cos(i * pi / 16))      i = 0..7
;        with
;     f(0) = f(0) * 2 / sqrt(2), which can be combined with the first step using the table for i=4.
;
; these multipliers can in part be incorporated in the quantization table, but for now they're out in the open.

jpg_prepdat

		.macro PREPDAT sec, f
			ldx #00
			lda #<sec
			sta jpg_point+0
			lda #>sec
			sta jpg_point+1
			lda f+0
			sta jpg_bitslo
			lda f+1
			jsr jpg_pmult
			sta f+1
			lda jpg_bitslo
			sta f+0
		.endmacro

		PREPDAT jpg_sec4, jpg_f0
		PREPDAT jpg_sec1, jpg_f1
		PREPDAT jpg_sec2, jpg_f2
		PREPDAT jpg_sec3, jpg_f3
		PREPDAT jpg_sec4, jpg_f4
		PREPDAT jpg_sec5, jpg_f5
		PREPDAT jpg_sec6, jpg_f6
		PREPDAT jpg_sec7, jpg_f7

		rts

jpg_pmult											; exit .a = bitshi
		bmi jpg_pmult_neg
		beq :++
:		inx											; shift count
		lsr
		ror jpg_bitslo
		cmp #00
		bne :-
:		sta jpg_bitshi
		lda jpg_bitslo
		asl
		rol jpg_bitshi
		adc jpg_point+0
		sta jpg_point+0
		lda jpg_bitshi
		adc jpg_point+1
		sta jpg_point+1
		ldy #00
		lda (jpg_point),y
		sta jpg_bitslo
		iny
		lda (jpg_point),y
		dex
		bmi :++
:		asl jpg_bitslo
		rol
		dex
		bpl :-
:		rts

jpg_pmult_neg
		sta jpg_bitshi
		lda #00
		sec
		sbc jpg_bitslo
		sta jpg_bitslo
		lda #00
		sbc jpg_bitshi
		beq :++
:		inx											; shift count
		lsr
		ror jpg_bitslo
		cmp #00
		bne :-
:		asl jpg_bitslo
		rol
		sta jpg_bitshi
		lda jpg_bitslo
		adc jpg_point+0
		sta jpg_point+0
		lda jpg_bitshi
		adc jpg_point+1
		sta jpg_point+1
		ldy #00
		lda (jpg_point),y
		sta jpg_bitslo
		iny
		lda (jpg_point),y
		dex
		bmi :++
:		asl jpg_bitslo
		rol
		dex
		bpl :-
:		sta jpg_bitshi
		lda #00
		sec
		sbc jpg_bitslo
		sta jpg_bitslo
		lda #00
		sbc jpg_bitshi
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; compute the inverse dct (1d)
; uses modified reversed flowgraph from pennebaker & mitchell, p. 52
;
; input:  dct coeffs contained in flo/fhi
; output: original coeffs in coeffs

/*
	void calcIdct()
	{
		double t1, t2, t3, t4;

		// Stage 1

		for (int i=0; i<8; i++)
		{
			F[i] = S[i] / (2.0 * Math.cos(i * ang/2));
		}

		F[0] = F[0]*2 / Math.sqrt(2.0);

		t1   =  F[5] - F[3];
		t2   =  F[1] + F[7];
		t3   =  F[1] - F[7];
		t4   =  F[5] + F[3];
		F[5] =  t1;
		F[1] =  t2;
		F[7] =  t3;
		F[3] =  t4;

		// Stage 2

		t1   =  F[2] - F[6];
		t2   =  F[2] + F[6];
		F[2] =  t1;
		F[6] =  t2;
		t1   =  F[1] - F[3];
		t2   =  F[1] + F[3];
		F[1] =  t1;
		F[3] =  t2;

		// Stage 3

		F[2] =  a1*F[2];
		t1   = -a5*(F[5] + F[7]);
		F[5] = -a2*F[5] + t1;
		F[1] =  a3*F[1];
		F[7] =  a4*F[7] + t1;

		// Stage 4

		t1   =  F[0] + F[4];
		t2   =  F[0] - F[4];
		F[0] =  t1;
		F[4] =  t2;
		F[6] =  F[2] + F[6];

		// Stage 5

		t1   =  F[0] + F[6];
		t2   =  F[2] + F[4];
		t3   =  F[4] - F[2];
		t4   =  F[0] - F[6];
		F[0] =  t1;
		F[4] =  t2;
		F[2] =  t3;
		F[6] =  t4;

		F[3] =  F[3] + F[7];
		F[7] =  F[7] + F[1];
		F[1] =  F[1] - F[5];
		F[5] = -F[5];

		// Final stage

		f[0] = (F[0] + F[3]);
		f[1] = (F[4] + F[7]);
		f[2] = (F[2] + F[1]);
		f[3] = (F[6] + F[5]);

		f[4] = (F[6] - F[5]);
		f[5] = (F[2] - F[1]);
		f[6] = (F[4] - F[7]);
		f[7] = (F[0] - F[3]);
	}
*/

jpg_idct
		jsr jpg_prepdat								; shift and such

; stage 1: f(5) <- f(5) - f(3)
;          f(1) <- f(1) + f(7)
;          f(7) <- f(1) - f(7)
;          f(3) <- f(5) + f(3)

		lda jpg_f5
		sec
		sbc jpg_f3
		sta jpg_t1
		lda jpg_f5+1
		sbc jpg_f3+1
		sta jpg_t1+1

		lda jpg_f1
		clc
		adc jpg_f7
		sta jpg_t2
		lda jpg_f1+1
		adc jpg_f7+1
		sta jpg_t2+1

		lda jpg_f1
		sec
		sbc jpg_f7
		sta jpg_t3
		lda jpg_f1+1
		sbc jpg_f7+1
		sta jpg_t3+1

		lda jpg_f5
		clc
		adc jpg_f3
		sta jpg_f3
		lda jpg_f5+1
		adc jpg_f3+1
		sta jpg_f3+1

		lda jpg_t3
		sta jpg_f7
		lda jpg_t3+1
		sta jpg_f7+1

		lda jpg_t2
		sta jpg_f1
		lda jpg_t2+1
		sta jpg_f1+1

		lda jpg_t1
		sta jpg_f5
		lda jpg_t1+1
		sta jpg_f5+1

; stage 2: f(2) <- f(2) - f(6)
;          f(6) <- f(2) + f(6)
;          f(1) <- f(1) - f(3)
;          f(3) <- f(1) + f(3)

		lda jpg_f2
		sec
		sbc jpg_f6
		sta jpg_t1
		lda jpg_f2+1
		sbc jpg_f6+1
		sta jpg_t1+1

		lda jpg_f2
		clc
		adc jpg_f6
		sta jpg_f6
		lda jpg_f2+1
		adc jpg_f6+1
		sta jpg_f6+1

		lda jpg_t1
		sta jpg_f2
		lda jpg_t1+1
		sta jpg_f2+1

		lda jpg_f1
		sec
		sbc jpg_f3
		sta jpg_t1
		lda jpg_f1+1
		sbc jpg_f3+1
		sta jpg_t1+1

		lda jpg_f1
		clc
		adc jpg_f3
		sta jpg_f3
		lda jpg_f1+1
		adc jpg_f3+1
		sta jpg_f3+1

		lda jpg_t1
		sta jpg_f1
		lda jpg_t1+1
		sta jpg_f1+1

; stage 3: f(2) <- a1*f(2)
;          f(5) <- -a2*f(5) + t1
;          f(1) <- a3*f(1)
;          f(7) <- a4*f(7) + t1
; where t1 = -a5*(f(5) + f(7))

; f(2) <- a1*f(2)

		ldx jpg_f2									; lo
		ldy jpg_f2+1								; hi
		lda jpg_a1lo,y
		clc
		adc jpg_a1hi,x
		sta jpg_bitslo								; lo byte
		lda jpg_a1hi,y
		adc #00
		cpy #$80
		bcc :+
		sta jpg_bitshi
		lda jpg_bitslo
		sbc #<jpg_a1216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a1216
:		sta jpg_f2+1
		lda jpg_bitslo
		sta jpg_f2

; f(1) = a3*f(1)

		ldx jpg_f1									; lo
		ldy jpg_f1+1								; hi
		lda jpg_a3lo,y
		clc
		adc jpg_a3hi,x
		sta jpg_bitslo
		lda jpg_a3hi,y
		adc #00
		cpy #$80
		bcc :+
		sta jpg_bitshi
		lda jpg_bitslo
		sbc #<jpg_a3216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a3216
:		sta jpg_f1+1
		lda jpg_bitslo
		sta jpg_f1

; t1 = -a5*(f(5) + f(7))

		lda jpg_f5
		clc
		adc jpg_f7
		tax											; lo
		lda jpg_f5+1
		adc jpg_f7+1
		tay											; hi
		lda jpg_a5lo,y
		clc
		adc jpg_a5hi,x
		sta jpg_bitslo
		lda jpg_a5hi,y
		adc #00
		sta jpg_bitshi
		cpy #$80
		bcc :+
		lda jpg_bitslo
		sbc #<jpg_a5216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a5216
		sta jpg_bitshi
:		lda jpg_bitslo
		eor #$ff
		clc
		adc #01
		sta jpg_t1
		lda jpg_bitshi
		eor #$ff
		adc #00
		sta jpg_t1+1

; f(5) = t1 - a2*f(5)

		ldx jpg_f5									; lo
		ldy jpg_f5+1								; hi
		lda jpg_a2lo,y
		clc
		adc jpg_a2hi,x
		sta jpg_bitslo
		lda jpg_a2hi,y
		adc #00
		cpy #$80
		bcc :+
		sta jpg_bitshi
		lda jpg_bitslo
		sbc #<jpg_a2216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a2216
:		sta jpg_bitshi
		lda jpg_t1
		sec
		sbc jpg_bitslo
		sta jpg_f5
		lda jpg_t1+1
		sbc jpg_bitshi
		sta jpg_f5+1

; f(7) = a4*f(7) + t1

		ldx jpg_f7									; lo
		ldy jpg_f7+1								; hi
		lda jpg_a4lo,y
		clc
		adc jpg_a4hi,x
		sta jpg_bitslo
		lda jpg_a4hi,y
		adc jpg_a4gh,x								; a4*.x can be >255
		cpy #$80
		bcc :+
		sta jpg_bitshi
		lda jpg_bitslo
		sbc #<jpg_a4216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a4216
:		sta jpg_bitshi
		lda jpg_bitslo
		clc
		adc jpg_t1
		sta jpg_f7
		lda jpg_bitshi
		adc jpg_t1+1
		sta jpg_f7+1

; stage 4:
;   f(0) <- f(0) + f(4)
;   f(4) <- f(0) - f(4)
;   f(6) <- f(2) + f(6)

		lda jpg_f0
		clc
		adc jpg_f4
		sta jpg_t1
		lda jpg_f0+1
		adc jpg_f4+1
		sta jpg_t1+1

		lda jpg_f0
		sec
		sbc jpg_f4
		sta jpg_f4
		lda jpg_f0+1
		sbc jpg_f4+1
		sta jpg_f4+1

		lda jpg_t1
		sta jpg_f0
		lda jpg_t1+1
		sta jpg_f0+1

		lda jpg_f2
		clc
		adc jpg_f6
		sta jpg_f6
		lda jpg_f2+1
		adc jpg_f6+1
		sta jpg_f6+1

; stage 5:
;   f(0) <- f(0) + f(6)
;   f(4) <- f(2) + f(4)
;   f(2) <- f(4) - f(2)
;   f(6) <- f(0) - f(6)
;   f(3) <- f(3) + f(7)
;   f(7) <- f(7) + f(1)
;   f(1) <- f(1) - f(5)
;   f(5) <- -f(5)

		lda jpg_f0
		clc
		adc jpg_f6
		sta jpg_t1
		lda jpg_f0+1
		adc jpg_f6+1
		sta jpg_t1+1

		lda jpg_f0
		sec
		sbc jpg_f6
		sta jpg_f6
		lda jpg_f0+1
		sbc jpg_f6+1
		sta jpg_f6+1
		lda jpg_t1
		sta jpg_f0
		lda jpg_t1+1
		sta jpg_f0+1

		lda jpg_f4
		clc
		adc jpg_f2
		sta jpg_t1
		lda jpg_f4+1
		adc jpg_f2+1
		sta jpg_t1+1

		lda jpg_f4
		sec
		sbc jpg_f2
		sta jpg_f2
		lda jpg_f4+1
		sbc jpg_f2+1
		sta jpg_f2+1
		lda jpg_t1
		sta jpg_f4
		lda jpg_t1+1
		sta jpg_f4+1

		lda jpg_f3
		clc
		adc jpg_f7
		sta jpg_f3
		lda jpg_f3+1
		adc jpg_f7+1
		sta jpg_f3+1

		lda jpg_f7
		clc
		adc jpg_f1
		sta jpg_f7
		lda jpg_f7+1
		adc jpg_f1+1
		sta jpg_f7+1

		lda jpg_f1
		sec
		sbc jpg_f5
		sta jpg_f1
		lda jpg_f1+1
		sbc jpg_f5+1
		sta jpg_f1+1

		lda #00
		sec
		sbc jpg_f5
		sta jpg_f5
		lda #00
		sbc jpg_f5+1
		sta jpg_f5+1

; final stage:
;   c(0) = f(0) + f(3)
;   c(1) = f(4) + f(7)
;   c(2) = f(2) + f(1)
;   c(3) = f(6) + f(5)
;   c(4) = f(6) - f(5)
;   c(5) = f(2) - f(1)
;   c(6) = f(4) - f(7)
;   c(7) = f(0) - f(3)
;
; note: values are offset -128

		lda jpg_f0
		clc
		adc jpg_f3
		sta jpg_c0
		lda jpg_f0+1
		adc jpg_f3+1
		sta jpg_c0+1

		lda jpg_f4
		clc
		adc jpg_f7
		sta jpg_c1
		lda jpg_f4+1
		adc jpg_f7+1
		sta jpg_c1+1

		lda jpg_f2
		clc
		adc jpg_f1
		sta jpg_c2
		lda jpg_f2+1
		adc jpg_f1+1
		sta jpg_c2+1

		lda jpg_f6
		clc
		adc jpg_f5
		sta jpg_c3
		lda jpg_f6+1
		adc jpg_f5+1
		sta jpg_c3+1

		lda jpg_f6
		sec
		sbc jpg_f5
		sta jpg_c4
		lda jpg_f6+1
		sbc jpg_f5+1
		sta jpg_c4+1

		lda jpg_f2
		sec
		sbc jpg_f1
		sta jpg_c5
		lda jpg_f2+1
		sbc jpg_f1+1
		sta jpg_c5+1

		lda jpg_f4
		sec
		sbc jpg_f7
		sta jpg_c6
		lda jpg_f4+1
		sbc jpg_f7+1
		sta jpg_c6+1

		lda jpg_f0
		sec
		sbc jpg_f3
		sta jpg_c7
		lda jpg_f0+1
		sbc jpg_f3+1
		sta jpg_c7+1
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_idct2d

jpg_idct2d_cols										; first the columns

		ldx #00
ji2dcloop		
		stx jpg_index
		ldy #00
:		lda jpg_trans+0,x
		sta jpg_dct+0,y
		lda jpg_trans+1,x
		sta jpg_dct+1,y
		txa
		clc
		adc #16
		tax
		iny
		iny
		cpy #16
		bne :-

		jsr jpg_idct

		ldy #0
		ldx jpg_index
:		lda jpg_coeff+0,y
		sta jpg_trans+0,x
		lda jpg_coeff+1,y
		sta jpg_trans+1,x
		txa
		clc
		adc #16
		tax
		iny
		iny
		cpy #16
		bne :-
		ldx jpg_index
		inx
		inx
		cpx #16
		bcc ji2dcloop

jpg_idct2d_rows										; then the rows

		ldx #00
		stx jpg_index
		stx jpg_count
ji2drloop		
		ldy #00
:		lda jpg_trans+0,x
		sta jpg_dct+0,y
		lda jpg_trans+1,x
		sta jpg_dct+1,y
		inx
		inx
		iny
		iny
		cpy #16
		bne :-
		stx jpg_index
		jsr jpg_idct
		ldy jpg_count
		ldx #00
:		lda jpg_coeff+0,x
		sta jpg_bitslo
		lda jpg_coeff+1,x
		cmp #$80
		ror
		ror jpg_bitslo
		cmp #$80
		ror
		ror jpg_bitslo
		sta jpg_bitshi
		lda jpg_bitslo
		adc #128									; c determines rounding
		; sta (dest),y
		sta jpg_trans,y
		lda jpg_bitshi								; range check
		adc #00
		beq ji2drcont
		bpl ji2drpos
		lda #00
		.byte $2c									; BIT $xxxx
ji2drpos
		lda #$ff
		; sta (dest),y
		sta jpg_trans,y
ji2drcont		
		; inc dest
		iny
		inx
		inx
		cpx #16
		bne :-
		sty jpg_count
		ldx jpg_index
		cpx #128
		bcc ji2drloop

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

;
; convert to rgb
;

.define jpg_ypoint		jpg_point
.define jpg_cbpoint		jpg_dest
.define jpg_crpoint		jpg_bitslo

jpg_torgb
		lda #<(jpg_ybuf)
		sta jpg_ypoint+0
		lda #>(jpg_ybuf)
		sta jpg_ypoint+1
		lda #<(jpg_cbbuf)
		sta jpg_cbpoint+0
		lda #>(jpg_cbbuf)
		sta jpg_cbpoint+1
		lda #<(jpg_crbuf)
		sta jpg_crpoint+0
		lda #>(jpg_crbuf)
		sta jpg_crpoint+1

		ldy #00
		ldx jpg_ncomps
		dex
		bne jpg_torgb_loop								; if grayscale, copy Y to Cb and Cr
		ldx #>(jpg_cbbuf-jpg_ybuf)
:		lda (jpg_ypoint),y
		sta (jpg_cbpoint),y
		sta (jpg_crpoint),y
		iny
		bne :-
		inc jpg_ypoint+1
		inc jpg_cbpoint+1
		inc jpg_crpoint+1
		dex
		bne :-
		rts

jpg_torgb_loop
		lda #00
		sta jpg_temp+1
		lda (jpg_cbpoint),y
		eor #$80
		bpl jpg_torgb_poscb
		eor #$ff ; negcb
		clc
		adc #01
		tax
		lda (jpg_ypoint),y
		clc
		adc jpg_cbtab1,x
		sta jpg_temp+0
		bcc :+
		inc jpg_temp+1								; high byte
:		lda (jpg_ypoint),y
		sec
		sbc jpg_cbtab2,x
		bcs jpg_torgb_cont
		lda #00										; underflow
		beq jpg_torgb_cont

jpg_torgb_poscb
		tax
		lda (jpg_ypoint),y
		sec
		sbc jpg_cbtab1,x
		sta jpg_temp+0
		bcs :+
		dec jpg_temp+1
:		lda (jpg_ypoint),y
		clc
		adc jpg_cbtab2,x
		bcc jpg_torgb_cont
		lda #255
jpg_torgb_cont
		sta jpg_temp2

		lda (jpg_crpoint),y
		eor #$80
		bpl jpg_torgb_poscr
		eor #$ff ; negcr
		clc
		adc #01
		tax
		lda jpg_temp+0
		clc
		adc jpg_crtab2,x
		sta jpg_temp+0
		lda jpg_temp+1
		adc #00
		beq :++
		bpl :+
		lda #00
		.byte $2c									; BIT $xxxx
:		lda #255
		.byte $2c									; BIT $xxxx
:		lda jpg_temp
		sta (jpg_cbpoint),y							; green
		lda (jpg_ypoint),y
		sec
		sbc jpg_crtab1,x
		bcs jpg_torgb_done
		lda #00
		beq jpg_torgb_done

jpg_torgb_poscr
		tax
		lda jpg_temp+0
		sec
		sbc jpg_crtab2,x
		sta jpg_temp+0
		lda jpg_temp+1
		sbc #00
		beq :++
		bpl :+
		lda #00
		.byte $2c									; BIT $xxxx
:		lda #255
		.byte $2c									; BIT $xxxx
:		lda jpg_temp
		sta (jpg_cbpoint),y
		lda (jpg_ypoint),y
		clc
		adc jpg_crtab1,x
		bcc jpg_torgb_done
		lda #255

jpg_torgb_done
		sta (jpg_ypoint),y							; red
		lda jpg_temp2
		sta (jpg_crpoint),y							; blue
		iny
		beq :++
:		jmp jpg_torgb_loop

:		inc jpg_ypoint+1
		inc jpg_cbpoint+1
		inc jpg_crpoint+1
		lda jpg_ypoint+1
		cmp #>(jpg_cbbuf)
		bcc :--
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; desample -- expand dct square by sample factor and reorg data.
; on entry: dest = destination buffer

jpg_linelen		.word jpg_bufwidth*8

jpg_desample 

		lda #00

jpg_desample_newrow		
		ldx jpg_vsamp
		stx jpg_huff								; temporary

jpg_desample_oldrow
		sta jpg_temp2								; current element
		lda #8
		sta jpg_count								; column
		
		ldy #00
:		ldx jpg_temp2
		lda jpg_trans,x
		ldx jpg_hsamp

jpg_desample_expand
		sta (jpg_dest),y
		iny
		dex
		bne jpg_desample_expand
		inc jpg_temp2
		dec jpg_count
		bne :-

		clc											; next scanline
		lda jpg_dest+0
		adc jpg_linelen+0
		sta jpg_dest+0
		lda jpg_dest+1
		adc jpg_linelen+1
		sta jpg_dest+1

		sec
		lda jpg_temp2
		sbc #8										; start of row
		dec jpg_huff								; horizonal sampling
		bne jpg_desample_oldrow
		lda jpg_temp2
		cmp #64
		bne jpg_desample_newrow
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_desample_bayer

		ldx #0

:		ldy #0
:		lda jpg_trans,x

		sta $d770
		lda jpg_bayer_matrix,x
		sta $d774
		lda $d778+1

		sta (jpg_dest),y
		iny
		inx
		cpy #8
		bne :-

		clc											; next scanline
		lda jpg_dest+0
		adc jpg_linelen+0
		sta jpg_dest+0
		lda jpg_dest+1
		adc jpg_linelen+1
		sta jpg_dest+1

		cpx #64
		bne :--

		rts

/*

; FOR DEBUG PURPOSES
; nodesample -- don't expand dct square, just leave as is, but copy so it can be debugged
; on entry: dest = destination buffer

jpg_nodesample 

		ldx #0

:		ldy #0
:		lda jpg_trans,x
		sta (jpg_dest),y
		iny
		inx
		cpy #8
		bne :-

		clc											; next scanline
		lda jpg_dest+0
		adc jpg_linelen+0
		sta jpg_dest+0
		lda jpg_dest+1
		adc jpg_linelen+1
		sta jpg_dest+1

		cpx #64
		bne :--

		rts

*/

; ----------------------------------------------------------------------------------------------------------------------------------------

; dequantize the vector vec
; mult is 16 bit signed x 8 bit unsigned with 16-bit result, so sign etc. are taken care of automatically.
; result -> trans

jpg_quanttab
		.word jpg_qt0
		.word jpg_qt1
		.word jpg_qt2

jpg_zigzag
		.byte   0,   2,   16,  32,  18,   4,   6,  20		; table to un-zigzag coeffs
		.byte  34,  48,   64,  50,  36,  22,   8,  10		; multiples of 2, since 2 byte result.
		.byte  24,  38,   52,  66,  80,  96,  82,  68
		.byte  54,  40,   26,  12,  14,  28,  42,  56
		.byte  70,  84,   98, 112, 114, 100,  86,  72
		.byte  58,  44,   30,  46,  60,  74,  88, 102
		.byte  116, 118, 104,  90,  76,  62,  78,  92
		.byte  106, 120, 122, 108,  94, 110, 124, 126

;   0  1  5  6 14 15 27 28
;   2  4  7 13 16 26 29 42
;   3  8 12 17 25 30 41 43
;   9 11 18 24 31 40 44 53
;  10 19 23 32 39 45 52 54
;  20 22 33 38 46 51 55 60
;  21 34 37 47 50 56 59 61
;  35 36 48 49 57 58 62 63

;  0  1  8 16  9  2  3 10
; 17 24 32 25 18 11  4  5
; 12 19 26 33 40 48 41 34
; 27 20 13  6  7 14 21 28
; 35 42 49 56 57 50 43 36
; 29 22 15 23 30 37 44 51
; 58 59 52 45 38 31 39 46
; 53 60 61 54 47 55 62 63

jpg_dequantize
		ldx jpg_curcomp
		lda jpg_cquant,x
		asl
		tax
		lda jpg_quanttab+0,x
		sta jpg_quantp+0
		lda jpg_quanttab+1,x
		sta jpg_quantp+1

		ldx #63
jpg_dequantize_loop
		txa
		tay
		lda (jpg_quantp),y
		sta jpg_mult1lo+0
		sta jpg_mult1hi+0
		eor #$ff
		clc
		adc #1
		sta jpg_mult2lo+0
		sta jpg_mult2hi+0

		ldy jpg_veclo,x
		bne :+
		sty jpg_bitslo
		sty jpg_bitshi
		beq jpg_dequantize_high

:		lda (jpg_mult1lo),y
		sec
		sbc (jpg_mult2lo),y
		sta jpg_bitslo
		lda (jpg_mult1hi),y
		sbc (jpg_mult2hi),y
		sta jpg_bitshi

jpg_dequantize_high
		ldy jpg_vechi,x
		lda (jpg_mult1lo),y
		sec
		sbc (jpg_mult2lo),y
		clc
		adc jpg_bitshi

		ldy jpg_zigzag,x							; un-zigzag
		iny
		sta jpg_trans,y
		dey
		lda jpg_bitslo
		sta jpg_trans,y
		dex

		bpl jpg_dequantize_loop

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_curbuf		.word 0
jpg_rend		.byte 0
;jpg_currow		.byte 0
jpg_curcol		.byte 0
jpg_rendflag	.byte 0

jpg_fetch											; fetch the data
		lda #00
		sta jpg_dest+1
		lda jpg_curcol
		cmp #jpg_bufwidth							; catches neg too
		rol jpg_rendflag							; c set?

		asl											; offset = col*8
		rol jpg_dest+1
		asl
		rol jpg_dest+1
		asl
		rol jpg_dest+1
		adc jpg_curbuf+0							; ybuf, etc.
		sta jpg_dest+0 								; data storage
		lda jpg_curbuf+1
		adc jpg_dest+1
		sta jpg_dest+1

:decode
		jsr jpg_decodedc
		lda jpg_error
		bne decode_error
		jsr jpg_decodeac
		lda jpg_error
		bne decode_error
		lda jpg_rendflag							; are we rendering (i.e. are we right of the offset?)? (no point dequantizing, etc, if we aren't)
		bne :++

		jsr jpg_dequantize

		jsr jpg_idct2d

		lda jpg_curcomp
		cmp #1
		bne :+
		;jmp jpg_desample_bayer						; add bayer pattern to Y/luma component layer
:		jmp jpg_desample

:		rts

decode_error
		lda #$00
		sta $d020
		lda #$80
		sta $d020
		jmp :-

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_buflen		.word jpg_bufwidth*8*8

jpg_col			.byte 0
jpg_coloff		.byte 0								; col offset

jpg_readdataunit
		sta jpg_curbuf+0
		sty jpg_curbuf+1
		stx jpg_curcomp								; 1 for Y, 2 for Cb, 3 for Cr

		lda #00
		sta jpg_rend

		ldy #00										; compute expansion factors
		tya											; maxsamp/samp
		clc
:		iny
		adc jpg_csamph,x
		cmp jpg_csamph								; max
		bcc :-
		sty jpg_hsamp
		lda #00
		tay
		clc
:		iny
		adc jpg_csampv,x							; x = 1 -> 2
		cmp jpg_csampv								; max = 2
		bcc :-
		sty jpg_vsamp								; 1

		lda jpg_csampv,x
		sta jpg_temp+0								; vert samp = 2

jpg_readdataunit_loopy
		ldx jpg_curcomp								; 1

		lda jpg_csamph,x
		sta jpg_temp+1								; horiz sampling = 2
		lda jpg_col
		sec
		sbc jpg_coloff
		sta jpg_curcol

jpg_readdataunit_loopx
		lda jpg_rend
		sta jpg_rendflag
		jsr jpg_fetch								; FETCH!!!

		lda jpg_error
		bne jpg_readdataunit_end
		lda jpg_curcol
		clc
		adc jpg_hsamp
		sta jpg_curcol
		dec jpg_temp+1
		bne jpg_readdataunit_loopx

		ldx jpg_vsamp
jpg_readdataunit_nextrow
		clc
		lda jpg_curbuf+0
		adc jpg_buflen+0
		sta jpg_curbuf+0
		lda jpg_curbuf+1
		adc jpg_buflen+1
		sta jpg_curbuf+1
		dex
		bne jpg_readdataunit_nextrow

		dec jpg_temp
		bne jpg_readdataunit_loopy

jpg_readdataunit_end		
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------
