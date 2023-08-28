; -----------------------------------------------------------------------------------------------

dc_base		= $02				; -
dc_bits		= dc_base			; 1
dc_put		= dc_base+2			; 4
dc_get_zp1	= dc_base+2+4		; 4
dc_get_zp2	= dc_base+2+4+4		; 4
dc_get_zp3	= dc_base+2+4+4+4	; 4

; ----------------------------------------------------

; 0 = literal run
; 1 = match

; 2A FF 1E A6 08 FE FE 08 26 16 1B 03 89 87 C4 73
; 9B 47 46 A3 63 E1 7D 8F F9 BD 04 FE B0 3C C4 F1

; $0000: Lit(1, F)
; $0001: Mat(1, 9, F)

; ----------------------------------------------------

; 80       10000000
; asl  1 < 00000000

; bits empty, get new bits

; 2a       00101010
; rol  0 < 01010101 < 1

; GETLEN (because c = 0)

;      A = 1
; asl  0 < 10101010
; carry clear, end

; GETLEN (because next sequence should be a match)

;          A = 1
; asl      10101010
;          01010100 c = 1          carry is one, continue adding to A
; asl      01010100
;          10101000 c = 0
; rol      A = 2                   7th bit 1, so continue
; asl      10101000
;          01010000 c = 1          carry is one, continue adding to A
; asl      01010000
;          10100000 c = 0
; rol      A = 4                   7th bit 1, so continue
; asl      10100000
;          01000000 c = 1          carry is one, continue adding to A
; asl      01000000
;          10000000 c = 0
; rol      A = 8                   7th bit 1, so continue
; asl      10000000
;          00000000 c = 0
;                                  dc_bits = 0 -> fetch next bits
; 

; -----------------------------------------------------------------------------------------------

.macro DECRUNCH_GETNEXTBIT
.scope
		asl dc_bits
		bne :+											; if dc_bits is empty, fetch new data
		jsr decrunch_getnewbits
:
.endscope
.endmacro

.macro DECRUNCH_GETLEN
.scope
		lda #1
decrunch_getlen_loop
		DECRUNCH_GETNEXTBIT
		bcc decrunch_getlen_end							; was the bit we just scrolled out 0?
		DECRUNCH_GETNEXTBIT								; nope, get next bit
		rol												; and scroll into length
		bpl decrunch_getlen_loop						; if the 7th bit is 1, continue getting length?
decrunch_getlen_end
.endscope
.endmacro

decrunch_getnewbits
dc_get1	pha
		lda [dc_get_zp1],z
		sta dc_bits
		rol dc_bits
		inz
		inx
		bne dc_getnewbits_end
dc_getnewbits_inc
		inc dc_get_zp1+1
		inc dc_get_zp2+1
		inc dc_get_zp2+1
dc_getnewbits_end
		pla
		rts

decrunch_tab
		.byte %11011111 ;  3							; short offsets
		.byte %11111011 ;  6
		.byte %00000000 ;  8
		.byte %10000000 ; 10
		
		.byte %11101111 ;  4							; long offsets
		.byte %11111101 ;  7
		.byte %10000000 ; 10
		.byte %11110000 ; 13

; -----------------------------------------------------------------------------------------------

decrunch
		lda #$2a
		sta dc_get_zp1+0
		sta dc_get_zp2+0
		sta dc_get_zp3+0
		lda #$37
		sta dc_get_zp1+1
		sta dc_get_zp2+1
		sta dc_get_zp3+1
		lda #$01
		sta dc_get_zp1+2
		sta dc_get_zp2+2
		sta dc_get_zp3+2
		lda #$00
		sta dc_get_zp1+3
		sta dc_get_zp2+3
		sta dc_get_zp3+3

		ldz #0											; get start address
		ldx #0
:		jsr decrunch_getnewbits
		sta dc_put-1,x									; x already 1 at this point, so subtract 1
		cpz #4
		bcc :-

		; bcs												; c is assumed to be 1 here

		lda #%10000000									; signal that new byte needs fetching
		sta dc_bits

dc_loop	DECRUNCH_GETNEXTBIT
		bcs dc_match

dc_literal
		DECRUNCH_GETLEN									; literal run - get length.
		sta dc_llen+1

		ldy #0
dc_literalloop
		lda [dc_get_zp3],z
		inz
		bne :+
		jsr dc_getnewbits_inc
:		phy
		
		;sta [dc_put+0],y
		iny
dc_llen	cpy #0
		bne dc_literalloop
		clc
		tya
		adc dc_put+0
		sta dc_put+0
		bcc :+
		inc dc_put+1

:		iny
		beq dc_loop

		; Literal has to be followed by a match

dc_match
														; Z = 6 here

		DECRUNCH_GETLEN									; has to continue with a match - get length.
		sta dc_mlen+1

														; Z = 7 here
		ldx #$0a										; LEN = 8 instead of 9?
:		stx $d020
		jmp :-

		cmp #$ff										; length 255 -> EOF
		beq decrunch_end

		cmp #2											; get num bits
		lda #0
		rol
		DECRUNCH_GETNEXTBIT
		rol
		DECRUNCH_GETNEXTBIT
		rol
		tay
		lda decrunch_tab,y
		beq :++

:		DECRUNCH_GETNEXTBIT								; get bits < 8
		rol
		bcs :-
		bmi dc_mshort
:		eor #$ff										; get byte
		tay
		lda [dc_get_zp2],z
		inz
		bne :+
		jsr dc_getnewbits_inc
:		jmp dc_mdone

dc_mshort
		ldy #$ff

dc_mdone
		;clc
		adc dc_put+0
		sta dc_mlda+1
		tya
		adc dc_put+1
		sta dc_mlda+2

		ldy #$ff
dc_mloop
		iny
dc_mlda	lda $beef,y
		sta (dc_put+0),y
dc_mlen	cpy #0
		bne dc_mloop

		;sec
		tya
		adc dc_put+0
		sta dc_put+0
		bcc :+
		inc dc_put+1

:		jmp dc_loop

decrunch_end
		rts

; -----------------------------------------------------------------------------------------------
