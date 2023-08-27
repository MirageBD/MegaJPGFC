; -----------------------------------------------------------------------------------------------

; call: Y = AddrLo
;       X = AddrHi

dc_base		= $02			; -
dc_bits		= dc_base		; 1
dc_put		= dc_base+2		; 2

; -----------------------------------------------------------------------------------------------

.macro DECRUNCH_GETNEXTBIT
.scope
		asl dc_bits
		bne decrunch_getnextbit_end
		jsr decrunch_getnewbits
decrunch_getnextbit_end
.endscope
.endmacro

.macro DECRUNCH_GETLEN
.scope
		lda #1
decrunch_getlen_loop
		DECRUNCH_GETNEXTBIT
		bcc decrunch_getlen_end
		DECRUNCH_GETNEXTBIT
		rol
		bpl decrunch_getlen_loop
decrunch_getlen_end
.endscope
.endmacro

decrunch_getnewbits

dc_get1	ldy $feed,x
		sty dc_bits
		rol dc_bits
		inx
		bne dc_getnewbits_end
dc_getnewbits_inc
		inc dc_get1+2
		inc dc_get2+2
		inc dc_get3+2
dc_getnewbits_end
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
		sty dc_get1+1
		stx dc_get1+2
		sty dc_get2+1
		stx dc_get2+2
		sty dc_get3+1
		stx dc_get3+2

		ldx #0											; get start address
:		jsr decrunch_getnewbits
		sty dc_put-1,x									; x already 1 at this point, so subtract 1
		cpx #2
		bcc :-

		lda #%10000000									; signal that new byte needs fetching
		sta dc_bits

dc_loop	DECRUNCH_GETNEXTBIT
		bcs dc_match

dc_literal
		DECRUNCH_GETLEN									; literal run - get length.
		sta dc_llen+1

		ldy #0
dc_literalloop
dc_get3	lda $feed,x
		inx
		bne :+
		jsr dc_getnewbits_inc
:		sta (dc_put+0),y
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

dc_match
		DECRUNCH_GETLEN									; has to continue with a match - get length.
		sta dc_mlen+1

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
dc_get2	lda $feed,x
		inx
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
