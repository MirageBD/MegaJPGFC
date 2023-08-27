; -----------------------------------------------------------------------------------------------

; call: Y = AddrLo
;       X = AddrHi

zp_base	= $02			; -
bits	= zp_base		; 1
put		= zp_base+2		; 2

; -----------------------------------------------------------------------------------------------

.macro DECRUNCH_GETNEXTBIT
.scope
		asl bits
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
get1	ldy $feed,x
		sty bits
		rol bits
		inx
		bne gnbend
gnbinc	inc get1+2
		inc get2+2
		inc get3+2
gnbend	rts

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
		sty get1+1
		stx get1+2
		sty get2+1
		stx get2+2
		sty get3+1
		stx get3+2

		ldx #0											; get start address
:		jsr decrunch_getnewbits
		sty put-1,x										; x already 1 at this point, so subtract 1
		cpx #2
		bcc :-

		lda #%10000000									; signal that new byte needs fetching
		sta bits

dloop	DECRUNCH_GETNEXTBIT
		bcs match

literal	DECRUNCH_GETLEN									; literal run - get length.
		sta llen+1

		ldy #0
lloop
get3	lda $feed,x
		inx
		bne :+
		jsr gnbinc
:		sta (put+0),y
		iny
llen	cpy #0
		bne lloop

		clc
		tya
		adc put+0
		sta put+0
		bcc :+
		inc put+1

:		iny
		beq dloop

match	DECRUNCH_GETLEN									; has to continue with a match - get length.
		sta mlen+1

		cmp #$ff										; length 255 -> EOF
		beq end

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
		bmi mshort
:		eor #$ff										; get byte
		tay
get2	lda $feed,x
		inx
		bne :+
		jsr gnbinc
:		jmp mdone

mshort	ldy #$ff

mdone	;clc
		adc put+0
		sta mlda+1
		tya
		adc put+1
		sta mlda+2

		ldy #$ff
mloop	iny
mlda	lda $beef,y
		sta (put+0),y
mlen	cpy #0
		bne mloop

		;sec
		tya
		adc put+0
		sta put+0
		bcc :+
		inc put+1

:		jmp dloop

end		rts

; -----------------------------------------------------------------------------------------------
