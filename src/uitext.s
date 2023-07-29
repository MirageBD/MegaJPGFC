.segment "TEXT"

uitxt_save			.byte "save", 0

fa1directorytxt		.byte 0, "                              "

.align 256

fa1boxtxt			.word fa1boxtxt00
					.word $ffff

					.repeat 512
					.byte 0
					.endrepeat

.align 256			; leave enough room for fa1boxtxt to grow. 256 directory entries allowed

fa1boxtxt00			.byte %00010000, $31, $03, "",            0
