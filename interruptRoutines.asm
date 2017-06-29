clearGbuf:
;textBuf
	ld hl,gbuf
	ld de,gbuf+1
	ld (hl),0
	ld bc,14*80
	ldir
	ret
