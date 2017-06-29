;these routines use SMC and therefore need to be in RAM
copy_to_ram:
.org ramRoutines
copy_to_ram_start:
drawGbuf:
	ld a,$80
	out ($10),a		;set row ($80: 0, to $BF:63)
	ld hl,gbuf-14+(14*64)	;al pasar tenemos que restar 14x64 (12 columnas en la pantalla, dos fuera/parcialmente fuera)
xOff = $+1
	ld a,0
	cp 8
	jr c,noSkip
	inc hl
noSkip:
yOff = $+1
	ld a,0
	or a
	jr z,noSkip2
	ld b,a
	ld de,14
	add hl,de
	djnz $-4
noSkip2:
	ld a,$20		;$20: col 0, $2E: col 14
	ld c,a
	ld b,64			;64 filas
fastCopyAgain:
	inc c			;avanzar a proxima fila
	push bc
	ld de,-(14*64)
	out ($10),a		;actualizar columna
	add hl,de
	ld de,13
	inc hl
fastCopyLoop:
	add hl,de
	ld a,(hl)		;cargar valor en gbuf a a
rotLeft:
 .db 0,0,0,0			;rotate the values we need
maskLeft = $+1
	and $FF			;necesitamos los valores de la izquierda
 	ld c,a
 	inc hl
 	ld a,(hl)		;el proximo byte que llena el resto del primero
rotRight:
 .db 0,0,0,0
maskRight = $+1
	and 0
	or c
	out ($11),a
	djnz fastCopyLoop
	pop bc
	ld a,c
	cp $2B+1
	jr nz,fastCopyAgain
	ret

callOffpage0:
	exx
	ex af,af'
		pop hl
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		push hl			;the byte after the call
		in a,(6)		;save current flash page
		push af
		ld a,(flashPage)
		out (6),a
		ld hl,offpageReturn
		push hl			;where to return to after running the routine
		push de			;where to jump to
	ex af,af'
	exx
	ret
offpageReturn:
	ld c,a			;save a in case a value was returned in a
	pop af
	out (6),a		;restore the flash page
	ld a,c
	ret	

callOffpage1:
	exx
	ex af,af'
		pop hl
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		push hl			;the byte after the call
		in a,(6)		;save current flash page
		push af
		ld a,(flashPage)
		dec a
		out (6),a
		ld hl,offpageReturn
		push hl			;where to return to after running the routine
		push de			;where to jump to
	ex af,af'
	exx
	ret

callOffpage2:
	exx
	ex af,af'
		pop hl
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		push hl			;the byte after the call
		in a,(6)		;save current flash page
		push af
		ld a,(flashPage)
		dec a
		dec a
		out (6),a
		ld hl,offpageReturn
		push hl			;where to return to after running the routine
		push de			;where to jump to
	ex af,af'
	exx
	ret

copy_to_ram_end:

