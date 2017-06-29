;ix = player sprite
;ix+32 = player sprite mask
;b = x
;e = y
drawMaskedSprite:
	ld h,0
	ld d,h		;ld d,0
	ld c,16		;c=altura de sprite, en pixeles
	ld a,e		;a=y coord
	or a
	 jp p,spriteOnscreen		;saltar si y coord es positivo
	dec c
	add a,c		;si el sprite está fuera de pantalla...
	 ret m		;...no dibujar
	inc a		;porque c tenía que ser 16, pero no quería comprobar si a era negativo o cero
	ld c,a		;16-número de pixeles fuera de la pantalla
	ld a,e		;cargar de nuevo valor y
	neg			;*-1
	add a,a		;x2, porque cada fila del sprite tiene dos bytes
	ld e,a
	add ix,de	;saltar al primer byte del sprite a mostrar
	ld e,h		;e=0, no queremos dibujar el sprite fuera de la pantalla
	 ret m		;si es negativo, está completamente fuera de la pantalla
spriteOnscreen:
; y * 14
	ld l,e		;hl=de
	add	hl, hl	; 2
	add	hl, de	; 3
	add	hl, hl	; 6
	add	hl, de	; 7
	add	hl, hl	; 14

; x / 8
	ld a,b
	srl b
	srl b
	srl b
	ld e,b
	add	hl, de		; A present on a le decalage dans hl
	ld	de,gbuf+2		; Prendre le debut du graphbuffer
	add	hl, de		; Puis ajouter le decalage
	and $07
	ld b,a
;b=xoff
drawMaskedSpriteLoop:
	push bc
	ld d,(ix+32)	;sprite mask
	ld e,(ix+33)
	ld a,$ff
	dec b \ inc b \ jr z,skipMaskRot	;si b=0
	scf \ rr d \ rr e \ rra \ djnz $-6	;rotate mask
skipMaskRot:
	and (hl)
	ld (hl),a
	dec hl
	ld a,(hl)
	and e
	ld (hl),a
	dec hl
	ld a,(hl)
	and d
	ld (hl),a
	pop bc
	inc hl
	inc hl
	push bc
	ld d,(ix)		;sprite
	ld e,(ix+1)
	xor a
	cp b
	jp z,skipSpriteRot
	srl d \ rr e \ rra \ djnz $-5	;rotate sprite
skipSpriteRot:
	or (hl)
	ld (hl),a
	dec hl
	ld a,(hl)
	or e
	ld (hl),a
	dec hl
	ld a,(hl)
	or d
	ld (hl),a
	inc ix
	inc ix
	ld de,16
	add hl,de
	pop bc
	dec c
	jp nz,drawMaskedSpriteLoop
	ret

;l = y
;a = x
;ix = sprite
putMaskedSprite:
	ld b,7
putMaskedSprite_var:
	ld h,0
	ld e,l
	ld d,h		;ld de,hl
	add	hl, hl	;x2
	add	hl, de	;x3
	add	hl, hl	;x6
	add	hl, de	;x7
	add	hl, hl	;x14

	ld e,a		;guardar e en a para sacar el offset x
	srl e
	srl e
	srl e		;/8
	add	hl,de
	ld de,gbuf
	add	hl,de
	and $07		;x offset
	ld c,a
	ld e,b
	ld d,0		;store how many bytes to skip to get the mask
	or a		;si x offset=0, significa que el sprite está alineado
	 jr nz,pMS_NotAlignedLoop
pMS_alignedLoop:
		ld a,(hl)
		and (ix+7)
		xor (ix)
		ld (hl),a		;guardar
		inc ix			;próximo byte
		ld de,14
		add hl,de 		;próxima fila del gbuf
		djnz pMS_alignedLoop
		ret
pMS_NotAlignedLoop:
	push bc			;b = número de iteraciones restantes, c = xoffset
	push de
		ld b,c
		push hl
			push ix
			pop hl
			add hl,de
			ld a,(hl)	;máscara
			sbc hl,de
			ld c,$FF	;dummy byte
			ld d,(hl)
			ld e,0
		pop hl
pMS_rotate:
		scf
		rra			;a = mask
		rr c		;c = overflow
		srl d		;d = sprite byte
		rr e		;e = sprite overflow
		djnz pMS_rotate
;mask = ac
;sprite = de
		and (hl)
		xor d
		ld (hl),a	;primer byte
		inc	hl
		ld a,c
		and (hl)
		xor e
		ld (hl),a	;segundo byte del sprite
		ld de,13
		add	hl,de	;próxima fila
		inc ix
	pop de
	pop	bc
	djnz pMS_NotAlignedLoop
	ret
