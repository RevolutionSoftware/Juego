;#############################################################################################
;#					SHAPES AND DRAWING

;b = height
;c = width
;d = starting x
;e = starting y
drawBox:
	dec b
	dec b
	dec b
	dec b
	ld a,(yOff)
	add a,e
	ld e,a
	ld a,(xOff)
	add a,d
	ld d,a
	and $7
	ex af,af'
		ld a,d
		ld l,e
		ld h,0
		ld d,h
		add hl,hl
		add hl,de
		add hl,hl
		add hl,de
		add hl,hl		;y*14
		rra
		rra
		rra
		and %00011111
		ld e,a
		add hl,de		;+x
		ld de,gbuf
		add hl,de		;position in gbuf
	ex af,af'
	ld de,%1111111011111111
	ld (scratchSpace),de
	ld e,%01111111
	call drawLine
	ld de,%1111111111111111
	ld (scratchSpace),de
	call drawLine
middleLoop:
	ld de,%0000000100000000
	ld (scratchSpace),de
	ld e,%10000000
	call drawLine
	djnz middleLoop

	ld de,%1111111111111111
	ld (scratchSpace),de
	call drawLine
	ld de,%1111111011111111
	ld (scratchSpace),de
	ld e,%01111111
drawLine:
	push bc
	push af
		ld b,a
		or a
		ld a,$FF
		 jr z,$+9		;check if aligned
		 	inc c
			srl a
			srl e
			djnz $-5
		cpl
		and (hl)
		or e
		ld (hl),a		;store in gbuf
		inc hl
		ld a,(scratchSpace)
		ld e,a			;e = the pattern to draw in the middle of the line
		ld a,c			;width+xOff
		sub 16			;-16 for the first two bytes in the line
		 jr c,exitLine
lineLoop:
		ld (hl),e		;draw 8 pixels at a time until we've got
		inc hl			; fewer than 8 pixels to draw
		sub 8
		 jr nc,lineLoop
exitLine:
		neg
		cp 8
		ld b,a
		ld a,(scratchSpace+1)
		 jr nz,unAlined
			dec hl
			ld (hl),a
			inc hl
			jr nextRow
unAlined:				;)
			ld e,a
			ld a,$FF
			sla a
			sla e
			 djnz $-4
			cpl
			and (hl)	;a holds the mask
			or e
			ld (hl),a
nextRow:
		ld a,c
		rra
		rra
		rra
		and %00011111
		ld e,a
		ld a,14
		sub e
		ld e,a
		ld d,0
		add hl,de		;+x
	pop af
	pop bc
	ret

;#############################################################################################
;#					MATH

;multiplica E*H y guarda el resultado en HL
multEH:
	ld l,0
	ld d,l
	ld b,8
multLoop:               ;hl=e*h
	add hl,hl
	jr nc,noAdd
	add hl,de
noAdd:
	djnz multLoop
	ret
