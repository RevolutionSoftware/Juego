;organization of bad guys and gals
;x and y on screen	(2) goes to monsterCoords
;name goes into descriptionList
;----------------------
;hp, max and actual (4, 2 bytes each)
;mp, max and actual (4, 2 bytes each)
;strength			(2)
;defense			(2)
;agility			(2)
;intelligence		(2)
;sincerity			(1)
;desperation		(1)
;====TOTAL==========(18)

ATK_ENTRY_SIZE	= 6	;speed (1), action (1), id attacker (1), id attacked (1), damage (2):: size of each entry in battle queue

resetBattleQueue:
	ld hl,battleQueue
	ld de,battleQueue+1
	ld (hl),0
	ld bc,299
	ldir
	ret

startBattle:
;load the battle background
	ld a,GRASS1
	page0(changeMap)	;load the battle background
	xor a
	ld (yCoord),a
	ld (xCoord),a
	ld (xOff),a

	ld bc,$1460			;h = 20, w = 96
	ld de,44			;x = 0, y = 44
boxLoop:
	push bc
	push de
;		page0(drawBox)
;		call drawGbuf
;		page0(drawMap)
	pop de
	pop bc
	inc e
	inc e
	dec b
	dec b
	ld a,b
	cp 4
	 jr nz,boxLoop
	call drawFullField
	call drawBattleMenu
	call drawGbuf

battleKeyLoop:
	ld a,$FE		;check the arrows
	out (1),a
	push af
	pop af
	in a,(1)
	rra
	 jr nc,chooseDown
	ld de,%0011111111110000
	rra
	 jr nc,chooseLeft
	rra
	 jr nc,chooseRight
	rra
	 jr nc,chooseUp
	jr battleKeyLoop

chooseDown:
	ld de,%0000001111111111
	ld hl,gbuf+(42*14)+5
	call drawBattleCursor
	call drawBattleCursor
	jr battleKeyLoop
chooseLeft:
	ld hl,gbuf+(30*14)+4
	call drawBattleCursor
	call drawBattleCursor
	jr battleKeyLoop
chooseRight:
	ld hl,gbuf+(30*14)+7
	call drawBattleCursor
	call drawBattleCursor
	jr battleKeyLoop
chooseUp:
	ld de,%0000001111111111
	ld hl,gbuf+(18*14)+5
	call drawBattleCursor
	call drawBattleCursor

	ld a,20
	ld (scratchSpace),a
battleAttack:
	call drawFullField
	ld l,10				;y
	ld a,11				;x
	ld ix,attackBarLeft
	ld b,11				;height
	call putMaskedSprite_var

	ld a,(scratchSpace)
	add a,11+5			;x

	ld l,10				;y
	ld ix,attackBarRight
	ld b,11				;height
	call putMaskedSprite_var


	ld a,(scratchSpace)
	ld b,a
	ld a,9
	ld hl,gbuf+(11*14)+2
clearBarMiddle:
	push af
		ld c,0
		call fillBar
	pop af
	ld de,14
	add hl,de
	dec a
	 jr nz,clearBarMiddle

	ld c,%10101010
	ld hl,gbuf+(12*14)+2
	call fillBar
	ld hl,gbuf+(18*14)+2
	call fillBar
	call drawGbuf

keyz:
	ld a,$FE
	out (1),a
	in a,(1)
	inc a
	 jr z,keyz
	dec a
	ld hl,scratchSpace
	rra
	rra
	 jr c,$+3
		dec (hl)
	rra
	 jr c,battleAttack
		inc (hl)
	jr battleAttack
	jr $

;hl = where to draw
;c = pattern
fillBar:
	push bc
	push hl
	ld a,%01111111		;the mask
fillBarLoop:
	push af
		and (hl)
		ld (hl),a
	pop af
	push af
		cpl
		and c
		or (hl)
		ld (hl),a	
	pop af
	rrca
	 jr c,$+3
		inc hl
	djnz fillBarLoop
	pop hl
	pop bc
	ret
	
drawBattleCursor:
	push hl
		ld b,10
drawBattleCursorLoop:
		ld a,(hl)
		xor d
		ld (hl),a
		inc hl
		ld a,(hl)
		xor e
		ld (hl),a
		push de
			ld de,13
			add hl,de
		pop de
		djnz drawBattleCursorLoop
		push de
			call drawGbuf
		pop de
	pop hl
	xor a
	out (1),a
	nop
	in a,(1)
	inc a
	 jr nz,$-3
	ret

drawBattleMenu:
	ld ix,battleMenu
	ld hl,battleMenuMask
	ld de,gbuf+(16*14)+4
	ld bc,$0526
drawBMLoopOut:
	push bc
drawBMLoopIn
		ld a,(de)
		and (hl)
		or (ix)
		ld (de),a
		inc hl
		inc ix
		inc de
		djnz drawBMLoopIn
		ex de,hl
		ld bc,9
		add hl,bc
		ex de,hl
	pop bc
	dec c
	 jr nz,drawBMLoopOut
	ret

;draw map and combattants
drawFullField:
	page0(drawMap)
drawCombattants:
	ld hl,battleMap
	ld d,0			;y position
	ld c,3
drawCombattantsOuter:
	ld e,0			;x position
	ld b,6
drawCombattantsLoop:
	ld a,(hl)
	inc hl
	cp $FF
	 call nz,drawCombattant
	inc e			;x+1
	djnz drawCombattantsLoop
	inc d
	dec c
	 jr nz,drawCombattantsOuter
	ret

drawCombattant:
	push hl
	push de
	push bc
;check if player or enemy
;	bit 7,a
		ld l,a
		ld h,0
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl	;x64
		ld bc,battleCharacterSprites
		add hl,bc
		push hl
		pop ix
		ex de,hl
		add hl,hl	;x2
		add hl,hl	;x4
		add hl,hl	;x8
		add hl,hl	;x16
		ld b,l
		ld a,h
		add a,8
		ld e,a
		call drawMaskedSprite
	pop bc
	pop de
	pop hl
	ret

battleMap:
.db $FF,$FF,$FF,$FF,$00,$FF
.db $FF,$FF,$FF,$FF,$FF,$01
.db $FF,$FF,$FF,$FF,$02,$FF

;entrada: c = número que sustraer: 43=1-6, 51=1-5, 26=1-10, etc. (255/##)
;salida: b = número aleatorio
randomNumber:
	ld a,(seed)
	ld b,a		;the "seed"
	ld a,r		;our random number base
	add a,b
	inc a		;just playing with the number
	ld (seed),a	;update the seed
	ld b,0
	 inc b		;inc a every time we subtract 43 from a. (0-42=1, 43-85=2, 85-127=3, etc.)
	 sub c		;Basically, we are dividing the number by 6 and storing the result in b.
	jr nc,$-2	;repeat until
	ret
