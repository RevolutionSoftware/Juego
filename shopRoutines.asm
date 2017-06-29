openShop:
	call drawShop
	ld hl,howManyTxt
;a=height of box
;b=y coord of box (starting at 1, not 0), b = 0 if no box
;c=x coord of box (in bytes, not pixels)
;e=width of box
	ld a,10
	ld bc,$1004
	ld e,5
;	call dibTextoCuadro
;	call wait2nd	
	jr $

howManyTxt:
.db "How many?",$FF

drawShop:
	ld a,(hl)
	add a,a
	ld c,a
	ld b,0
	ld hl,shopList
	add hl,bc
	getHL()			;hl points to shop data now
	push hl
;		call dibTextoA	;draw the shop introduction text
		ld hl,itemsInMenu
		ld (hl),0	;reset items in menu
	pop hl
	inc hl
	inc hl			;skip the text label
	ld de,menuList
	exx
		ld de,priceList
	exx
getShopItems:
	ld a,(hl)		;item id
	inc a			;check if end of item list ($FF)
;	 jr z,drawMenu
;count how many items are in the store
	ld a,(itemsInMenu)
	inc a
	ld (itemsInMenu),a
;	ld a,T_MENUSHOP	;special menu
	ld (de),a
	inc de
;	ld a,T_JOIN
	ld (de),a
	inc de
	push hl
		ld a,(hl)
		add a,a
		add a,a			;x4	2 bytes coste, 2 bytes texto
		ld c,a
		ld b,0
		ld hl,itemList
		add hl,bc		;pointer to item data
		push hl
		exx
			pop hl
			ldi
			ldi
		exx
		inc hl
		inc hl
		ldi
		ldi				;load text label into de (description list)
	pop hl
;	ld a,T_EOS
	ld (de),a
	inc de
	inc hl
	jr getShopItems

;drawMenu:
	call clearGbuf
	res scrollMenu,(iy+textActions)
	ld hl,menuList
	xor a				;start off at the first item
	ld (menuCounter),a	;saveHL will be used as a counter to know where we are
drawMenuOuterLoop:
	ld a,3
	ld (penRow),a
	ld a,1
	ld (penCol),a
	ld a,46
	ld b,1
;	call dibLineaSinPausa

	ld a,(itemsInMenu)
	dec a
	jr z,dIL_end
	ld b,a
	cp 6
	jr c,drawItemsLoop
	 set scrollMenu,(iy+textActions)
	 ld b,6
drawItemsLoop:
	push bc
		ld b,0
;		call dibLineaSinPausa	;draw a line of text with newline afterward
	pop bc
	djnz drawItemsLoop
dIL_end:
;	call wait2nd
;returning from loop, check if we need to scroll. a=curOption
	ld hl,menuCounter
	ld d,0
;	bit scrollDown,(iy+textActions)
	jr z,noBajar
;		ld (curOption),a
		add a,(hl)
		inc a
		ld b,a
		ld a,(itemsInMenu)
		cp b
		jr z,returnToMenuLoop
		inc (hl)
		jr returnToMenuLoop
noBajar:
;	bit scrollUp,(iy+textActions)
	jr z,noSubir
		ld a,(hl)
		or a
		jr z,returnToMenuLoop
		dec (hl)
returnToMenuLoop:
	ld a,(hl)
	add a,a
	add a,a
	add a,(hl)			;x5
	ld e,a
	ld hl,menuList
	add hl,de
	jr drawMenuOuterLoop
noSubir:
;calcular elemento correcto
	ld (iy+textActions),0
	ret

;where to find the list of items each shop has
shopList:
.dw itemsBookshop
.dw itemsItemshop

;items found at each shop
itemsBookshop:
.dw shopBookshop	;shop introduction
.db BREAD
.db DRIEDFRUIT
.db PIE
.db WARMWATER
.db TEA
.db HERB
.db MEDICINE
.db FLOWER
.db DRIEDFRUIT
.db TEA
.db HERB
.db FLOWER
.db PIE
.db DRIEDFRUIT
.db WARMWATER
.db $FF

itemsItemshop:
.dw shopItemshop
.db BREAD
.db PIE
.db TEA
.db HERB
.db MEDICINE
.db FLOWER
.db $FF
