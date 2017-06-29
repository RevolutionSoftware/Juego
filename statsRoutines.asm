startMenu:
	call drawMap		; erase the screen [drawMap.asm]
	call drawPlayer		; player was erased in the call above
	xor a				; default option selected
	ld hl,startMenuTxt
	call drawMenu
	call menuGetkey
	jp main				;if they cancel out of the menu with [Alpha], go back to the main loop

dispStats:
	call clearGbuf
	ld bc,$4060			;dimensions
	ld de,$0000			;coords
	ld hl,statsMenuTxt	;txt
	call drawTextBox
	call drawGbuf
	call pause
	jr startMenu

dispItems:
	ld hl,playerInventory
	ld a,(hl)
	or a
	 jr z,noItems
	exx
		ld hl,itemMenuTxt
		ld (hl),a		;# of items
		inc hl
	exx
	ld b,a
	ld ix,subMenu1Txt
createItemListLoop:
	inc hl				;points to first item ID
	ld e,(hl)
	inc hl				;item amount
	ld d,(hl)
	push de
	exx
		pop de
		ld (hl),MN
		inc hl
		ld (hl),ITEM
		inc hl
		ld (hl),e
		inc hl
		ld (hl),NEWX
		inc hl
		ld (hl),82
		inc hl
		ld (hl),NUM8
		inc hl
		ld (hl),d
		inc hl
		ld (hl),NEWL
		inc hl			;.db MN,ITEM,[ID],NEWX,70,NUM8,[#],NEWL
	exx
	ld (ix),ITEMD
	inc ix
	ld (ix),e
	inc ix
	ld (ix),0
	inc ix				;.db ITEMD,[ID],0
	djnz createItemListLoop
;zero terminate the string
	xor a
	exx
		dec hl
		ld (hl),a		;this overwrites the last NEWL with a 0
;just ignore shadows
	ld hl,itemMenuTxt
	ld de,$0000			;y,x
	ld bc,$2E60			;width,height
	ld a,5				;how many items to be drawn at a time
	call drawMenuScroll

	ld de,$002C			;e=y,d=x
	ld bc,$1460			;width,height
	call installSub1
	call menuGetkey
	jp startMenu

noItems:
	ld hl,noItemsTxt
	call drawDialog
	jp startMenu
