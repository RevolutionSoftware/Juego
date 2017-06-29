;vputs = displays a string hl at coordinates de
;drawDialog = displays hl in a dialog box at the bottom of the screen
;drawMenu = displays menu hl using coordinates built into the menu
;drawMenuScroll = displays a menu that can scroll up/down at coordinates c,b
;menuGetkey = handles the cursor for drawMenu

;special characters
MN		= $FF
NEWX	= $FE
NEWXY	= $FD
NEWL	= $FC
BRK		= $FB
PERS	= $FA	;load person to disply their stats
ITEM	= $F9	;display the name of an item based on item's ID
ITEMD	= $F8	;display item's description based on ID
NUM		= $F7	;display a 3-byte number (starting with LSB, so $123456 would be stored as $56,$34,$12)
NUM8	= $F6	;display a 1-byte number
ATK		= $F5	;display attack name (ID)
MAG		= $F4	;display magic name (ID)
STAT	= $F3	;mark that you specifically want to insert a stat (for stat values >= $20 aka 32)

;stats
;NAME	= 0	;name
;CURHP	= 0	;hp
;MAXHP	= 2	;
;CURMP	= 4	;mp
;MAXMP	= 6	;
;STR	= 8	;strength
;DEF	= 10;defense
;AGI	= 12;agility/speed/dodging
;INT	= 14;intelligence/magic
;CNC	= 16;spirit (0-99)
;SNC	= 17;sincerity (luck) (0-99)
;DSP	= 18;desperation (critical) (0-99)

;bc = dimensions (c = width,b = height)
;de = coordinates (e = y, d = x)
;hl = string
drawTextBox:
	push hl
	push de
		xor a
		ld (textDelay),a
		call drawBox
	pop de
	pop hl
	ld a,e
	ld e,d
	ld d,a
	inc e
	inc e
	inc d
	inc d
	inc d
	jp vputs

;hl = text to draw
drawDialog_:
	getHL()
	call drawDialog
	jp main
drawDialog:
	push hl
		ld a,1
		ld (textDelay),a
		ld bc,$1460			;h = 20, w = 96
		ld de,44			;x = 0, y = 44
		call drawBox
	pop hl
	ld de,$2F02
	jp vputs

;a = default
drawMenu:
	res returnMenu,(iy+textActions)
	res scrollMenu,(iy+textActions)
	call setupMenu
	ld (optionList),hl		;table of routines to jump to once a choice has been selected
	add a,a
	ld c,a
	ld b,0
	add hl,bc				;skip to coordinates
	ld de,menuBoxY			;copy the coordinates and size of the text box around the menu
	ld c,4
	ldir
	ld de,(menuBoxY)
	ld a,d
	inc a
	inc a
	ld d,e
	inc d
	inc d
	inc d
	inc d
	ld e,a
	jr boxputs

;****define box to draw***
;** e = y, d = x
;** c = width, b = height
drawMenuScroll:
	ld (maxItems),a
	set returnMenu,(iy+textActions)
	set scrollMenu,(iy+textActions)
	ld (menuBoxY),de
	ld (menuBoxWidth),bc
	xor a
	call setupMenu
	ld (textStart),hl
	ld de,(menuBoxY)
	ld a,d
	inc a
	inc a
	ld d,e
	inc d
	inc d
	inc d
	inc d
	ld e,a
	ld (textCoords),de		;save coordinates and start of text data
	jr boxputs

;de = y,x
;bc = width,height
installSub1:
	ld hl,numSubMenus
	ld (hl),1
	ld (subMenu1Y),de
	ld (subMenu1Width),bc
	ret

;de = y,x
;bc = width,height
installSub2:
	ld hl,numSubMenus
	ld (hl),2
	ld (subMenu2Y),de
	ld (subMenu2Width),bc
	ret

setupMenu:
	ld (menuCursor),a
	xor a
	ld (numSubMenus),a
	ld (textDelay),a
	ld (textOffset),a		;for scrolling menus, we need to know how many items we are from the first item
	ld (menuItems),a		;reset menuItems to 0
	ld a,(hl)
	inc hl
	ld ix,menuCoordinates+1
	ld (ix-1),a				;number of options in the menu
	ret

boxputs:
	push de
	push hl
		ld bc,(menuBoxWidth)
		ld de,(menuBoxY)
		call drawBox
	pop hl
	pop de
	xor a
	ld (menuItems),a		;reset menuItems to 0
;hl = text
;de = coordinates (e = x, d = y)
vputs:
	ld (penCol),de
	ld a,e
	ld (defCol),a
vputsLoop:
	ld a,(hl)				; grab the next character
	or a					; quit if we're at the end of the string (EOF = 0)
	 ret z
	cp STAT					; last special character
	 jr c,notSpecial		; if it isn't a special character
		ld bc,vputsLoop		; put vputsLoop on the stack so that we jump there with ret
		push bc
	 	push hl				; hl = current byte in string
			ld hl,txtTable	; table of text actions
			cpl				; this gives us the offset to jump
			add a,a			; *2
			ld c,a
			ld b,0			; bc = offset in table (each entry = 2 bytes)
			add hl,bc		; hl = position in table
			ld c,(hl)
			inc hl
			ld b,(hl)		; bc = address to jump to
		pop hl				; hl = address in the string
		inc hl				; hl points to parameters
		push bc				; bc = routine adress where we want to jump
		ret					; **don't forget that vputsLoop is on the stack**
notSpecial:
	cp $20
	 jr nz,notSpace
		ld a,(penCol)	;space
		inc a
		ld (penCol),a
		jr endVputs
notSpace:
	cp _DSP+1			;if a <= DSP, we're drawing a stat
	 jr c,insertStat
	call vputc
endVputs:
	inc hl
	ld a,(textDelay)	;= 0 si pas de delai, != 0 s'il y en a
	or a
	call nz,textWait
	jr vputsLoop

textWait:
	di
	exx
	ld a,$df	;check ALPHA to skip text typing
	call getKey
	bit 7,a
	 jr nz,$+6
		xor a
		ld (textDelay),a
	ld b,7
	push bc
		call drawGbuf
	pop bc
	djnz $-5
	exx
	ret

;--------
;insertStat:
;	insert player's stat in a string
;--------
; hl =	position in string
; a  =	stat to check
insertStat:
	inc hl					;continue with the rest of the string
	push hl
		cp _NAME			; [juego.asm]
		 jr z,insertName
		ld hl,playerData-3	; -2 for name, -1 because we start at 1 not 0 (0 = EOS)
		ld e,a				; de = stat id
		ld d,0
		add hl,de			; hl = offset in player's data
		ld e,(hl)
		inc hl
		ld h,(hl)
		ld l,e				; hl = stat's value
		cp _END				; all stats after endurance are 1 byte
		 jr c,$+4
			ld h,0			;if we are drawing END, CNC, SNC, or DSP, only display 1 byte
		call numberToString
		call vputsLoop		;draw the number
	pop hl
	jr vputsLoop

insertName:
		ld hl,playerNamesList
		getHL()
		call vputsLoop
	pop hl
	jp vputsLoop

;############################################# SPECIAL TEXT ROUTINES ###########################################
txtTable:
	.dw txtAddMenu
	.dw txtNewX
	.dw txtNewXY
	.dw txtNewLine
	.dw txtBreak
	.dw txtLoadPerson
	.dw txtDisplayItem	; item name
	.dw txtDisplayItemD	; item description
	.dw txtNumber
	.dw txtNumber8
	.dw 0000			;attack name
	.dw 0000			;magic name
	.dw txtStat

txtAddMenu:
	ld de,menuItems		;(menuItems)+1
	ex de,hl
		inc (hl)
	ex de,hl
	ld de,(penCol)
	ld (ix),e			;coordinate X
	ld (ix+1),d			;coord Y
	ld a,e
	add a,9
	ld (penCol),a
	inc ix				;skip ahead to the next set of empty coordinates
	inc ix
	ret
txtNewX:
	ld de,penCol		;hl points to the X value to load
	ldi					;copy the X into penCol
	ret
txtNewXY:
	ld a,(hl)
	ld (defCol),a		;update default column with the new X value
	ld de,penCol
	ldi
	ldi
	ret
txtNewLine:
	ld a,(defCol)
	ld de,penCol
	ld (de),a
	inc de
	ld a,(de)
	add a,7
	ld (de),a
	cp 58
	 ret c				;if we're gonna draw off-screen, quit
	pop hl
	ret
txtBreak:
	pop bc			;clear stack the stack
	push hl
		ld hl,cursorWait
		ld (hl),50
		jr flipCursor
flipCursorWait:
		ei
		halt
		di
		ld a,$bf	;check 2nd
		call getKey
		bit 5,a
		 jr z,loadNextText
		ld hl,cursorWait
		dec (hl)
		 jr nz,flipCursorWait
		ld (hl),50
flipCursor:
		ld a,90
		ld e,56
		push ix					;ix contains menuCoordinates
			ld ix,cursor_sprite
			ld b,3
			call putSprite_var
		pop ix
		call drawGbuf
		jr flipCursorWait
loadNextText:
		ld hl,textDelay			;si on a poussé une touche pour faire avancer plus rapidement le texte
		inc (hl)				; il faut remettre le texte pour qu'il s'affiche à la vitesse normale
		in a,(1)
		inc a
		 jr nz,$-3				;il faut relâcher la touche avant de continuer
	pop hl
	jp drawDialog

txtLoadPerson:

;----------------
;Display the item's name
;----------------
; hl =	argument position in string
;		points to item id
;----------------
txtDisplayItem:
	push hl
		ld l,(hl)
		ld h,0
		add hl,hl
		add hl,hl
		ld de,itemList+2		; first two bytes are the item price
		add hl,de
		getHL()
		call vputsLoop
	pop hl
	inc hl
	ret

;----------------
;Display the item's description
;----------------
; hl =	argument position in string
;		points to item id
;----------------
txtDisplayItemD:
	push hl						; we need to pass hl back to vputsloop
		ld l,(hl)				; item ID
		ld h,0
		add hl,hl
		add hl,hl				; x4
		ld de,itemList+2		; first two bytes hold the item's price
		add hl,de
		getHL()					; HL = string address
		xor a					; item format is "name",0,"description",0
		ld bc,100				; .. so we search for a 0
		cpir					; .. and go to the next byte
		call vputsLoop			; add this text to the stack
	pop hl
	inc hl						; skip the item id
	ret

txtNumber:
;----------------
;show an 8-bit number
;----------------
; hl =	pointer to number
;----------------
txtNumber8:
	push hl
		ld l,(hl)
		ld h,0
		call numberToString
		call vputsLoop
	pop hl
	inc hl
	ret

;----------------
;txtStat:
;	
;----------------
; hl =	pointer to stat id
;----------------
txtStat:
	ld a,(hl)
	cp _DSP+1
	 jp c,insertStat
	inc hl					;continue with the rest of the string
	push hl
		ld hl,playerData-3	;-2 for name, -1 because we start at 1 not 0 (0 = EOS)
		ld e,a
		ld d,0
		add hl,de
		ld e,(hl)
		cp _LVL
		 jr z,insertLvl
		inc hl
		ld d,(hl)
		inc hl
		ld a,(hl)
		ex de,hl
		call bigNumberToString
		jr skipInsertLvl
insertLvl:
		ld l,e
		ld h,0			;if we are drawing END, CNC, SNC, or DSP, only display 1 byte
		call numberToString
skipInsertLvl:
		call vputsLoop		;draw the number
	pop hl
	ret


;a = group to check
getKey:
	out (1),a
	push af
	pop af
	in a,(1)
	ret

;########################
;#VPUTC
;# Draw a letter to the text buffer
;#input: a = letter to draw
;########################
vputc:
	push hl
	ld d,0
	ld hl,width5		;[alphabet.inc]
	ld bc,width3-width5
	cpir
	ld e,6
	 jr z,vputc_updatepenCol
	ld c,width2-width3
	cpir
	ld e,4
	 jr z,vputc_updatepenCol
	ld c,width_end-width2
	cpir
	ld e,3
	 jr z,vputc_updatepenCol
	ld e,5
vputc_updatepenCol:
	ld bc,(penCol)
	ex de,hl
	add hl,bc
	ld (penCol),hl
	ex de,hl
	push af
;compensate y coordinate
		ld a,(yOff)
		add a,b			;starting y
		ld b,a
;compensate x coordinate
		ld a,(xOff)
		add a,c			;starting x
		ld c,a
	pop af
	push bc
		ld hl,dropTable
		ld bc,6
		cpir
	pop bc
	 jr nz,$+3
		inc b
;find letter's sprite
	exx
		ld e,a
		ld d,0
		ld l,e
		ld h,d
		add hl,hl		;x2
		add hl,de		;x3
		add hl,hl		;x6
		ld de,alfabeto-($21*6)
		add hl,de
	exx
	ld e,b			;put penRow into e
	ld l,e			;hl = penRow
	ld h,0
	ld d,h
	ld b,h
	add hl,hl		;y*2
	add hl,de		;y*3
	add hl,hl		;y*6
	add hl,de		;y*7
	add hl,hl		;y*14
	ld de,gbuf
	add hl,de
	ld a,c
	and $7			;offset
	srl c
	srl c
	srl c			;/8
	add hl,bc		;hl = starting point in gbuf
	ld b,a			;b = how x offset (0-7)
	ld c,6			;height of letters
;hl' = sprite address
;hl = gbuf
drawLetterLoop:
	exx
		ld a,(hl)	;sprite
		inc hl
	exx
	ld e,b					;guardar b
	ld d,0
	dec b \ inc b			;offset = 0?
	 jr z,dLL_noclip
	rra \ rr d \ djnz $-3	;rotate sprite
	ld b,e
dLL_noclip:
	or (hl)
	ld (hl),a
	inc hl
	ld a,d
	or (hl)
	ld (hl),a
	ld de,13
	add hl,de
	dec c
	jr nz, drawLetterLoop
	pop hl
	ret

;############################################### MENU STUFF ####################################################
menuGetkey:
	call checkSubMenu
	ld hl,animationCounter
	inc (hl)
	call drawCursor
	ei
	halt				;a little delay, and save some power
	di
	ld a,$DF			;check ALPHA
	call getKey
	rla
	 ret nc
	ld a,$BF			;check 2nd ($bf = key group #7 : Y=, WINDOW, ZOOM, TRACE, GRAPH, 2nd, MODE, DEL)
	call getKey
	cp 223				;2nd
	 jp z,menuSelect
;*************************
	ld hl,menuGetkey	;*
	push hl				;* where to jump
;*************************
	ld a,$FE			;check arrows
	call getKey
	or $F0				;erase bits 4-7
	ld b,a
	inc b				;if b+1 = 0, b = $FF then no arrows have been pushed
	ld hl,cursorWait	;le compteur/delai
	 jr nz,$+4 			;sauter si une touche a été poussée
		ld (hl),b		;remettre le compteur à zéro
		ret
	dec (hl)			;compteur-1
	 ret p				;quitter si positif (>=0)
	ld (hl),8			;speed of cursor
;which arrow ?
	ld hl,menuCursor
	bit scrollMenu,(iy+textActions)
	 jr z,checkMenuKeys	;if not set, check the normal keys
checkScrollKeys:
	rra
	 jr nc,scrollDown
	bit 2,a
	 jr z,scrollUp
	ret
;textOffset+menuCursor < menuCoordinates
scrollDown:
	ld a,(hl)
	inc a					;because menuCursor starts at 0, whereas menuCoords starts at 1
	ld hl,menuCoordinates
	cp (hl)					;if at the last menu item of all the items
	 ret z
;check if we need to scroll down
	ld (menuCursor),a
	ld hl,textOffset
	sub (hl)
	ld hl,maxItems
	sub (hl)				;if menuCursor-textOffset = menuItems, scroll menu down
	 ret nz					;if menuItems+textOffset != menuCursor, quit
	pop hl					;clear the stack
	ld hl,(textStart)
		inc hl
		ld a,(hl)
		cp MN
	 jr nz,$-4
	ld (textStart),hl		;move to the next menu item.
	ld a,(textOffset)
	inc a
	ld (textOffset),a
	ld de,(textCoords)
	jp boxputs
scrollUp:
	ld b,(hl)
	dec b
	 ret m					;if we're at the first menu item, quit
	ld a,(textOffset)
	sub (hl)				;if menuCursor = textOffset, we need to scroll
	ld (hl),b				;store menuCursor-1
	 ret nz
	ld hl,(textStart)
		dec hl
		ld a,(hl)
		cp MN
	 jr nz,$-4
	ld (textStart),hl		;find the previous menu item.
	ld a,(textOffset)
	dec a
	ld (textOffset),a
	ld de,(textCoords)
	jp boxputs
checkMenuKeys:
	rra
	 jr nc,cursorDown
	rra
	 jr nc,cursorLeft
	rra
	 jr nc,cursorRight
cursorUp:
	ld a,(hl)
	or a
	ret z
	ld b,a					;actual menuCursor
	call getCursorCoords	;DE isn't destroyed
	ld a,(hl)				;X coordinate
findNextUp:
	dec b
	ret m
	dec hl
	dec hl
	cp (hl)
	jr z,foundUp
	jr findNextUp
foundUp:
	ld a,b
	ld (menuCursor),a
	ret

cursorDown:
	ld a,(menuCoordinates)	;# of coordinates
	dec a
	cp (hl)
	 ret z					;can't scroll down if we're at last coordinate
	ld c,(hl)				;menuCursor actuel
	sub c					;nombre d'options - cursor actuel
	 ret z
	ld b,a					;nombre de coordonnées à chercher
	call getCursorCoords	;ne modifie pas la valeur de DE
	ld a,(hl)				;coordonnée X
findNextDown:
	inc c
	inc hl
	inc hl
	cp (hl)
	jr z,foundDown
	djnz findNextDown
	ret
foundDown:
	ld a,c
	ld (menuCursor),a
	ret

cursorLeft:
;	ld a,(sub_menu_flag)
;	cp 2
;	 jr nz,move_cursor_left
;sub_menu_left = $+1
;		ld hl,$0000
;		ld (sub_menu_addr),hl
;		ret
moveCursorLeft:
	ld a,(hl)
	or a
	ret z
	dec (hl)
	ret

cursorRight:
;	ld a,(sub_menu_flag)
;	cp 2
;	 jr nz,moveCursorRight
;sub_menu_right = $+1		;if there is a special submenu, pressing right will load a second menu
;	ld hl,$0000
;	ld (sub_menu_addr),hl
;	ret
moveCursorRight:
	ld a,(menuCoordinates)	;# de coordinées
	dec a
	cp (hl)
	ret z
	inc (hl)
	ret

getCursorCoords:
	ld a,(menuCursor)
	add a,a
	ld e,a
	ld d,0
	ld hl,menuCoordinates+1
	add hl,de
	ret

menuSelect:
	xor a
	call getKey			;il faut relacher la touche d'abord
	inc a
	 jr nz,menuSelect
	ld a,(menuCursor)
	bit returnMenu,(iy+textActions)
	 ret nz
	pop hl				;nettoyer la pile (le call)
	add a,a
	ld c,a
	ld b,0
	ld hl,(optionList)
	add hl,bc
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	jp (hl)

drawCursor:
	ld hl,textOffset
	ld a,(menuCursor)	;cargar primera opción
	sub (hl)
	add a,a				;x2 cada entrada ocupa 2 bytes
	ld e,a
	ld d,0
	ld hl,menuCoordinates+1
	add hl,de			;load the coordinates from the table
	ld b,(hl)			;x coord
	ld a,(animationCounter)
	and %00110000		;little poking animation
	 jr nz,$+3
	  inc b
	inc hl				;avanzar a y
;compensate y coordinate
	ld a,(yOff)
	add a,(hl)			;
	ld l,a				;cargar y en l
;compensate x coordinate
	ld a,(xOff)
	add a,b				;starting x
	ld ix,cursor
	call putSpriteMasked
	call drawGbuf
	ld hl,cursorGbufSave
	ld de,(cursorGbufLoc)
	ld a,7
	jp restoreGbuf

checkSubMenu:
	ld a,(numSubMenus)
	or a
	 ret z
	ld hl,subMenu2Txt
	ld de,(menuCursor)
	ld d,0
	add hl,de
	add hl,de
	add hl,de
	push de
		ld de,(subMenu2Y)
		ld bc,(subMenu2Width)
		dec a
		call nz,drawTextBox		;if a > 1, then we need to draw both text boxes
	pop de
	ld hl,subMenu1Txt
	add hl,de
	add hl,de
	add hl,de
	ld de,(subMenu1Y)
	ld bc,(subMenu1Width)
	jp drawTextBox

;############################################# NUMBER CONVERSION ##############################################

;#######################
;#NUMBERTOSTRING
;#Convert number in HL to a string
;#input: hl = number to display
;#output: hl = pointer to string
;#######################
numberToString:
	exx
		ld de,numberString	;for the ldi in dN_b2dloop
	exx
	ld de,-10000	;check how many 10,000s there are in the number
	call dN_b2d
	ld de,-1000		;check how many 1,000s units there are
	call dN_b2d
	ld de,-100		;hundreds
	call dN_b2d
	ld de,-10		;tens
	call dN_b2d
	ld de,-1		;single digits
	call dN_b2d

	ld c,-1				;this part here removes the leading 0s
	ld hl,numberString-1  ;where the string is stored
nTS_clearZeros:
	inc hl
		ld a,(hl)
		cp '0'			;ten ($0A) is the value of 0 in my alphabet (i think ASCII uses $30?)
	 jr z,$-4		;repeat until we find a non-zero number
	or a
	 jr nz,$+3
		dec hl
	ret

dN_b2d:
	ld a,-1		 ;if there is no carry the first run through, # = 0
dN_b2dloop:
	inc a			;each iteration increase accumulator by 1
	add hl,de
	jr c,dN_b2dloop
	or a
	sbc hl,de		;225 - 100 = 125 - 100 = 25 - 100 = -75. This adds 100 again so we can check the next digits.
	exx
		add a,$30
		ld (de),a
		inc de
	exx
	ret

;#######################
;#BIGNUMBERTOSTRING
;# Convert number in AHL to a string
;# input: ahl = number to display
;# output: hl = pointer to converted string
;#######################
bigNumberToString:
	ld ix,bigNumberString	;where we will store the result
;-10,000,000
	ld c,$67
	ld de,$6980
	call b2d			;count how many 10,000,000s there are
;-1,000,000
	ld c,$F0
	ld de,$BDC0
	call b2d
;-100,000
	ld c,$FE
	ld de,$7960
	call b2d
;-10,000
	inc c			;ld c,$FF
	ld de,-10000
	call b2d
;-1,000
	ld de,-1000
	call b2d
;-100
	ld de,-100
	call b2d
;-10
	ld e,-10
	call b2d
;-1
	ld e,-1
	call b2d
	ld (ix),a		;put the terminating zero
	ld hl,bigNumberString-1  ;where the string is stored
	jr nTS_clearZeros

;#######################
;#B2D
;# Convert the units of a 24-bit binary
;#	number in AHL into decimal
;# input: ahl = number to display
;#		  cde = negative value of units to check (if you
;#				want to check 10s, cde should equal -10)
;#		  ix  = string pointer to store the unit
;#######################
b2d:
	ld b,-1			;if there is no carry the first run through, # = 0
b2dLoop:
		inc b		;each iteration increase accumulator by 1
		add hl,de
		adc a,c
	 jr c,b2dLoop
	sbc hl,de		;225 - 100 = 125 - 100 = 25 - 100 = -75. This adds 100 again so we can check the next digits.
	sbc a,c
	set 4,b
	set 5,b			;b+$30
	ld (ix),b
	inc ix
	ret

cursor:
#include "sprites/misc/cursor.bmp"
#include "sprites/misc/cursor_mask.bmp"																											
