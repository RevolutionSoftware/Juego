;page0 : tilemap
;page1 : map data
;page2 : battles
#include "ti83plus.inc"
#include "ion.inc"
#include "app.inc"

#define CAST		;spanish CAST, french FR, or english EN

#macro getHL()
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
#endmacro

;24 bit addition
.addinstr add ahl,de	00CE19	3 NOP 1	;add hl,de \ adc a,0	;3 bytes
.addinstr add ahl,bc	00CE09	3 NOP 1	;add hl,bc \ adc a,0
.addinstr add ahl,cde	8919	2 NOP 1	;add hl,de \ adc a,c	;2 bytes
.addinstr add ahl,bde	8819	2 NOP 1	;add hl,de \ adc a,b
.addinstr add ahl,ebc	8309	2 NOP 1	;add hl,bc \ adc a,e
.addinstr add ahl,dbc	8209	2 NOP 1	;add hl,bc \ adc a,d

;24 bit subtraction
.addinstr sub ahl,cde	52ED91	3 NOP 1	;sub c \ sbc hl,de		;3 bytes
.addinstr sbc ahl,bde	9852ED	3 NOP 1	;sbc hl,de \ sbc a,b	;3 bytes
.addinstr sbc ahl,de	00DE52ED	4 NOP 1 ;sbc hl,de \ sbc a,0		;4 bytes

;###############_CONSTANTS_##############
PLAY_X_START = 2		;starting coordinates of player
PLAY_Y_START = 6
MAP_START	 = 2

PLYR_Y_OVRHG = 12		;how many pixels of character can overlap a tile
PLYR_X_OVRHG = 1		;how many pixels of sprite overlap x-wise
NON_PASS	 = 14		;donde empiezan los tiles que no se pueden atravesar
SCROLL_LEFT	 = $24		;donde empezar a mover la cámara
SCROLL_RIGHT = $2C		;donde empezar a mover la cámara
SCROLL_DOWN	 = $1C
SCROLL_UP	 = $14
;player directions:
FACE_RIGHT	= 1

;routines in RAM:
ramRoutines = $8E67		;494 bytes saferam at "graphVariables"
;drawGbuf		= 64 bytes

;saferam1 utilizado en el motor de batallas

;rutina de texto:
menuCoordinates = saferam3	;saferam3 contiene 128 bytes
cursorGbufLoc	= menuCoordinates+100
cursorGbufSave	= cursorGbufLoc+2	;guardar 14 bytes detras del cursor

;stats
HP			= 0	
MAXHP		= 2
MP			= 4
MAXMP		= 6
STR			= 8		;strength
UDF			= 10	;upper defense
LDF			= 12	;lower defense
AGI			= 14	;agility (attack speed and dodging capability)
INT			= 16	;intelligence (magic)
END			= 18	;endurance (attack bar)
CNC			= 19	;concentration (ability to do special attacks)
SNC			= 20	;sincerity (likelihood of doing extra damage)
DSP			= 21	;desperation (limit breaks)
;equipment
HEAD		= 22	;head armor
CHEST		= 23	;chest armor
HANDS		= 24	;hand armor
FEET		= 25	;feet armor
FEET_SLOT	= 26	;accessory for feet armor
ARM_L		= 27	;weapon for left hand
ARM_L_SLOT	= 28	;slot for an accessory
ARM_R		= 29	;weapon for right hand
ARM_R_SLOT	= 30	;slot
ACC1		= 31	;accessory 1
ACC2		= 32	;accessory 2
EXP			= 33	;total experience
LVL			= 36	;current level
STATS_TOTAL	= 37

;STATS equates for the text routine
;jugadores
_NAME	= 1			;remember, 0 = end of text, so we have to start at 1
_CURHP	= 3+HP
_MAXHP	= 3+MAXHP
_CURMP	= 3+MP
_MAXMP	= 3+MAXMP
_STR	= 3+STR
_UDF	= 3+UDF
_LDF	= 3+LDF
_AGI	= 3+AGI
_INT	= 3+INT
_END	= 3+END
_CNC	= 3+CNC
_SNC	= 3+SNC
_DSP	= 3+DSP
;...armor/weapons
_EXP	= 3+EXP
_LVL	= 3+LVL

;---------------------
;hp, max and actual (4, 2 bytes each)
;mp, max and actual (4, 2 bytes each)
;strength			(2)
;defense			(2)
;agility			(2)
;intelligence		(2)
;sincerity			(1)
;desperation		(1)
;====TOTAL==========(18)

;item equates:
BREAD		= 0
DRIEDFRUIT	= 1
PIE			= 2
WARMWATER	= 3
TEA			= 4
HERB		= 5
MEDICINE	= 6
FLOWER		= 7

;gbuf is used as a 1120 byte buffer instead of 768
;yCoord/xCoord
;xOff: x offset of map (gbuf)
;yOff: y offset of map (gbuf)
;playerDir: direction value between 0 & 3 
;playerY: player's y coordinate on screen (range between $10-$20) 
;playerX: x coord (range: $18-$38)

playerGold	= $8A36				;4 bytes
playerData 	= appData			;256 bytes
pCurHP		= playerData+HP		;0-1 999
pMaxHP		= playerData+MAXHP	;2-3 999
pMaxMP		= playerData+MP		;4-5 999 mind (magic)
pCurMP		= playerData+MAXMP	;6-7 999
pStr		= playerData+STR	;8-9 999
pUDef		= playerData+UDF	;10-11 999
pLDef		= playerData+LDF	;12-13 999
pAgi		= playerData+AGI	;14-15 999 agility (speed/dodging attacks)
pInt		= playerData+INT	;16-17 999 intelligence (magic power)
pEnd		= playerData+END	;18	100 endurance (size of attack bar)
pConc		= playerData+CNC	;19	100
pSinc		= playerData+SNC	;20	100 sincerity (luck)
pDesp		= playerData+DSP	;21 100 desperation: limit breaks
;...
pLevel		= playerData+LVL

;************* map variables and some text variables
pulsado		= saferam4	;ops: 66 bytes
flechaPulsada	= pulsado+1			;0
secondPulsado	= flechaPulsada+1	;1
animationCounter = secondPulsado+1	;2
animationFrame	= animationCounter+1;3 walking frame of character
playerAnimation	= animationFrame+1	;4
saveHL		= playerAnimation+1		;5 4 bytes
menuCounter	= saveHL
mapWidth	= saveHL+4				;9 number of columns
mapHeight	= mapWidth+1			;10 number of rows map has
yCoord		= mapHeight+1			;11 map Y coord, aligned. Y=2 is really pixel 2*16=32 on the screen
xCoord		= yCoord+1				;12 map X coord
;mapLocation = xCoord+1				;13 2 bytes address of current map
mapOverDraw = xCoord+1				;15 used in map routine to determine which part/"layer" of the map to draw
defTile		= mapOverDraw+1			;16 default tile to draw
playerY		= defTile+1				;17	player coordinates
playerYOff	= playerY+1				;18
playerX		= playerYOff+1			;19
playerXOff	= playerX+1				;20
playerDir	= playerXOff+1			;21 hacia donde se dirige el personaje
tileCheck	= playerDir+1			;22 para la detección de colisiones
bigNumberString = tileCheck+1		;23	7 bytes
numberString	= bigNumberString+1	;	continuation of bignumberstring
battleCounter	= numberString+6	;30	how many steps left until a battle
menuCursor	= battleCounter+1		;31	the currently selected menu choice
optionList	= menuCursor+1			;32 2 bytes
textDelay	= optionList+2			;34 a delay when drawing a text
defCol		= textDelay+1			;35 default column to go to after a newline
cursorWait	= defCol+1				;36 a timer for the blinking cursor
textOffset	= cursorWait+1			;37 the first menu item to draw in a scrolling menu
textStart	= textOffset+1			;38 stores the first byte of a menu's text (skipping header)
textCoords	= textStart+2			;40 the coordinates where text should be updated for scrolling menu
menuItems	= textCoords+2			;42 how many menu items were drawn
menuBoxY	= menuItems+1			;43 X/Y coordinates of a menubox
menuBoxX	= menuBoxY+1
menuBoxWidth= menuBoxX+1			;45 width/height
menuBoxHeight	= menuBoxWidth+1
numSubMenus	= menuBoxHeight+1		;46 how many submenus there are (0, 1, 2)
subMenu1Y	= numSubMenus+1			;47	y position of submenu 1's box
subMenu1X	= subMenu1Y+1			;48 x position
subMenu1Width	= subMenu1X+1		;49 etc.
subMenu1Height	= subMenu1Width+1	;50
subMenu2Y	= subMenu1Height+1		;51 same for menu 2
subMenu2X	= subMenu2Y+1			;52
subMenu2Width	= subMenu2X+1		;53
subMenu2Height	= subMenu2Width+1	;54
maxItems	= subMenu2Height+1		;55
flashPage	= maxItems+1			;56	starting flash page of the application

;battle engine and shop engine
monsterBuffer	= saferam1					;0-149 saferam1 = 756 bytes
monsterCoords	= monsterBuffer+150			;150 monster coordinates in a battle
playerCoords	= monsterCoords+24			;174 player coords in a battle X,Y,spriteID (3 bytes/entry) 
monsterCount	= playerCoords+24			;198
monsterSelectBuffer	= monsterCount+1		;199
battleQueue		= monsterSelectBuffer+20	;219-518
damageAmount	= battleQueue+300			;519
descriptionList	= damageAmount+1			;520 descriptions in the store
menuList		= descriptionList+40		;560-659+ to store the main entries in a list (TMENU,TJOIN,textlabel,TNEWLINE = 5 bytes)
priceList		= menuList+100				;660-699 an array of the prices (2 bytes) in the shop
itemsInMenu		= priceList+40				;700 how many items are in the menu

smallEditCol	= $8177				;185 bytes
playerInventory	= smallEditCol		;itemid,amount - this should allow the player up to 92 items
									;first byte = how many items player has

MD5Stuff	= $8259					;74 bytes
scratchSpace = MD5Stuff

MD5Buffer3	= $83CF					;2 bytes
seed		= MD5Buffer3			;random seed

saferam0 = $9D95					;5000 bytes
tileData = saferam0
unpackBuffer	= saferam0+4000-36
tileData_inout	= saferam0+4000-4
brush_inout		= saferam0+4000-2
itemMenuTxt		= saferam0+4000		;up to 50 items, 8 bytes each
subMenu1Txt		= saferam0+4400		;3 bytes each
subMenu2Txt		= saferam0+4550		;4 bytes each

mapBuffer		= saferam0+5000

;"flags"
;asm_Flag1/2/3
textActions = asm_Flag1
scrollMenu	= 0						;whether or not to allow scrolling in the menu
returnMenu	= 1						;whether to automatically execute a routine or return when a choice is selected

battleActions = asm_Flag2
enAtt		= 0
batWon		= 1
batLost		= 2

#macro page0(routine)				;destroys shadows
	call callOffpage0
	.dw routine
#endmacro

#macro page1(routine)
	call callOffpage1
	.dw routine
#endmacro

#macro page2(routine)
	call callOffpage2
	.dw routine
#endmacro

defpage(0, "Juego  ")

initiate:
	in a,(6)						;current page
	ld (flashPage),a
#include "setup.asm"
;	page2(startBattle)

main:
	call drawMap	;dibujar las tilas normales/el "suelo"
	call drawPlayer
	call drawGbuf
	
	ld hl,(battleCounter)
	ld a,l
	or h
;	call z,startBattle
	
keyLoop:
	ld hl,flechaPulsada	;se está pulsando alguna de las flechas? para la animacion del personaje
	ld (hl),0

	ld a,$FD		;grupo 2: enter, minus, plus, clear, div, etc.
	out (1),a		;puerto 1: el teclado
	in a,(1)
	cp 191			;clear
	ret z

	ld a,$BF		;grupo 7: graph, window, mode, 2nd, del	
	out (1),a
	in a,(1)
	cp 191
	call z,startMenu
	cp 223			;2nd
	jr nz,no2nd
	ld a,(secondPulsado)
	or a
	call z,buscaInteraccion
	jr no2nd2
no2nd:
	xor a
	ld (secondPulsado),a
no2nd2:
	ld hl,pulsado
	ld (hl),1
    ld a,$DF        ;Grupo 6
    out (1),A
    in a,(1)
    cp 127          ;Alpha
	jr nz,noPulsado
	inc (hl)
noPulsado:
	ld a,$FF
	out (1),a
	ld a,$FE
	out (1),a
	in a,(1)
	rra
	push af
		call nc,mapDown
	pop af
	rra
	push af
		call nc,mapLeft
	pop af
	rra
	push af
		call nc,mapRight
	pop af
	rra
	call nc,mapUp
	jr main

salirDelPrograma:
;restaurar valores que sobreescribimos para crear nuestro "saferam"
	ld hl,gbuf
	ld (hl),0
	ld de,gbuf+1
	ld bc,1119
	ldir				;reinicializar valores
	bcall(_RandInit)	;limpiando lo que sobreescribimos
	bcall(_ClrTxtShd)
	bcall($4573)		;_SaveCmdShadow
	bcall($4831)		;_grReset
;delete inserted ram
	ld de,12000
	ld hl,$9D95
	bcall(_DelMem)
	bjump(_JForceCmdNoChar) ;chao!

updateRotation:
	ld a,(xOff)
	and 7
	ld hl,gbufMask
	ld e,a
	ld d,0
	add hl,de
	ex af,af'
	ld a,(hl)
	ld hl,maskLeft
	ld (hl),a
	ld hl,maskRight
	xor $FF
	ld (hl),a
	ex af,af'

	ld hl,rotateRight
	cp 4
	jr nc,rotarDer
	ld hl,rotateLeft
rotarDer:
	and %00000011
	ld e,a
	ld d,0
	add hl,de
	push hl
	ld de,rotLeft
	ldi
	ldi
	ldi
	ldi
	pop hl
	ld de,rotRight
	ldi
	ldi
	ldi
	ldi
	ret

rotateRight:       ;if offset greater than or equal to 4, rotate the gbuf right up to four times
 rrca 
 rrca 
 rrca 
 rrca 
rotateLeft:       ;the nops keep it smoother (same delay as rlca)
 nop          ; so whether we shift or not, it will take the same amount of cycles
 nop 
 nop 
 nop 
 rlca 
 rlca 
 rlca 
 rlca

gbufMask:
.db %11111111	;0
.db %11111110	;1
.db %11111100	;2
.db %11111000	;3
.db %11110000	;4
.db %11100000	;5
.db %11000000	;6
.db %10000000	;7

;waits for the user to press (and release) [2nd]
pause:
	ld a,$BF
	call getKey
	bit 5,a
	 jr nz,pause
	ld a,$BF
	call getKey
	bit 5,a
	 jr z,$-7
	ret

#include "interruptRoutines.asm"
#include "math.asm"
#include "moveRoutines.asm"
#include "textRoutines.asm"
;#include "textMenuRoutines.asm"
#include "mapRoutines.asm"
#include "spriteRoutines.asm"
#include "statsRoutines.asm"
;#include "battleRoutinesEnemy.asm"
;#include "battleRoutinesPlayer.asm"
#include "shopRoutines.asm"
#include "objects.asm"
#include "playerRoutines.asm"
#include "playerData.inc"
#include "monsterData.inc"
#include "itemData.inc"
#include "alphabet.inc"

cursor_sprite:
#include "sprites/misc/cursorWait.bmp"

tileData_inside:
#include "map editor/inside/tiles.inc"
tileData_outside:
#include "map editor/outside/tiles.inc"

#ifdef CAST
#include "translations/castellano/castellano.inc"
#endif

#include "copyToRam.asm"
.org copy_to_ram+(copy_to_ram_end-copy_to_ram_start)

;page 1 contains maps. loadMap just loads a map into saferam.
defpage(1)
loadMap:
	add a,a
	ld l,a
	ld h,0
	ld bc,mapTable
	add hl,bc
	getHL()			;map address into hl
	ld a,(hl)
	ld (mapWidth),a	;first byte is the map width
	inc hl
	ld a,(hl)		;second byte is the map height
	ld (mapHeight),a
	ld a,(playerY)	;check if player Y is too far down the map
	add a,2
	cp (hl)			;(hl) = mapheight
	 jr c,$+10
		ld a,(mapHeight)
		sub 4		;+2-4 will move the map 2 more pixels up
		ld (yCoord),a
	inc hl			;default tile
	ld a,(hl)
	ld (defTile),a
	inc hl			;start of tilemap
	ld de,mapBuffer
	ld bc,7000
	ldir
	ret

#include "mapData.asm"

defpage(2)
#include "battleRoutines.asm"
#include "battleSprites.asm"
#include "battleSpritesData.inc"
