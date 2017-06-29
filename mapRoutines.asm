;for "bridges" podemos seguir escaneando una fila más para ver si hay algún puente (overhead) y dibujarlo a, o creo que es al revés pero piénsalo!

;from an interaction with the map
changeMap_action:
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld a,(hl)
;a = mapID
;e = player X
;d = player Y
changeMap:
;hl points to first parameter
	push af
		xor a
		ld (playerXOff),a
		ld (playerYOff),a
		ld (yOff),a
		ld a,8
		ld (xOff),a
		ld a,e			;1st parameter: player starting X
			ld (playerX),a
			sub 3
			jr nc,$+6
				xor a
				ld (xOff),a
			ld (xCoord),a
		ld a,d
			ld (playerY),a
			sub 2
			 jr nc,$+3
				xor a
			ld (yCoord),a
	pop af
	push af
		page1(loadMap)
		call updateRotation
	pop af
;premask the tiles to save processing time
	ld hl,brushes_inside	;start of inside brushes / how many there are
	ld de,tileData_inside
	ld b,(brushes_inside_end-brushes_inside)/2
	cp INSIDE_MAP
	 jr nc,$+10
		ld hl,brushes_outside
		ld de,tileData_outside
		ld b,(brushes_outside_end-brushes_outside)/2
	push de
	exx
	pop de
		ld hl,(defTile)
		ld h,0
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl				;x32
		add hl,de				;hl' = default tile
	exx
	ld (tileData_inout),de		;save if we are using inside or outside tiles
	ld (brush_inout),hl			;..and brushes
	ld de,tileData
unpackTileLoop:
	exx
	push hl
		ld de,unpackBuffer
		ld bc,32
		ldir
	pop hl
	exx
	push bc
	push hl
	push de
		getHL()
		ld c,(hl)	;action byte
		inc hl
		inc hl
		ld a,(hl)	;mask number
		dec hl
		ld l,(hl)	;sprite number
		ld h,0
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl	;x32
		ld de,(tileData_inout)
		add hl,de	;start of sprites data
		bit 2,c
		 jr z,notMasked
;hl = start of sprite data
;a = mask number
		push hl
		pop ix		;ix = start of sprite data
		ld l,a		;mask id
		ld h,0
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl
		add hl,hl	;x32
		ld de,maskTiles
		add hl,de	;hl = mask data
		ld de,unpackBuffer
		ld b,32		;32 bytes
maskSpriteLoop:
		ld a,(de)	;default tile
		and (hl)	;mask
		or (ix)		;sprite data
		ld (de),a
		inc hl
		inc de
		inc ix
		djnz maskSpriteLoop
		ld hl,unpackBuffer
notMasked:
	pop de
	ld bc,32
	ldir			;copy the masked sprite
	pop hl
	pop bc
	inc hl
	inc hl
	djnz unpackTileLoop
	ret

;input:
;de = xcoord
;b = y coord
drawMap:
	ld hl,animationCounter
	dec (hl)
	jr nz,_anim
		ld (hl),32
		inc hl		;animationFrame
		inc (hl)
		ld a,(hl)
		cp 3
		jr nz,_anim
			ld (hl),0
_anim:
	ld hl,mapOverDraw
	ld a,1
	xor (hl)
	ld (hl),a
	ld a,(xCoord)
	ld d,0
	ld e,a
	ld hl,mapBuffer
	add hl,de
	ld a,(yCoord)
	ld e,a
	ld a,(mapWidth)
	ld b,a
	add hl,de
	djnz $-1
	exx
		ld hl,gbuf		;donde guardar el mapa antes de dibujarlo a la pantalla
		ld de,13		;numero de columnas en gbuf menos uno (por el inc hl')
		ld bc,$0705		;dibujar 7 columnas y 5 filas de sprites (en la pantalla caben 6x4))
drawRow:
		push hl			;save gbuf
	exx
	push hl				;save map loc	
	ld l,(hl)
;	call retrieveBrush	;returns hl pointing to brush data
;	ld a,(hl)			;action byte
	ld bc,tileData
;	inc hl
;	and %00000001		;a ver si tile es animado o no
;	 jr z,notAnimated
;		ld a,(animationFrame)
;		or a
;		 jr z,notAnimated	;if frame 0, no anim.
;			inc hl
;			ld bc,animatedTiles-32
notAnimated:
;	add a,(hl)			;animationFrame+(spriteID) if there's an animation, or 0+(spriteID) if not
;	ld l,a	;numero del sprite
	ld h,0	;h=0
	add hl,hl	;x2
	add hl,hl	;x4
	add hl,hl	;x8
	add hl,hl	;x16
	add hl,hl	;x32
	add hl,bc	;points to first byte in sprite
	ld b,16		;16 filas en sprite
drawMapSprite:
	ld a,(hl)	;sprite pixels to draw
	inc hl		;next set of pixels
	exx			;change to shadow vars
		ld (hl),a
		inc hl	;proximo byte en gbuf
	exx
	ld a,(hl)
	inc hl		;2nd set (pixels are 16x16: 2 bytes wide)
	exx
		ld (hl),a
		add hl,de	;bajar a la proxima fila
	exx
	djnz drawMapSprite
skipSprite:
	pop hl		;posicion en el mapa
	inc hl
	exx
		pop hl	;gbuf anterior
		inc hl
		inc hl	;saltar dos bytes porque los sprites son de 16x16
		djnz drawRow
	exx
	ld a,(mapWidth)
	sub 7			;ya hemos avanzado un cierto numero de tiles
	ld e,a
	ld d,0
	 jr nc,$+3
		dec d		;si el mapa tiene sólo tiene 6 baldosas de ancho...
	add hl,de		;próxima fila del mapa
	exx
noDibujar:
		ld d,0
		ld e,14*15	;
		add hl,de
		ld e,13
		ld b,7
		dec c
		jp nz,drawRow
	ret

;a=y offset
;c=x
;z = puedes pasar, nz = camino bloqueado
checkTile:
	ld hl,tileCheck
	ld (hl),0			;add a,0
;si hay algun error revisa el valor de a, tal vez sea negativa
	rra
	rra
	rra
	rra					;/16
	and $0F
	ld hl,playerY
	add a,(hl)
	ld hl,mapHeight
	cp (hl)
	jr c,$+4
		inc a			;reset z flag
		ret
	ld h,a
	ld a,(mapWidth)
	ld e,a
	call multEH
	ld a,c
	ld b,$FF
	cp $F0
	jr nc,negativeX
	rra
	rra
	rra
	rra					;/16
	and $0F
	ld b,a
negativeX:
	ld a,(playerX)
	add a,b
	ld e,a
	add hl,de
	ld de,mapBuffer
	add hl,de
	push hl
;first tile
	call retrieveBrush	;hl = pointer to brush
	ld (saveHL),hl
	ld a,(hl)
;	and %01000010		;segundo bit, si podemos pasar o no
	and %00000010		;segundo bit, si podemos pasar o no
	or a
	call nz, testBrush
	ld b,a
;second tile
    ld de,0
    ld c,e
    ld a,(playerDir)
	cp 2
    jr c,moveX
moveY:
    ld a,(playerXOff)
   	add a,PLYR_X_OVRHG
    and 15              ;check to see if we are aligned (osea si hay que revisar dos o solo un cuadrado debajo de nosotrxs)
    cp PLYR_X_OVRHG*2+1
    jr c,vamos
	inc e
	jr vamos
moveX:
	ld a,(playerYOff)
   	add a,PLYR_Y_OVRHG
    and 15              ;check to see if we are aligned (osea si hay que revisar dos o solo un cuadrado debajo de nosotrxs)
	cp PLYR_Y_OVRHG+1
    jr c,vamos
	ld hl,tileCheck
	ld (hl),16			;add a,16
	ld hl,mapWidth
	ld e,(hl)
vamos:
	pop hl
	add hl,de

	call retrieveBrush

	ld (saveHL+2),hl
	ld a,(hl)
	and %00000010		;segundo bit, si podemos pasar o no
	call nz,testBrush
	or b				;si a & b = 0
	ret z
	ld hl,(saveHL)
	ld de,(saveHL+2)
	sbc hl,de			;or b resets carry flag
	ret nz				;if hl and de aren't the same, don't pass
	ex de,hl			;restore hl
	ld a,(hl)
	ld b,%01000010
	and b				;reset all bits but 1 and 6
	cp b				;check if a=%01000010, if so we need to do an action
	jp z,goToInteraction
	bit 1,a
	ret

;input: hl=brush pointer (label)
	;return carry if passable/has a mask
skipBrushInfo:
	ld a,(hl)
	inc hl				;info byte
	inc hl				;sprite # byte	ld a,(hl)
	rra					;animated
	jr nc,$+3
	inc hl				;skip animated byte
	rra					;passable
	ret nc
	rra					;mask tiletop
	ret nc
	inc hl
	ret

;returns carry if not passable
testBrush:
	ld a,(hl)
	bit 2,a
	ret z
doSomething:
	inc hl				;skip extra info byte
	inc hl				;skip sprite no byte
	bit 0,a				;if animated
	jr z,$+3
	inc hl				;skip animated byte
	inc hl				;skip mask
;start code
	ld a,(hl)			;hl = top border
	or a
	ld a,1
	ret z
	ld a,(tileCheck)
	add a,(hl)

	ld c,a				;update byte
	exx
		ld a,e			;y offset
	exx
	and $0f
	cp c				;if top border = y offset
	jr nz,$+3
	inc a

	cp c			;if top border < y offset
	ret nc
	ex af,af'
	ld a,(playerDir)
	cp 2
	jr z,$+7
	ex af,af'
	add a,15-PLYR_Y_OVRHG
	cp c
	ret nc
	xor a
	ret

buscaInteraccion:
	ld a,(playerDir)
	or a
	jr z,checkLeft
	dec a
	jr z,checkRight
	dec a
	jr z,checkDown
checkUp:
   	ld a,(playerXOff)
   	add a,PLYR_X_OVRHG
   	ld c,a
	ld a,(playerYOff)
	add a,PLYR_Y_OVRHG-1
	jr continueCheck
checkDown:
   	ld a,(playerXOff)
   	add a,PLYR_X_OVRHG
   	ld c,a
	ld a,(playerYOff)
	add a,16
	jr continueCheck
checkLeft:
	ld a,(playerXOff)
	add a,PLYR_X_OVRHG-1
	ld c,a
	ld a,(playerYOff)
	add a,PLYR_Y_OVRHG
	jr continueCheck
checkRight:
	ld a,(playerXOff)
	add a,16-(PLYR_X_OVRHG*2)+1
	ld c,a
	ld a,(playerYOff)
	add a,PLYR_Y_OVRHG
continueCheck:
	exx
		ld e,a
	exx
	call checkTile
	ret z
	ld hl,(saveHL)
	ld a,(hl)
	bit 7,a
	jr z,firstByteNothing
	jr goToInteraction
firstByteNothing:
	ld hl,(saveHL+2)
	ld a,(hl)
	bit 7,a
	ret z

;hl = address of brush
goToInteraction:
	call getBrush
	ld a,(hl)			;action #
	add a,a
	inc hl				;parameters
	ex de,hl			;save in de
	ld l,a
	ld h,0
	ld bc,interacciones
	add hl,bc
	getHL()				;store address in jump table into hl
	push hl				;push it onto the stack
	ex de,hl			;...put the parameters back into hl
	ret					;...and jump to the action

getBrush:
	ld a,(hl)
	inc hl				;info byte
	inc hl				;sprite # byte
	rra					;animation
	jr nc,$+3
		inc hl	
	rra					;passable
	rra					;topborder
	jr nc,$+4
		inc hl				;mask #
		inc hl				;# of pixels
	ret

;input: hl = map location
;output: hl = first byte in brush
retrieveBrush:
	ld de,(brush_inout)
	ld l,(hl)
	ld h,0
	add hl,hl	;x2
	add hl,de	;pointer to brush
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ret
