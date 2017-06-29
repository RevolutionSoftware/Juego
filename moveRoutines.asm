mapDown:
	ld hl,updateMapDown
	push hl				;saltar a updateMapDown después de terminar la rutina
	ld hl,flechaPulsada	;para avanzar la animacion del jugador
	ld (hl),1
	ld hl,playerDir
	ld (hl),2

   	ld a,(playerXOff)
   	add a,PLYR_X_OVRHG	;número de pixeles del sprite del personaje que no forman parte del personaje
   	ld c,a
	ld a,(playerYOff)
	ld hl,pulsado		;=número de pixeles a saltar
	add a,(hl)
	add a,15			;revisar azulejo debajo del personaje
	exx
		ld e,a
	exx
	call checkTile
	jr z,tilePassDown
		ld de,mapHeight
		sbc hl,de
		jr nz,downMasked
			ld a,(playerXOff)
			add a,2
			ld c,a
			xor a
			call checkTile
			ld hl,(saveHL)
			ld a,(hl)
			bit 6,a
			jp nz,goToInteraction
downMasked:
;masked tiles
		ld hl,(saveHL)		;first brush
		ld a,(hl)
		ld c,$0F
		rra					;animation
		rra					;if passable
		jr nc,notVarTop		;if passable, skip
		ld b,0
		rra
		jr c,varHeight
		ld hl,playerYOff
		ld a,(hl)
		ld (hl),0
		or a
		ret z				;si l'on était déjà aligné, il ne faut pas augmenter playerY
		ld hl,playerY
		inc (hl)
		ret					;if no var height, leave
varHeight:
		call skipBrushInfo
		jr nc,notVarTop
		ld b,(hl)
		ld c,b
notVarTop:
		ld hl,(saveHL+2)
		ld a,(hl)
		rra
		rra
		jr nc,endTopCheck		;if not passable, skip
		ld b,0
		rra
		ret nc				;if no var height, leave
		call skipBrushInfo
		jr nc,endTopCheck
		ld a,c
		cp (hl)
		jr c,$+3
		ld b,(hl)
endTopCheck:
		ld hl,playerYOff
		ld (hl),b
		ret
tilePassDown:
	ld hl,playerY
	ld a,(mapHeight)
	dec a
	ld b,a					;for updatemapdown
	cp (hl)
	ret z
	ex de,hl
	ld hl,playerYOff
	ld a,(pulsado)
	add a,(hl)
	cp 16
	jr nc,newTileEnd
	ld (hl),a
	ret

mapRight:
	ld hl,updateMapRight
	push hl
	ld hl,flechaPulsada
	ld (hl),1
	ld hl,playerDir
	ld (hl),FACE_RIGHT

    ld a,(playerXOff)
    ld hl,pulsado
    add a,(hl)				;current x position on map
    add a,15-PLYR_X_OVRHG
    ld c,a
    ld a,(playerYOff)
   	add a,PLYR_Y_OVRHG
    exx
    	ld e,a
    exx
	call checkTile
	jr z,tilePassRight
	
	ld hl,playerXOff
	ld a,(pulsado)
	add a,(hl)
	cp 16
	ld (hl),PLYR_X_OVRHG
	ret c
	ld hl,playerX
	inc (hl)
	ret
tilePassRight:
	ld hl,playerX
	ld a,(mapWidth)
	dec a
	ld b,a
	cp (hl)
	ret z
	ex de,hl
	ld hl,playerXOff
	ld a,(pulsado)
	add a,(hl)
	ld (hl),a
	cp 16
	ret c
;hl = o playerXOff o playerYOff
;de = o playerX o playerY
newTileEnd:
	sub 16
	ld (hl),a		;hl = playerXOff or playerYOff
					;de = playerX or playerY, depending on calling routine
	ex de,hl
	inc (hl)		;playerX += 1
	ld a,(hl)
	cp b			;b=mapWidth
	ret c			;if mapWidth>playerX, quit
	ld (hl),a
	xor a
	ld (de),a		;de = playerXOff/YOff
	ret

mapUp:
	ld hl,updateMapUp
	push hl
	ld hl,flechaPulsada
	ld (hl),1
	ld hl,playerDir
	ld (hl),3

   	ld a,(playerXOff)
   	add a,PLYR_X_OVRHG
   	ld c,a
	ld a,(playerYOff)
	ld hl,pulsado
	sub (hl)
	add a,PLYR_Y_OVRHG		;para saber donde comienzan las colisiones del personaje (para dejar que
	exx						; el personaje se acerque un poco más a las paredes/etc.)
		ld e,a
	exx
	call checkTile
	jr z,tilePassUp
;if we can't pass...
	ld hl,playerYOff
	ld a,(hl)
	add a,PLYR_Y_OVRHG
	ld (hl),16-PLYR_Y_OVRHG
	cp 8
	ret nc
	ld (hl),0
tilePassUp:
	ld hl,playerYOff
	ld a,(pulsado)
	neg
	add a,(hl)
	ld (hl),a
	cp 16
	ret c
	add a,16
	ld (hl),a
	ld de,playerY
	ld a,(de)
	or a
	jr nz,notBeginningTile
	ld (hl),a			;set playerXOff to 0
	ret
notBeginningTile:
	dec a
	ld (de),a
	ret


;set flechapulsada, which let's the animation routine know that the player is moving (or trying to)
;update playerdir to LEFT
;subtract pulsado (which says how many pixels to move) from the player's xoffset then add plyr_x_ovrhg (how much of the player is empty space)
; for example: our x position is 13. the leftmost 2 bits of the sprite are empty, the sprite doesn't begin until pixel 15. say we are running,
; pulsado = 2. we start with x=13 then subtract (because we need to subtract to go left) 2 pixels. We're at x=11.
; HOWEVER, at x=11 there are two blank columns of pixels in the sprite, therefore we add 2, now x=13.
; so we will check whatever's at x=13 to see if we can pass
mapLeft:
	ld hl,updateMapLeft
	push hl					;return to updateMapLeft
	ld hl,flechaPulsada
	ld (hl),1
	xor a
	ld (playerDir),a
	
    ld a,(playerXOff)
    ld hl,pulsado
	sub (hl)				;current y position on map
    add a,PLYR_X_OVRHG
    ld c,a
    ld a,(playerYOff)
    add a,PLYR_Y_OVRHG
    exx
    	ld e,a
    exx
	call checkTile
	jr z,tilePassLeft
;if we can't pass...
	ld hl,playerXOff
	ld a,c
	ld (hl),16-PLYR_X_OVRHG
	cp $F0
	ret c
	ld hl,playerX
	dec (hl)
	ret
tilePassLeft:
	ld hl,playerXOff
	ld a,(pulsado)
	neg
	add a,(hl)
	ld (hl),a
	cp 16
	ret c
	add a,16
	ld (hl),a
	ld de,playerX
	ld a,(de)
	or a
	jr nz,notBeginningTile
	ld (hl),a			;set playerXOff to 0
	ret

updateMapX:
	ld hl,playerX
	ld a,(hl)
	ld hl,xCoord
	sub (hl)			;playerX - xCoord, shouldn't be any carry
	rlca
	rlca
	rlca
	rlca				;x16
	ld hl,playerXOff
	add a,(hl)
	ld hl,xOff
	sub (hl)			;player's position in pixels on screen
	ret

updateMapRight:
	ld hl,xCoord
	ld a,(mapWidth)
	sub 6				;hay 6 columnas ya en la pantalla
	cp (hl)
	jr nz,notRight
	xor a				;X align map because we've reached the edge
	ld (xOff),a			;hl = yOff
	jp updateRotation
notRight:
	ex de,hl
	call updateMapX
	cp SCROLL_RIGHT
	ret c
	add a,(hl)
	sub SCROLL_RIGHT
	ld (hl),a
	cp 16
	jp c,updateRotation
	sub 16
	ld (hl),a
	ex de,hl
	inc (hl)
	jp updateRotation

updateMapLeft:
	call updateMapX
	cp SCROLL_LEFT
	ret nc
	add a,(hl)
	sub SCROLL_LEFT
	ld (hl),a
	cp 16
	jp c,updateRotation
	add a,16
	ld (hl),a
	ld de,xCoord
	ld a,(de)
	or a
	jr nz,notLeft
	ld (hl),a
	jp updateRotation
notLeft:
	dec a
	ld (de),a
	jp updateRotation
	
updateMapDown:
	ld hl,yCoord
	ld a,(mapHeight)
	sub 4				;hay 4 filas ya en la pantalla
	cp (hl)
	jr nz,notBottom
	xor a
	ld (yOff),a			;hl = yOff
	ret
notBottom:
	ex de,hl
	call updateMapY
	cp SCROLL_DOWN
	ret c
	add a,(hl)
	sub SCROLL_DOWN
	ld (hl),a
	cp 16
	ret c
	sub 16
	ld (hl),a
	ex de,hl
	inc (hl)
	ret

updateMapUp:
	call updateMapY
	cp SCROLL_UP
	ret nc
	add a,(hl)
	sub SCROLL_UP
	ld (hl),a
	cp 16
	ret c
	add a,16
	ld (hl),a
	ld de,yCoord
	ld a,(de)
	or a
	jr nz,notTop
	ld (hl),a
	ret
notTop:
	dec a
	ld (de),a
	ret

updateMapY:
	ld hl,playerY
	ld a,(hl)
	ld hl,yCoord
	sub (hl)
	rlca
	rlca
	rlca
	rlca		;x16
	ld hl,playerYOff
	add a,(hl)
	ld hl,yOff
	sub (hl)
	ret
