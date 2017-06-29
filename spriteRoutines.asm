;playerSprite: pointer to first sprite. each player direction has 3 sprites and 3 masks, all 16x16. 
;playerDir: direction value between 0 & 3 
;playerY: player's y coordinate on screen (range between $10-$20) 
;playerX: x coord (range: $10-$40 or $20-$30, haven't decided yet) 

drawPlayer:
	call drawPlayer_call
;check if the tile below is a masked tile or not
	ld a,(mapWidth)
	ld h,a
	ld a,(playerY)			;tile at player's head
	ld e,a
	call multEH
	ld de,mapBuffer
	add hl,de
	ld de,(playerX)
	ld d,0
	add hl,de				;hl = location in map
	push hl
;find position in gbuf
		exx
			ld hl,yCoord
			ld a,(playerY)
			sub (hl)
			add a,a
			add a,a
			add a,a
			add a,a				;x 16
			ld e,a				;de = y offset on screen
			ld d,0
	
			ld hl,xCoord
			ld a,(playerX)
			sub (hl)
			add a,a

			ld l,e
			ld h,d
			add hl,hl
			add hl,de
			add hl,hl
			add hl,de
			add hl,hl			;x14
			ld e,a
			add hl,de
			ld de,gbuf
			add hl,de
			ld de,13
		exx
;check if it's a bridge-masked brush
		call retrieveBrush
		ld a,(hl)
		bit 3,a
		 call nz,drawTileMask
check2ndTile:
	pop hl					;map location
	push hl
		inc hl				;next map square
		exx
			inc hl
			inc hl			;move two bytes over in gbuf
		exx
		call retrieveBrush
		ld a,(hl)
		bit 3,a
		 call nz,drawTileMask
;checkTileLeftFeet
	pop hl
	ld de,(mapWidth)
	ld d,0
	add hl,de
	push hl
		exx
			ld bc,(14*16)-2
			add hl,bc		;next row in gbuf
		exx
		call retrieveBrush
		ld a,(hl)
		and %1100
		 call nz,drawTileMask
	pop hl
	inc hl
		exx
			inc hl
			inc hl
		exx
	call retrieveBrush
	ld a,(hl)
	and %1100
	 ret z
drawTileMask:
	exx
		push hl
	exx
	inc hl
	ld e,(hl)	;sprite id
	inc hl
	xor a
	ld bc,maskTiles
	ld l,(hl)		;mask
	ld h,a		;h=0
	add hl,hl	;x2
	add hl,hl	;x4
	add hl,hl	;x8
	add hl,hl	;x16
	add hl,hl	;x32
	add hl,bc	;points to first byte in mask
	push hl
	pop ix		;ix=first byte in mask

	ld bc,(tileData_inout)
	ld l,e		;sprite id
	ld h,a		;h=0
	add hl,hl	;x2
	add hl,hl	;x4
	add hl,hl	;x8
	add hl,hl	;x16
	add hl,hl	;x32
	add hl,bc	;points to first byte in sprite
	ld b,16		;16 filas en sprite
;hl = sprite
;ix = mask
;hl' = gbuf
drawSpriteMasked:
	ld a,(ix)
	inc ix
	exx				;change to shadow vars
		and (hl)	;mask the gbuf
	exx
	or (hl)			;tilesprite
	inc hl
	exx
		ld (hl),a
		inc hl	;proximo byte en gbuf
	exx
	ld a,(ix)
	inc ix
	exx
		and (hl)	;mask
	exx
	or (hl)			;tileSprite
	inc hl		;2nd set (pixels are 16x16: 2 bytes wide)
	exx
		ld (hl),a
		add hl,de	;bajar a la proxima fila
	exx
	djnz drawSpriteMasked
	exx
		pop hl		;restore gbuf location
	exx
	ret

drawPlayer_call:
	ex af,af'
		xor a				;offset Y del jugador a añadir
	ex af,af'
	ld hl,playerAnimation
	ld a,(flechaPulsada)	;si una flecha está pulsada o no	
	inc (hl)
	or a
	jr nz,$+4				;si no flecha pulsada
		ld (hl),2
	jr z,pNoMove
		ld hl,(battleCounter)	;count down to a wild battle
		ld a,(pulsado)
		ld c,a
		ld b,0
		sbc hl,bc
		jr nc,$+3
			inc hl
		ld (battleCounter),hl
pNoMove:
	ld a,(playerAnimation)
	ld bc,64		;cada sprite = 64 bytes
	ld hl,playerSprite
	and %00001100	;sólo nos importan estos dos bits
	bit 2,a			;esencialmente: %1000 y %0000 mostraremos la postura "normal"
	jr z,normalStance	;			%0100 mostraremos la primera animación, y %1100 la segunda.
	add hl,bc		;1ª animacion	así, mostramos 1. %0000 normal, 2. %0100 primera animación
	ex af,af'		;				3. %1000 postura normal, y 4. %1100 segunda animación.
		dec a
	ex af,af'
	bit 3,a
	jr z,$+3
	add hl,bc		;2ª (recuerda, bc= 64, cada sprite con su máscara = 64 bytes
normalStance:
	push hl
	pop ix			;dirección de sprite en ix
	ld a,(playerDir)
	ld b,a
	ld de,32*2*3	;cada direccion tiene 3 sprites con sus mascaras
	or a
	jp z,$+7		;si playerDir=0, saltar el djnz
		add ix,de
		djnz $-2
	ld hl,yCoord
	ld a,(playerY)
	sub (hl)
		
	add a,a
	add a,a
	add a,a
	add a,a			;x16
	ld b,a
	ld a,(playerYOff)
	add a,b
	ld e,a
	ex af,af'
		add a,e		;más arriba hay un xor a y luego un dec a. el dec a pondrá a a -1 sólo cuando
		ld e,a		; el personaje está en una de las animaciones (1ª o 2ª)
	ex af,af'

	call revYFuera
	jp m,endPlayerSprite	;esto a lo mejor no es necesario porque no creo que se salga nunca completamente de la pantalla, pero...
;revYFuera deja h y d con un valor de cero
	ld l,e
	add hl,hl		;y*2
	add hl,de		;y*3
	add hl,hl		;y*6
	add hl,de		;y*7
	add hl,hl		;y*14
	ld de,gbuf+2	;86EC
	add hl,de
	ld a,(xCoord)
	ld b,a
	ld a,(playerX)
	sub b			;playerX-xCoord
	ld e,a
	sla e
	ld a,(playerXOff)
	cp 8
	jr c,$+3
		inc e			;if XOff >= 8, move one byte over
	ld d,0
	add hl,de		;hl = starting point in gbuf
	and $7
	ld b,a
	call drawPlayerSprite
endPlayerSprite:
	ret


;entrada:
; e = y coord
;salida:
; c = número de filas del sprite a dibujar 
; ix = primera fila a dibuja
; e = y coord (0)
;destruye:
; h = 0
; d = 0
; a
revYFuera:
	ld h,0
	ld d,h		;ld d,0
	ld c,16		;c=altura de sprite, en pixeles
	ld a,e		;a=y coord
	or a
	ret p		;saltar si y coord es positivo
		dec c
		add a,c	;si el sprite está fuera de pantalla...
		ret m	;...no dibujar
		inc a	;porque c tenía que ser 16, pero no quería comprobar si a era negativo o cero
		ld c,a	;16-número de pixeles fuera de la pantalla
		ld a,e	;cargar de nuevo valor y
		neg		;*-1
		add a,a	;x2, porque cada fila del sprite tiene dos bytes
		ld e,a
		add ix,de	;saltar al primer byte del sprite a mostrar
		ld e,h	;e=0, no queremos dibujar el sprite fuera de la pantalla
		ret

;ix = player sprite
;ix+32 = player sprite mask
;b = x
;e = y
dibSprite:
	call revYFuera	;revisar y coord para ver si está fuera de pantalla
	ret m		;si es negativo, está completamente fuera de la pantalla
dS_positive
; y * 12
	ld l,e		;hl=de
	add	hl, hl	; 2
	add	hl, de	; 3
	add	hl, hl	; 6
	add	hl, hl	; 12

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
drawPlayerSprite:
drawPlayerSpriteLoop:
	push bc
drawPlayerRow
   ld d,(ix+32)   ;sprite mask
   ld e,(ix+33)
   ld a,$ff
   dec b \ inc b \ jr z,skipMaskClip	;si b=0
   scf \ rr d \ rr e \ rra \ djnz $-6   ;rotate mask
skipMaskClip:
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
   ld d,(ix)      ;sprite
   ld e,(ix+1)
   xor a
   cp b
   jp z,skipSpriteClip
   srl d \ rr e \ rra \ djnz $-5   ;rotate sprite
skipSpriteClip:
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
   jp nz, drawPlayerSpriteLoop
   ret

; a = x
; e = y
; ix = sprite
; (b = hauteur du sprite)
putSprite:
	ld	b, 8	; Sprite 8 de hauteur
putSprite_var:
;adjust for the x/yOffset
	ld c,a
	ld a,(yOff)
	add a,e
	ld e,a
	ld a,(xOff)
	add a,c
	ld h,0
	ld d,h		;d & h = 0
; y * 14
	ld l,e
	add	hl, hl	; 2
	add	hl, de	; 3
	add	hl, hl	; 6
	add	hl, de	; 7
	add	hl, hl	; 14
; x / 8
	ld	e, a
	srl e
	srl e
	srl e
	add	hl,de		; A present on a le decalage dans hl
	ld de,gbuf	; Prendre le debut du graphbuffer
	add	hl,de		; Puis ajouter le decalage
	and $07			; %0000111 pour en tirer l'offset x
	ld c,a			; Sauver dans c
	or a            ; aligné ?
	jr nz,dso_non_aligne
dso_aligne:
	ld de,14
dso_aligne_loop:
	ld a,(ix)
	xor (hl)		;hl = octet dans gbuf
	ld (hl),a		;écrire le nouvel octet !
	inc	ix			;prochain octet du sprite
	add	hl,de 		;prochain rang dans gbuf
	djnz dso_aligne_loop
	ret
; a= decalage
dso_non_aligne:
	ld de,13
dso_non_aligne_loop:
	push	bc
	ld	b, c	; On va utiliser le nombre de rotations comme compteur
	ld	a, (ix)	; L'octet qu'il faut decaler 
	inc	ix
	ld	c, d	; mettre c à 0
dso_shift_loop:
; carry = 0 (regarder le inc ix)
	 rra		; Decaler a vers la droite et ce qui sort va en carry
	 rr	c		; Injecter la carry dans c
	 djnz	dso_shift_loop
	xor	(hl)
	ld	(hl), a	; Ecrire le premier octet
	inc	hl		; Avancer d'un cran
	ld	a, c
	xor	(hl)
	ld	(hl), a ; Ecrire le second octet
	add	hl, de 	; prochain rang du gbuf
	pop	bc
	djnz dso_non_aligne_loop
	ret

;l = y
;a = x
;ix = sprite
putSpriteMasked:
	ld b,7
putSpriteMasked_var:
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
	ld (cursorGbufLoc),hl
	push hl
	push bc
	ex af,af'
;copy the bytes beneath the cursor
		ld a,b			;sprite height
		ld de,cursorGbufSave
		 ldi			;load gbuf into gbuf buffer (:P)
		 ldi			;guardar dos bytes debajo del cursor
		 ld bc,12
		 add hl,bc		;próxima fila del gbuf
		 dec a
		jr nz,$-9
	ex af,af'
	pop bc
	pop hl
	and $07		;x offset
	ld c,a
	or a		;si x offset=0, significa que el sprite está alineado
	jr nz,maskedNotAlignedLoop
		ld de,14
maskedAlignedLoop:
		ld a,(hl)
		and (ix+7)
		xor (ix)
		ld (hl),a		;guardar
		inc ix			;próximo byte
		add hl,de 		;próxima fila del gbuf
		djnz maskedAlignedLoop
		ret
maskedNotAlignedLoop:
	push bc			;b = número de iteraciones restantes, c = xoffset
		ld b,c
		ld a,(ix+7)	;máscara
		ld c,$FF	;dummy byte
		ld d,(ix)
		ld e,0
mNARotate:
		scf
		rra			;a = mask
		rr c		;c = overflow
		srl d		;d = sprite byte
		rr e		;e = sprite overflow
		djnz mNARotate
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
	pop	bc
	djnz maskedNotAlignedLoop
	ret

;a = sprite height
restoreGbuf:
	ld hl,cursorGbufSave
	ld de,(cursorGbufLoc)
	 ldi			;load gbuf into gbuf buffer (:P)
	 ldi			;guardar dos bytes debajo del cursor
	 ld bc,12
	 ex de,hl
		 add hl,bc		;próxima fila del gbuf
	 ex de,hl
	 dec a
	jr nz,$-11
	ret

playerSprite:
;########################_izquierda#################################
 .db $0F,$E0,$10,$10,$38,$08,$3F,$F8,$2F,$F8,$33,$F8,$21,$F8,$13,$EC
 .db $0F,$68,$07,$40,$0C,$C0,$0C,$A0,$13,$10,$10,$08,$1C,$18,$0F,$F0
 ;mascara
 .db $F0,$1F,$E0,$0F,$C0,$07,$C0,$07,$C0,$07,$C0,$07,$C0,$07,$E0,$13
 .db $F0,$97,$F8,$BF,$F0,$3F,$F0,$1F,$E0,$0F,$E0,$07,$E0,$07,$F0,$0F
 ;izq 1
 .db $0F,$E0,$10,$10,$38,$08,$3F,$F8,$2F,$F8,$33,$F8,$21,$F8,$13,$E4
 .db $0E,$D0,$15,$20,$1B,$20,$08,$D0,$10,$18,$28,$14,$24,$24,$1B,$D8
 ;mascara
 .db $F0,$1F,$E0,$0F,$C0,$07,$C0,$07,$C0,$07,$C0,$07,$C0,$07,$E0,$1B
 .db $F0,$2F,$E0,$1F,$E0,$1F,$F0,$0F,$E0,$07,$C0,$03,$C0,$03,$E4,$27
 ;izq 2
 .db $0F,$E0,$10,$10,$38,$08,$3F,$F8,$2F,$F8,$33,$FA,$21,$FC,$13,$F0
 .db $0E,$68,$13,$D0,$12,$30,$0C,$10,$10,$18,$28,$14,$24,$24,$1B,$D8
 ;mascara
 .db $F0,$1F,$E0,$0F,$C0,$07,$C0,$07,$C0,$07,$C0,$05,$C0,$03,$E0,$0F
 .db $F0,$17,$E0,$0F,$E0,$0F,$F0,$0F,$E0,$07,$C0,$03,$C0,$03,$E4,$27
;########################_derecha_##################################
 .db $07,$F0,$08,$08,$10,$1C,$1F,$FC,$1F,$F4,$1F,$CC,$1F,$84,$37,$C8
 .db $16,$F0,$02,$E0,$03,$30,$05,$30,$08,$C8,$10,$08,$18,$38,$0F,$F0
 ;mascara
 .db $F8,$0F,$F0,$07,$E0,$03,$E0,$03,$E0,$03,$E0,$03,$E0,$03,$C8,$07
 .db $E9,$0F,$FD,$1F,$FC,$0F,$F8,$0F,$F0,$07,$E0,$07,$E0,$07,$F0,$0F
 ;der 1
 .db $07,$F0,$08,$08,$10,$1C,$1F,$FC,$1F,$F4,$1F,$CC,$1F,$84,$27,$C8
 .db $0B,$70,$04,$A8,$04,$D8,$0B,$10,$18,$08,$28,$14,$24,$24,$1B,$D8
 ;mascara
 .db $F8,$0F,$F0,$07,$E0,$03,$E0,$03,$E0,$03,$E0,$03,$E0,$03,$D8,$07
 .db $F4,$0F,$F8,$07,$F8,$07,$F0,$0F,$E0,$07,$C0,$03,$C0,$03,$E4,$27
 ;der 2
 .db $07,$F0,$08,$08,$10,$1C,$1F,$FC,$1F,$F4,$5F,$CC,$3F,$84,$0F,$C8
 .db $16,$70,$0B,$C8,$0C,$48,$08,$30,$18,$08,$28,$14,$24,$24,$1B,$D8
 ;mascara
 .db $F8,$0F,$F0,$07,$E0,$03,$E0,$03,$E0,$03,$A0,$03,$C0,$03,$F0,$07
 .db $E8,$0F,$F0,$07,$F0,$07,$F0,$0F,$E0,$07,$C0,$03,$C0,$03,$E4,$27
;########################_abajo_####################################
 .db $0F,$F0,$10,$08,$27,$E4,$38,$1C,$32,$4C,$30,$0C,$39,$9E,$67,$E2
 .db $5A,$58,$27,$E4,$24,$24,$18,$18,$08,$10,$10,$08,$08,$F0,$07,$60
 ;mascara
 .db $F0,$0F,$E0,$07,$C0,$03,$C0,$03,$C0,$03,$C0,$03,$C0,$01,$98,$1D
 .db $A4,$27,$C0,$03,$C0,$03,$E0,$07,$F0,$0F,$E0,$07,$F0,$0F,$F8,$9F
 ;abajo 1
 .db $0F,$F0,$10,$08,$27,$E4,$38,$1C,$32,$4C,$30,$0E,$28,$9E,$17,$F2
 .db $0E,$49,$17,$C8,$14,$30,$08,$90,$08,$50,$13,$D0,$14,$A0,$0B,$00
 ;mascara
 .db $F0,$0F,$E0,$07,$C0,$03,$C0,$03,$C0,$03,$C0,$01,$D0,$01,$E8,$0D
 .db $F0,$06,$E0,$07,$E0,$0F,$F0,$0F,$F0,$0F,$E0,$0F,$E0,$5F,$F4,$FF
 ;abajo 2
 .db $0F,$F0,$10,$08,$27,$E4,$38,$1C,$32,$4C,$30,$0C,$79,$14,$4F,$E8
 .db $92,$70,$13,$E8,$0C,$28,$04,$90,$09,$08,$09,$E8,$06,$90,$00,$60
 ;mascara
 .db $F0,$0F,$E0,$07,$C0,$03,$C0,$03,$C0,$03,$C0,$03,$80,$0B,$B0,$17
 .db $60,$0F,$E0,$07,$F0,$07,$F8,$0F,$F0,$07,$F0,$07,$F9,$0F,$FF,$9F
;########################_arriba_###################################
 .db $0F,$F0,$10,$08,$20,$04,$38,$1C,$3F,$FE,$7F,$FE,$7F,$FE,$6F,$FE
 .db $4D,$7A,$0D,$30,$14,$28,$14,$38,$08,$10,$10,$08,$10,$78,$0F,$F0
 ;mascara
 .db $F0,$0F,$E0,$07,$C0,$03,$C0,$03,$C0,$01,$80,$01,$80,$01,$90,$01
 .db $B0,$05,$F0,$0F,$E0,$07,$E0,$07,$F0,$0F,$E0,$07,$E0,$07,$F0,$0F
 ;arr 1
 .db $0F,$F0,$10,$08,$20,$04,$38,$1C,$3F,$FC,$3F,$FE,$7F,$FE,$6F,$F9
 .db $2F,$64,$09,$20,$09,$20,$06,$90,$05,$08,$08,$68,$08,$90,$07,$60
 ;mascara
 .db $F0,$0F,$E0,$07,$C0,$03,$C0,$03,$C0,$03,$C0,$01,$80,$01,$90,$06
 .db $D0,$1B,$F0,$1F,$F0,$1F,$F8,$0F,$F8,$07,$F0,$07,$F0,$0F,$F8,$9F
 ;arr 2
 .db $0F,$F0,$10,$08,$20,$04,$38,$1C,$3F,$FC,$7F,$FC,$7F,$FE,$9F,$F6
 .db $26,$F4,$04,$90,$04,$90,$09,$60,$10,$A0,$16,$10,$09,$10,$06,$E0
 ;mascara
 .db $F0,$0F,$E0,$07,$C0,$03,$C0,$03,$C0,$03,$80,$03,$80,$01,$60,$09
 .db $D8,$0B,$F8,$0F,$F8,$0F,$F0,$1F,$E0,$1F,$E0,$0F,$F0,$0F,$F9,$1F
