specialChar:	
	cp $F0				;si bit 7 está armado pero no es un código especial, significa que es una opción de menú
	jr nc,skipMenu
		call setUpMenu
		ld a,(hl)
		res 7,a
		jp textoBucle
skipMenu:
	ld bc,textoBucle-1	;(para incluir el ld a,(hl))
	push bc				;donde volver
	ex de,hl			;guardar hl
	ld c,a
	ld b,$FF			;sign extending bc so that -1 ($FF) stays -1 ($FFFF) and not 255 ($00FF)
	ld hl,specialCharTable
	add hl,bc
	add hl,bc			;x2 (esencialmente estamos restando, porque el valor de bc es negativo)
	getHL()
	push hl
	ex de,hl			;recordar valor de hl
	ret
	
;si añades más, ponlos aquí, encima de los demás
.dw dispNum8
.dw joinString	; ^ $F6
.dw shopMenu	;/|\$F7
.dw normalMenu	; | $F8
.dw insertStat	; | $F9
.dw newXY		; | $FA
.dw newX		; | $FB
.dw newLine		; | $FC
.dw newBox		; | $FD
.dw insertSpace	; | $FE
.dw endOfString	; | $FF
specialCharTable:;|

endOfString:
	pop bc
    inc hl			;saltar $FF
    pop bc			;bc = próximo label a escribir
    ld a,c
    or b			;bc=0?
	 jp z,wait2nd	;if bc=0, there aren't any more strings to display. we use bc to preserve the value of hl
	dec a
	or b
	 ret z

    ld l,c
    ld h,b			;cargar próximo string en hl
    call contarLetras
    ld a,(hl)
    jp textoBucle

insertSpace:
	inc hl
	exx
		ld hl,penCol
		inc (hl)
	exx
	call contarLetras
	inc b
	 ret nz					;b=$FF if text has reached the bottom of the screen
	pop bc
	jp wait2ndMoreText

newBox:
	pop bc		;desalojar pila
	inc hl		;skip $FD character
	jp wait2ndMoreText

newLine:
 	inc hl
 	push hl
 		ld hl,mapData	;porque contarLetras se ocupa de los newlines, un dummy string (mapData)
 		call contarLetras
 	pop hl
 	ld a,(hl)
 	inc b
 	ret nz
 	 jp wait2ndMoreText
	
newX:
	inc hl			;saltar $FB
	ld de,penCol
	ldi
	ret

newXY:
 	inc hl
 	ld de,penCol
 	ldi				;(hl)+ to (de)+
 	ld de,penRow
 	ldi				;(hl)+ to (de)+
 	ret

;STATS:
;Jugadore:
;I_MAXHP = 0
;I_CURHP = 2
;I_MAXMP = 4
;I_CURMP = 6
;I_STR	= 8
;I_DEF	= 10
;I_AGI	= 12
;I_INT	= 14
;I_SNC	= 16
;I_DSP	= 17
;Enemigo:
;I_ECURHP	= 31
;I_EMAXHP	= 33
insertStat:
	inc hl              ;hl now points to what to insert
	ld a,(hl)           ;what to show?
	inc hl
	ld b,(hl)
	inc b
	jr z,$+5			;si el próximo byte es $FF (EOS), no hay que empujar nada
		pop bc
		push hl			;empujar próximo byte, donde continuará el string después de mostrar este texto
		push bc			;tenemos que desocupar la pila para poner otro valor encima
	ld hl,playerData
	ld c,I_TOTAL		;size of each player's data
	cp 31
	jr c,$+9
		ld hl,monsterBuffer
		sub 31			;enemy values start at 31, subtract this to calculate correctly the offset later on
		ld c,I_ETOTAL	;cantidad que ocupan los datos de cada enemigo
	ld e,a				;que vamos a dibujar (hp, mp, etc.)
	ld a,(curOption)
	or a
	ld b,a
	ld a,e				;valor que dibujar
	push af				;guardarlo
	jr z,noAddPlayerOffset
	 add a,c
	djnz $-1
	ld e,a
noAddPlayerOffset:
	ld d,0
	add hl,de
	getHL()				;destruye a
	pop af				;a = atributo que queremos mostrar
	cp I_SNC
	jr c,$+3
		ld h,d			;todo después de sinceridad ocupa un solo byte, por eso hay que poner h a 0
	call numberToString
	ret

shopMenu:			;for shops
	inc hl				;skip special menu token
	set dispDesc,(iy+textActions)	;mostrar un texto con la descripción del elemento escogido
	push hl
		inc hl			;skip join character
		getHL()
		ld a,T_EOS
		inc hl
		cp (hl)
		jr nz,$-2		;loop until we've reached the end of the string
		inc hl			;skip T_EOS
		ex de,hl		;de = string pointer
		ld hl,descriptionList
		ld a,(numOptions)
		add a,a			;description list is just a list of strings, so two bytes/entry
		ld c,a
		ld b,0
		add hl,bc
		ld (hl),e		;de = string pointer, load into hl
		inc hl
		ld (hl),d
	pop hl
	call setUpMenu
	ret

joinString:
		inc hl
		inc hl
		inc hl				;donde reanudar el procesamiento del string anterior después de acabar de procesar este nuevo
		pop bc
		push hl
		push bc				;despejar la pila para meterle otro valor
		dec hl
		dec hl
		getHL()
		ret

normalMenu:
	inc hl
setUpMenu:
	exx
		ld hl,numOptions    ;how many buttons there are
		ld a,(hl)
        inc (hl)
	    add a,a				;a*2, each entry has 2 bytes: y,x
        ld c,a
        ld b,0
        ld hl,buttonCoords
        add hl,bc			;get the correct offset in buttonCoords to store the next coordinates
        ld a,(penCol)		;the coordinates are used to know where to highlight each button
        ld (hl),a
        add a,9
        ld (penCol),a		;actualizar penCol
        inc hl
        ld a,(penRow)  	 	;save the row/column
		ld (hl),a
	exx
	ret

dispNum8:
	inc hl
	ld a,(hl)		;number to display
	inc hl
	pop bc
	push hl			;empujar próximo byte, donde continuará el string después de mostrar este texto
	push bc			;tenemos que desocupar la pila para poner otro valor encima
	ld l,a
	ld h,0
	jp numberToString
