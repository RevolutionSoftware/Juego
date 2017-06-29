drawNPCs:
npcList = $+1
	ld hl,npcRalena
	ld a,(hl)
	inc hl
	ex af,af'
;load npc position vars
		ld b,(hl)		;X
		inc hl
		inc hl
		ld c,(hl)		;Y
;check if x coord is off screen
		ld hl,xCoord
		ld a,b
		sub (hl)		;npc x - xcoord
		cp 6
			ret nc			;revisar próximo npc		
		ld b,a
;check if y coord is off screen
		ld hl,yCoord
		ld a,c
		sub (hl)		;npc y - ycoord
		cp 4
			ret nc			;revisar próximo npc
		ld c,a
	ex af,af'
	ld l,a		;load sprite number into hl
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl		;x32
	add hl,hl		;x64
	ld de,npcSprites
	add hl,de		;primer byte de npc
	push hl
	pop ix
	ld a,c			;npc y
	add a,a
	add a,c
	add a,a
	add a,c
	add a,a			;x14
	ld l,a
	ld h,0
	add hl,hl		;y*2
	add hl,hl		;y*4
	add hl,hl		;y*8
	add hl,hl		;y*16

	ld a,b			;b=npc x
	add a,a			;x2
	ld b,0
	ld c,a
;	add hl,bc
	ld bc,saferam1+2	;86EC
	add hl,bc
	ld b,0			;b=x offset
	call drawPlayerSprite
	ret