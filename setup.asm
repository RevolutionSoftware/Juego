	di

	ld hl,12000
	bcall(_EnoughMem)
	 jr nc,$+5
		bjump(_JForceCmdNoChar) ;chau!
	ex de,hl					;hl = RAM to insert
	ld de,$9D95
	bcall(_InsertMem)

	ld h,d
	ld l,e
	inc de
	ld (hl),0
	ld bc,11999
	ldir

	ld hl,salirDelPrograma
	push hl			;el último ret nos mandará a salirDelPrograma

;cargar rutinas en RAM
	ld hl,copy_to_ram
	ld de,ramRoutines
	ld bc,copy_to_ram_end-copy_to_ram_start
	ldir

;vaciar inventorio
	ld hl,playerInventory
	ld (hl),$FF
	ld de,playerInventory+1
	ld bc,184
	ldir

	ld hl,playerInventory
	ld a,8
	ld (hl),a				;4 items

	inc hl
	ld (hl),0
	inc hl
	ld (hl),99

	inc hl
	ld (hl),1
	inc hl
	ld (hl),4

	inc hl
	ld (hl),2
	inc hl
	ld (hl),40

	inc hl
	ld (hl),3
	inc hl
	ld (hl),50

	inc hl
	ld (hl),4
	inc hl
	ld (hl),5


	inc hl
	ld (hl),5
	inc hl
	ld (hl),7

	inc hl
	ld (hl),6
	inc hl
	ld (hl),19

	inc hl
	ld (hl),7
	inc hl
	ld (hl),2


	ld a,$3
	ld (playerGold),a
	ld hl,$FF70
	ld (playerGold+1),hl	;playerGold = $3FFFF

;default values
	xor a
	ld b,6
	ld hl,bigNumberString
	ld (hl),a
	inc hl
	djnz $-2
	ld (hl),a				;0 terminate the string
	ld (playerDir),a

;default stats
	xor a
	call loadPlayer			;load player ID 0 (franci)

	xor a
	ld hl,playerAnimation
	ld (hl),a
	ld hl,mapOverDraw	;variable to know which pass through the mapper we're at (si hay que dibujar el suelo o las mascaras)
	ld (hl),1
	ld hl,animationCounter
	ld (hl),32
	inc hl			;animation frame
	ld (hl),a		;frame 0
	ld (secondPulsado),a
	ld a,MAP_START
	ld de,(PLAY_Y_START*256)+PLAY_X_START
	call changeMap
