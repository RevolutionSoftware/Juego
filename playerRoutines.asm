;a = player ID to load
;b = level of player
loadPlayer:
	ld e,a
	ld h,STATS_TOTAL		;size of extracted stats
	call multEH
	ld bc,playerData
	add hl,bc
	push hl
		ld h,CHAR_STATS_SIZE
		call multEH
		ld bc,characterStats
		add hl,bc			;start of player data
	pop de
	ld a,(hl)				;CurHP
	ld (de),a
	inc de
	inc hl
	ld a,(hl)				;CurHP
	ld (de),a
	inc de
	dec hl
	ldi						;MaxHP
	ldi
	ld a,(hl)				;CurMP
	ld (de),a
	inc de
	inc hl
	ld a,(hl)				;CurMP
	ld (de),a
	inc de
	dec hl
	ld bc,END-(MP+1)
	ldir					;MAXMP->Endurance
	xor a
	ld (de),a				;set concentration to 0
	inc de
	ldi						;Sincerity
	ld (de),a				;set desperation to 0
	inc de
	ld bc,ACC2-DSP
	ldir					;load weapons, armor, accessories
	ex de,hl
	ld (hl),1
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a				;set experience to 0
	inc hl
	ld a,(de)
	ld (hl),a				;level
;	call advanceLevel		;LVL+1 until we're at the desired level
	ret

;adjust players stats for the next level
advanceLevel:

;input:
;bc ?? = experience to add
;returns c if they've reached a new level, a = how many levels
addExperience:
