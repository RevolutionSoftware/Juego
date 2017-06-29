mapTable:
;outside maps
RALENA = ($-mapTable)/2 \		.dw map_ralena

;battle maps
GRASS1 = ($-mapTable)/2 \		.dw map_battlegrass1

INSIDE_MAP = ($-mapTable)/2
;inside maps
BOOKSHOP = ($-mapTable)/2 \		.dw map_bookshop
ITEMSHOP = ($-mapTable)/2 \		.dw map_itemshop
WEAPONSHOP = ($-mapTable)/2 \	.dw weaponShop

mapData:
;outside
#include "maps/outside/map/ralena.inc"
;battles
#include "maps/outside/map/battleGrass1.inc"
;inside
#include "maps/inside/map/bookShop.inc"
#include "maps/inside/map/itemShop.inc"

weaponShop:
.db 11,7,33
.db 32,32,32,32,32,32,32,32,32,32,32
.db 33,33,33,33,33,33,33,33,33,33,33
.db 33,33,33,33,33,33,33,33,33,33,33
.db 33,33,37,33,33,33,33,33,33,33,33
.db 33,33,33,33,33,33,33,33,33,33,33
.db 33,33,33,33,33,33,33,33,33,33,33
.db 34,34,34,34,30,31,34,34,34,34,34


animatedTiles:
 .db $00,$00,$00,$80,$02,$00,$09,$80,$02,$00,$01,$00,$07,$E0,$0D,$F0
 .db $1B,$F8,$37,$FC,$2F,$FC,$2F,$FC,$2F,$FC,$1F,$F8,$0F,$F0,$07,$E0
 ;Tile 35
 .db $00,$00,$0C,$06,$0B,$9A,$08,$64,$05,$04,$1D,$E8,$E3,$CC,$8F,$E3
 .db $6E,$79,$27,$E6,$2F,$C8,$43,$F0,$5A,$D0,$E9,$08,$06,$C8,$04,$38

npcMasks:
npc0mask = (npcMasks-maskTiles)/32
#include "sprites/npcs/npc0mask.bmp"
