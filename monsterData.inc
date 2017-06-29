monsterData:
.dw mSoldier
.dw mPoli
.dw mSoldier2

mSoldier:
.db 0	;sprite#
.dw mSoldierTxt
.dw 30	;hp
.dw	20	;mp
.dw 4	;str
.dw 5	;def
.dw 4	;agi
.dw 2	;int
.db 1	;sinc
.db 1	;desp

mSoldier2:
.db 0	;sprite#
.dw mSoldierTxt
.dw 20	;hp
.dw	20	;mp
.dw 8	;str
.dw 5	;def
.dw 6	;agi
.dw 2	;int
.db 1	;sinc
.db 1	;desp


mPoli:
.db 1	;sprite#
.dw mPoliTxt
.dw 40	;hp
.dw	10	;mp
.dw 5	;str
.dw 5	;def
.dw 11	;agi
.dw 2	;int
.db 1	;sinc
.db 1	;desp


enemySprites:
;npc0Sprite
#include "sprites/npcs/npc0.bmp"
policeSprite:
#include "sprites/npcs/police.bmp"