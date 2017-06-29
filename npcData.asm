npcTable:
.dw npcRalena


;.db sprite
;.db x,xOff
;.db y,yOff
;.dw text
npcRalena:
npcR1:
.db 0			;sprite no
.db 5,0			;x
.db 6,0			;y
.dw npcSprites
;end npc data
.db $FF











npcSprites:
;0
#include "sprites/npcs/npc1.bmp"
#include "sprites/npcs/npc1mask.bmp"