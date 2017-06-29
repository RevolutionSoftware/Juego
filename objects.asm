;0=text
;1=map change

;bit 0:animated
;bit 1:passable: set if nonpassable
;bit 2:tile top: shorter
;bit 3:bridge masking
;bit 4:
;bit 5:
;bit 6:collision interaction (when player touches tile)
;bit 7:interaction
;	0 - text (text label)
;	1 - change map (x,y,mapid)
;	2 - open shop (shop_id)

brushes_inside:
#include "map editor/inside/objects.asm"
brushes_inside_end:

brushes_outside:
#include "map editor/outside/objects.asm"
brushes_outside_end:

TEXT = 0
MAP = 1
SHOP = 2

;masks
#include "map editor/inside/masks.inc"

interacciones:
 .dw $0000				;00 draw text
 .dw changeMap_action	;01
 .dw openShop			;02

defaultMapLocation:
 .db PLAY_X_START,PLAY_Y_START,MAP_START
