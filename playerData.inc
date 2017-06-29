;base stats
characterStats:
franciStats:
.dw 55		;hp
.dw 10		;mp
.dw 12		;str
.dw 12		;udef
.dw 12		;ldef
.dw	8		;agi
.dw 4		;int
.db 8		;end
.db 1		;sinc
.db 0		;head
.db 0		;chest
.db 0		;hands
.db 0		;feet
.db 0		;feet slot
.db 0		;arm left
.db 0		;arm left slot
.db 0		;arm right
.db 0		;arm right slot
.db 0		;accessory 1
.db 0		;accessory 2
.db 1		;level
CHAR_STATS_SIZE = $-characterStats

anselmoStats:
.dw 55		;hp
.dw 10		;mp
.dw 12		;str
.dw 12		;udef
.dw 12		;ldef
.dw	8		;agi
.dw 4		;int
.db 5		;end
.db 1		;sinc
.db 0		;head
.db 0		;chest
.db 0		;hands
.db 0		;feet
.db 0		;feet slot
.db 0		;arm left
.db 0		;arm left slot
.db 0		;arm right
.db 0		;arm right slot
.db 0		;accessory 1
.db 0		;accessory 2
.db 0		;level

nadyaStats:
.dw 55		;hp
.dw 10		;mp
.dw 12		;str
.dw 12		;udef
.dw 12		;ldef
.dw	8		;agi
.dw 4		;int
.db 5		;end
.db 1		;sinc
.db 0		;head
.db 0		;chest
.db 0		;hands
.db 0		;feet
.db 0		;feet slot
.db 0		;arm left
.db 0		;arm left slot
.db 0		;arm right
.db 0		;arm right slot
.db 0		;accessory 1
.db 0		;accessory 2
.db 0		;level

porterStats:
.dw 55		;hp
.dw 10		;mp
.dw 12		;str
.dw 12		;udef
.dw 12		;ldef
.dw	8		;agi
.dw 4		;int
.db 5		;end
.db 1		;sinc
.db 0		;head
.db 0		;chest
.db 0		;hands
.db 0		;feet
.db 0		;feet slot
.db 0		;arm left
.db 0		;arm left slot
.db 0		;arm right
.db 0		;arm right slot
.db 0		;accessory 1
.db 0		;accessory 2
.db 0		;level

pucStats:
.dw 55		;hp
.dw 10		;mp
.dw 12		;str
.dw 12		;udef
.dw 12		;ldef
.dw	8		;agi
.dw 4		;int
.db 5		;end
.db 1		;sinc
.db 0		;head
.db 0		;chest
.db 0		;hands
.db 0		;feet
.db 0		;feet slot
.db 0		;arm left
.db 0		;arm left slot
.db 0		;arm right
.db 0		;arm right slot
.db 0		;accessory 1
.db 0		;accessory 2
.db 0		;level

characterSprites:
anselmo_sprite:	#include "npcs/anselmo.bmp"
nadya_sprite:	#include "npcs/nadya.bmp"
porter_sprite:	#include "npcs/porter.bmp"
puc_sprite:		#include "npcs/puc.bmp"
