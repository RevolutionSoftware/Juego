;start of brushes
	.dw carpetL_ralItem
	.dw carpetR_ralItem
	.dw counter_bottom
	.dw counter_corner
	.dw counter_horiz
	.dw counter_vert
	.dw floor1
	.dw floor2
	.dw floor_bottom
	.dw floor_top

carpetL_ralItem:
	.db %01000000
	.db 0
.db MAP,5,23,0
carpetR_ralItem:
	.db %01000000
	.db 1
.db MAP,5,23,0
counter_bottom:
	.db %00000010
	.db 2
counter_corner:
	.db %00000110
	.db 3
	.db 0
	.db 6
counter_horiz:
	.db %00001100
	.db 4
	.db 15
	.db 0
counter_vert:
	.db %00000010
	.db 5
floor1:
	.db %00000000
	.db 6
floor2:
	.db %00000000
	.db 7
floor_bottom:
	.db %00000110
	.db 8
	.db 1
	.db 12
floor_top:
	.db %00000110
	.db 9
	.db 2
	.db 1
