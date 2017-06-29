;start of brushes
	.dw grass
	.dw path
	.dw path_corner1
	.dw path_corner2
	.dw path_corner3
	.dw path_corner4
	.dw path_round1
	.dw flower
	.dw tree_topleft
	.dw tree_topright
	.dw tree_topleft_forest
	.dw tree_topright_forest
	.dw tree_bottom_left
	.dw tree_bottomright
	.dw sign_item
	.dw wall_vert
	.dw wall_horiz
	.dw wall_bottom_left_corner
	.dw wall_bottom_right_corner
	.dw wall_top_left_corner
	.dw wall_top_right_corner
	.dw house_new_left
	.dw house_new_right
	.dw door_pretty
	.dw door_old_books
	.dw house_old_left_sign
	.dw house_old_right
	.dw roof_left
	.dw roof_right
	.dw roof_bookshop
	.dw roof_item_shop
	.dw roof_weapon_shop
	.dw sign_hanging_libros
	.dw sign_weapon
	.dw arch_bottom_left
	.dw arch_bottom_middle
	.dw arch_bottom_right
	.dw arch_top_left
	.dw arch_top_middle
	.dw arch_top_right

grass:
	.db %00000000
	.db 0
path:
	.db %00000000
	.db 1
path_corner1:
	.db %00000000
	.db 2
path_corner2:
	.db %00000000
	.db 3
path_corner3:
	.db %00000000
	.db 4
path_corner4:
	.db %00000000
	.db 5
path_round1:
	.db %00000000
	.db 6
flower:
	.db %00000000
	.db 7
tree_topleft:
	.db %00001100
	.db 8
	.db 11
	.db 0
tree_topright:
	.db %00001100
	.db 9
	.db 12
	.db 0
tree_topleft_forest:
	.db %00001110
	.db 10
	.db 18
	.db 4
tree_topright_forest:
	.db %00001110
	.db 11
	.db 18
	.db 4
tree_bottom_left:
	.db %00000110
	.db 12
	.db 9
	.db 2
tree_bottomright:
	.db %00000110
	.db 13
	.db 10
	.db 2
sign_item:
	.db %10000110
	.db 14
	.db -1
	.db 14
.db TEXT \ .dw text2
wall_vert:
	.db %00000010
	.db 15
wall_horiz:
	.db %00000110
	.db 16
	.db 15
	.db 6
wall_bottom_left_corner:
	.db %00000010
	.db 17
wall_bottom_right_corner:
	.db %00000010
	.db 18
wall_top_left_corner:
	.db %00000110
	.db 19
	.db 16
	.db 6
wall_top_right_corner:
	.db %00000110
	.db 20
	.db 17
	.db 6
house_new_left:
	.db %00000010
	.db 21
house_new_right:
	.db %00000010
	.db 22
door_pretty:
	.db %00000010
	.db 23
door_old_books:
	.db %01000010
	.db 24
.db MAP,5,7,2
house_old_left_sign:
	.db %00000010
	.db 25
house_old_right:
	.db %00000010
	.db 26
roof_left:
	.db %00000110
	.db 27
	.db 5
	.db 1
roof_right:
	.db %00000110
	.db 28
	.db 6
	.db 10
roof_bookshop:
	.db %00000110
	.db 29
	.db 18
	.db 0
roof_item_shop:
	.db %00000110
	.db 30
	.db 18
	.db 0
roof_weapon_shop:
	.db %00000110
	.db 31
	.db 18
	.db 0
sign_hanging_libros:
	.db %10000110
	.db 32
	.db -1
	.db 14
.db TEXT \ .dw text1
sign_weapon:
	.db %10000110
	.db 14
	.db 7
	.db 14
.db TEXT \ .dw text0
arch_bottom_left:
	.db %00000110
	.db 33
	.db 19
	.db 6
arch_bottom_middle:
	.db %00000100
	.db 34
	.db 20
	.db 0
arch_bottom_right:
	.db %00000110
	.db 35
	.db 21
	.db 6
arch_top_left:
	.db %00001100
	.db 36
	.db 22
	.db 0
arch_top_middle:
	.db %00001100
	.db 37
	.db 23
	.db 0
arch_top_right:
	.db %00001100
	.db 38
	.db 24
	.db 0
