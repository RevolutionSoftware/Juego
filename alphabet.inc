;_MENU se adjunta antes de la letra que empieza el elemento del menú
#DEFINE _MENU(xxx) %10000000 | xxx

alfabeto:
#include "sprites/alphabet/alphabet.bmp"

;	á	é	í	ó	ú
A = $5B
E = $5C
I = $5D
O = $5E
U = $5F

dropTable:
.db "gjpqy,"	;letters that drop one pixel below where most letters are drawn (letters with tails)
width5:
.db "MNVWmvw#"	;# = cursor
width3:
.db "\"+1<>ITijlst",I
width2:
.db "!'(),./:;"
width_end:
