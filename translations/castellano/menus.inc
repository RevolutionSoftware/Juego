;# menu items
;list of routines to call for each choice
;x,y box coordinates
;width,height of box
;item text
;##########################_principal_######################################
startMenuTxt:
.db 7
.dw dispItems
.dw main
.dw main
.dw dispStats
.dw main
.dw main
.dw main
.dw $0303			;y,x
.dw $392A			; $WWHH Width and Height of box
.db MN,"Elem",NEWL
.db MN,"Magia",NEWL
.db MN,"Equip",NEWL
.db MN,"Estado",NEWL
.db MN,"Opcs",NEWL
.db MN,"Guard",NEWL
.db MN,"Salir",0

;##########################_batallas_######################################

;###########################_tiendas_######################################
shopBookshop:
.db "Bienvenida a la librería! Qué te gustaría comprar?",0

shopItemshop:
.db "Puede que lo que tenemos no sea mucho, pero es más de lo que tienes tú!",0

;#############################_estadísticas_###############################
noItemsTxt:
.db "Hombre, no tienes",NEWL
.db "nada",BRK,0

statsMenuTxt:
.db NEWX,20,_NAME,NEWX,70,"Nv ",STAT,_LVL,NEWXY,2,12
.db "PV:",NEWX,24,_CURHP,"/",_MAXHP,NEWL
.db "PM:",NEWX,24,_CURMP,"/",_MAXMP,NEWL
.db "FZ:",NEWX,24,_STR,NEWX,50,"EXP:",NEWL
.db "UDF:",NEWX,24,_UDF,NEWX,50,STAT,EXP,NEWL
.db "LDF:",NEWX,24,_LDF,NEWL
.db "AGI:",NEWX,24,_AGI,NEWL
.db "INT:",NEWX,24,_INT,NEWX,50,"SNC:",NEWX,78,_SNC,0
;desperation and concentration don't have numerical values to show
