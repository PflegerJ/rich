Create2x3GameObject:
    rts 
C
DrawDynamic2x3GameObject:
    rts  

DrawStatic2x3GameObject:
    rts 

2x3DynamicGameObjectsDrawLo:
    .byte   Toilet
2x3DynamicGameObjectsDrawHi:
    .byte  

2x3StaticGameObjectsDrawLo:
    .byte
2x3StaticGameObjectsDrawHi:
    .byte 

;variables
;   0       x pos int
;   1       x pos float
;   2       y pos int
;   3       y pos float
;   4       attribute
;   5       state   
;   6       var1
;   7       var2
;   8       var3
;   9       animation offset
;   10      animation timer
;   11      drawFuntionLo
;   12      drawFunctionHi
;   13      gameLoopFunctionLo
;   14      gameLoopFunctionHi
;   15      next
;