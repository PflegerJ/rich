; 1/13/2025
; this assumes that pointerLo has already been set
CreateGameObject:
    ; hi and low are set. we just need to get the 8 values stored in the table they are pointing to.
    ; set them using the object_blah_blah_blah and do the linked list shit. so do we need to check for empty list each time?
    ldy #$00    ; index set to 0
    ; it might be better to jsut not loop. because i need one of the registers to use as the offset for storing the data into spriteram
    ; th8is is fucking wrong and dumb and im dumb 

    ; ok another yolo
    ; we have gameObjLo and Hi. Lo will be pointing to the start of the next free slot of ram.
    ; Hi will always be $03 cause im just using range of 0300-03FF
    ; this is bad and basic but i have to do something and it sorta makes sense on how to do it. 
    ; so while i'm doing this i'll probably figure out the why its not done this way


    ; just thought of a major issue
    ; what if i have 10 game objects. and the 5th needs to be deleted? right now that would not work... fuck thats why you have a linked list. but like....
    ; hmmmmmmmmm


    ; so we assuming the gameObjectLo is already pointing to the correct place
    lda (pointerLo), y 
    sta (gameObjectLo), y 
    iny 

    lda (pointerLo), y 
    sta (gameObjectLo), y 
    iny  

    lda (pointerLo), y 
    sta (gameObjectLo), y 
    iny  

    lda (pointerLo), y 
    sta (gameObjectLo), y 
    iny   

    lda (pointerLo), y 
    sta (gameObjectLo), y 
    iny  

    lda (pointerLo), y 
    sta (gameObjectLo), y 
    iny  

    lda (pointerLo), y 
    sta (gameObjectLo), y 
    iny  

    lda (pointerLo), y 
    sta (gameObjectLo), y 
    iny  

    lda gameObjectLo
    clc 
    adc #$08
    sta gameObjectLo

    rts 