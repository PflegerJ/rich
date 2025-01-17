; this function is written to work for now. but we have to be overhauled when game objects start to have more than one tile associated with them.
    ; i'll have to write draw functions for each and then figure out a buffer
    ; it seems like people have implemented buffers in their games for shit so i think its a solid idea. just don't know how bigg and how many and what overhead they require
        ; addressHi, addressLo, count, data, data.... is one idea
            ; having the address means it can be more generic. 
                ; i wonder if I could turn all my tables into a format that would work with the buffer?
                ; at least it would streamline some of the handling of data
                    ; not sure if there is any actual benefit but its something to consider down the road


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;objectNext = spriteRamStart + objectMax * 0 ; ok we are going to try this implementation i guess
    ;objectXPos = spriteRamStart + objectMax * 1
    ;objectYPos = spriteRamStart + objectMax * 2
    ;objectTile = spriteRamStart + objectMax * 3
    ;objectAtt = spriteRamStart + objectMax * 4
    ;objectVar1 = spriteRamStart + objectMax* 5
    ;objectHi = spriteRamStart + objectMax * 6
    ;objectLo = spriteRamStart + objectMax * 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; I'm stupid I don't need the other variables even for this stupid version. This is just display info which only uses y pos, tile, att, x pos
CopyObjectRamToSpriteRam:
    
    ldx #$00
    ldy firstOccupiedSlot

@SpriteCopyLoopStart:    
    cpy #objectMax
    beq @WritingFE

    lda objectYPos,y
    sta SPRITE_RAM_START,x
    inx 
    lda objectTile,y 
    sta SPRITE_RAM_START,x 
    inx 
    lda objectAtt,y 
    sta SPRITE_RAM_START,x
    inx 
    lda objectXPos,y 
    sta SPRITE_RAM_START,x
    inx 
    
    lda objectNext,y 
    tay 
    jmp @SpriteCopyLoopStart

; ok we need to also copy $FE to the rest of the space in case there is sprite data from a deleted sprite that isn't overwritten with new sprite data

    ;so if x is 20 * 5 = 100. which is... $64 
    ; wait var count doesn't matter. cause its just 4 per right? caused its 4 bytes per.
    ; if x is that, then we are at the end of ram space for game objects. so we are done over writing possible junk with $FE
            ; oh idea....
                ; what if we only over write when we delete a game object?
                ; that seems like it either would actually suck 
                    ; only make a difference if i delete a lot of things a lot, or some other niche way the game plays out and im not sure
                    ; or be really cool and quick and smart and make me look like a genius
                ; def worth looking into. I can always count the CCs or might only be able to know after I see how many and how often im creating and deleting game objects.

; when gameobjects have their own write functions that fill a buffer this will be not needed 
@WritingFE:             
    lda #$FE

    ; I "unrolled" this loop. I remember reading about it in hardware class and i saw it online which reminded me about it
        ; i think it works, since each object is 4 bytes, i should be able to write FE in groups of 4 without ever leaving garbage or overwriting where I shouldn't be

    ; this whole part has 0 error checking so far i'm honestly weirdly brain fried right now yolo 
@WritingFELoop:
    cpx #objectMax * 4
    beq @DoneCopyingSpriteData
    sta SPRITE_RAM_START, x         ; I need to write FE ( variable count ) * ( objectMax - Number of Ga,me Objects in Ram )
    inx                                 ; but its 8 times per object slot since we are running ith 8 variables
    sta SPRITE_RAM_START, x     ; wait
    inx                             ; I don't copy over all this data its just the display info so I don't need to do it 8 times or whatever I'm stupid
    sta SPRITE_RAM_START, x
    inx 
    sta SPRITE_RAM_START, x
    inx 
    jmp @WritingFELoop 

@DoneCopyingSpriteData:
    rts 


    DrawEngineJmp2:
    lda objectDrawLo,x 
    pha 
    lda objectDrawHi,x 
    pha 
    rts  