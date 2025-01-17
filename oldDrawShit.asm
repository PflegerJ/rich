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




; removed from main on 1/13/25



; Ok so the next thing I want to add is making the player character be multiple sprites, and then have animations based off those states.
        ; this is going to probably be a major overhaul of how the player works, but again this is the meat of the shit. the game design is the easy part.
; this should be just like every other draw function;
    ; I get the animation offset, and the state? wait no the draw function would care not the drawengine. so here i use state. i didn't with text cause it has 1 state. I guess it could if i had it loaded but turned off.
DrawPlayer:

    ; first we use the state of the player 
    ldx playerState2    ; im using player state 2 until im down reworking all the player shit
    lda PlayerMetaSpriteDataLo,x 
    sta pointerLo
    lda PlayerMetaSpriteDataHi,x
    sta pointerHi

    ; based off the state, we then use the facing direction
    lda playerFaceingDirection
    asl 
    tay 
    lda (pointerLo),y
    sta pointer2Lo
    iny 
    lda (pointerLo),y 
    sta pointer2Hi

    ; and finally based off that, we use the animation counter to finally store the sprite meta data address in pointerLo and pointerHi for the draw engine to write to OAM
    lda playerAnimationCounter
    and #$01
  ;  cmp #$0F
  ;  lda playerAnimationCounter2
 ;   bne @dontSwap 
  ;  clc 
  ;  adc #$01
 ;   and #$01
  ;  ora #$01

@dontSwap:
    asl 
    tay 
    lda (pointer2Lo),y 
    sta pointerLo
    iny 
    lda (pointer2Lo),y 
    sta pointerHi
 
    rts 

PLAYER_TEST_SPRITE = $023C
DrawPlayerBad:
    jsr DrawPlayer
    ldx #$00
    ldy #$00

    lda (pointerLo),y 
    sta PLAYER_TEST_SPRITE,y
    iny 
    
    lda (pointerLo),y 
    sta PLAYER_TEST_SPRITE,y
    iny 

    lda (pointerLo),y 
    sta PLAYER_TEST_SPRITE,y
    iny 

    lda (pointerLo),y 
    sta PLAYER_TEST_SPRITE,y

    rts 


; this is supposed to be a generic game object drawing subroutine
; it will basically be the same as the player
    ; i think I need to have like, another table that has each gameobject type first?
    ; which is that better or should each game object have its own draw function? 
        ; i mean how many types do i have? human? player? text? clock? score? ui? 
; ok fuck the generic. lets draw the clock, could possibly be turned into draw UI or something. which would be cool
FIFTEEN_SECONDS = %01000000

; ok so I have 3 variables holding the offsets for the ROM metasprite tables
DrawClock:

    rts 

; ok lets do this one more time baby
;DrawEngine2:
    
    ; this time we will be iterating through the game object offsets in the offset array
    ; i still need to write the shuffling thing for them..

    ;lda #SPRITE_BUFFER_START    ; reset offset to be at where I want to start putting game object sprite data
   ; sta spriteBufferOffset

   ; ldx #$00
;@DrawEngine2LoopStart:
   ; lda GAME_OBJECT_OFFSET,x 
   ; cmp gameObjectCounter              ; gameObjectCounter is the index of the first empty slot or 1 past end so we iterate till we reach it
   ; beq @DoneDrawingGameObjects
   ; sta currentGameObjectOffset
   ; tax 
   ; sta stupidTemp                  ; why do i write code this fucking tired
  ;  inc stupidTemp  ; please god be sorta right  
  ;  jsr DrawEngineJmp2
  ;  ldx stupidTemp
  ;  jmp @DrawEngine2LoopStart

;@DoneDrawingGameObjects:
   ; rts 



DrawEngine:
    lda spriteBufferOffset
    sta spriteBufferOffsetStart
    lda firstOccupiedSlot
    cmp #objectMax                  ; ok we need to check here if firstOccupiedSlot points to object max. if so then there are no objects so I need to do some extra stuff to make sure I don't mess up the offset
    beq @NoGameObjectsToDraw
    jmp @AtLeastOneGameObject
@DrawEngineLoopStart:
    cmp #objectMax
    beq @DoneDrawingObjects
@AtLeastOneGameObject:
    tax 
    lda objectNext,x     
    sta stupidTemp          ; right now im storing the next index in stupid temp. i wonder if there is some sort of sequence where i don't need to but we shall see
                                    ; so if i wanted to have all the logic here for every type of object. I would need some variable to determine which type it is, to then go to the correct table of draw data
                                            ; that might be better. but for now. im going to have each object have its own draw function that is called but it will return the address of where the data is
                                            ; again i honestly don't know which is better so lets just do this one cause its my idea and see

    jsr DrawEngineJmp       ; this should set pointerLo and Hi to point to the table of spritemeta data
    lda objectXPos,x        ; we store the x and y anchors to use for the relative sprite positions
    sta temp1
    lda objectYPos,x
    sta temp2
    ldy #$00                ; we can use y to get the data with indirect addressing
    ; ldx #$00                ; and x can be used to store the data since we have spriteBufferLo and Hi set
    ; now pointer hi and low should be set to the table of tiles and offsets
    ldx spriteBufferOffset          ; we need to 
@StoringOneSpriteFromObject:
    
    lda (pointerLo),y               ; this should be the y offset. the offest will never be FF so I'm using it as a terminator for sprites
    cmp #$FF
    beq @DoneDrawingThisObject
    clc 
    adc temp2
    sta SPRITE_OAM_START,x
    iny   

    lda (pointerLo),y           ; this is the tile. should just take and place no problem
    sta SPRITE_OAM_START + 1,x  
    iny 

    lda (pointerLo),y           ; this is the attribute. same thing take and place
    sta SPRITE_OAM_START + 2,x
    iny 

    lda (pointerLo),y           ; this is the x offset. add it to fuck. negative numbers. I'll have to use the first byte to determine if its negative or not unless the position is always the top left but idk if that works
    clc 
    adc temp1 
    sta SPRITE_OAM_START + 3,x
    iny 
    inx 
    inx 
    inx 
    inx 
    cpx #$00
    bne @HaveNotLooped2
    ldx #$40                ; starting address for object sprite dtata. but im assuming this will have to change
@HaveNotLooped2:
    ;stx spriteBufferOffset
    jmp @StoringOneSpriteFromObject

@DoneDrawingThisObject:
    stx spriteBufferOffset
    lda stupidTemp
    jmp @DrawEngineLoopStart



@NoGameObjectsToDraw:
    ldx spriteBufferOffset
    jmp @StartClearingGarbage


@DoneDrawingObjects:
    ldx spriteBufferOffset
    cpx spriteBufferOffsetStart
    beq @DoneClearingGarbage
   ; txa 
  ;  clc 
  ;  adc spriteBufferOffset
  ;  tax 
    ;   now we have to fill the rest with FE to clear garbage data
@StartClearingGarbage:
    lda #$FE
@ClearingGarbageLoopStart:
    sta SPRITE_OAM_START,x 
    inx 
    sta SPRITE_OAM_START,x 
    inx 
    sta SPRITE_OAM_START,x 
    inx 
    sta SPRITE_OAM_START,x 
    inx 
    cpx #$00
    bne @HaveNotLooped3 
    ldx #$40
@HaveNotLooped3:
    cpx spriteBufferOffsetStart
    beq @DoneClearingGarbage
    jmp @ClearingGarbageLoopStart

@DoneClearingGarbage:
    txa 
    clc 
    adc #$08
    cmp #$40
    bcs @HaveNotLooped4
    lda #$40
@HaveNotLooped4:
    sta spriteBufferOffset
    rts 

; this we have to assume we have pointerLo set up
; we are also assuming anchorX and anchorY have been set up.
; this also doesn't care about sprite 0. somehow? how do i fix this issue
; so all we have to do is grab the data and place it at SPRITE_BUFFER_START,spriteBufferOffset
; we need to store the new offset
; i made variables currentX and currentY. i feel like im dancing around the really effiencnt cool way to do this but i haven't landed on it quite yet
DrawMetaSprite:
    ldx spriteBufferOffset
    ldy #$00

    ; god why is this so fucking hard
    ; I also need to think about flipping sprites. 
        ; right now i probably only need to flip horizontally
    lda (pointerLo),y   ; y postition
            
    clc 
    adc currentY 
    sta SPRITE_OAM_START,x 
    iny 
    inx 

    lda (pointerLo),y   ; tile 
    sta SPRITE_OAM_START,x 
    iny 
    inx 

    lda (pointerLo),y   ; att
    sta SPRITE_OAM_START,x 
    iny 
    inx 

    lda (pointerLo),y   ; x postition
    clc 
    adc currentX 
    sta SPRITE_OAM_START,x 
    iny 
    inx

    lda (pointerLo),y ; will be FF to say its done 

    rts 

DrawEngineJmp:
    lda objectDrawHi,x 
    pha 
    lda objectDrawLo,x 
    pha 
    rts 
;; ONLY 8 SPRITES CAN SHARE A SCANLINE
DrawTextStatic:
; y is holding the current gameobject ID
    ; this why i can't take these breaks. time to relearn fucking x and y reg with indirect addressing for the 100th time
        ; its ok everytime i learn it i learn it more.... maybe
        ; if it needs to be x i can move it or do something i will figure it out. stop letting everything you don't know stop you
        ; you know nothing so who cares

    ; first this is static. so what do i need. I need to know anchor pos, which objectxpos,y and objectypos,y should fetch.


    
    ; i need to get the address of the table of characters from TextTableLo and TextTableHi using var2 or some bits in var1 as the index to get the right text table address

    ; buffer needs to first have count (which is characters in string * 4) but this would not include white space or newlines unless i do like some text box thingy which is not in this scope rn
    ; I then need to loop through the characters,
    ; putting into a buffer:
        ; y pos that is objectypos + ( 8 * newline chars read )
        ; tile that is take from the texttable
        ; att this is either taken from text table or set as something default
        ; x pos that is objectxpos + ( 8 * char count ( reset every newline char ))


    ; so then how do i fill a buffer?
        ; this is the hard part that hopefully will open up a lot more things to be possible.
    ; im guessing i would have like. a variable that has the start of the buffer. so that is set.
    ; and then a variable that is where the next value goes as an offset.
    ; so like load buffer into pointer2Lo and pointer2Hi. and put data in (pointer2Lo),x where x isd that variable. put the 4 pieces of info in and then repeat for the next char
    ; so then after we read the buffer we reset it by putting the pointer of the end at the start. no need to clear the data... well we would need to put 00 at the end then so it doesn't read some and then see garbage and think its the next count
        ; but zeroing out the buffer sounds not the move. so just setting where its pointing to 00 after each draw func is called s ounds smart. that takes like no time.

    ; i should have the pointer variable for the buffer already loaded at the start of the function that calls all the draw functions.
    ; so it should be as simple as im currently doing with writing to sprite ram. just into the buffer instead. and then from that buffer go to sprite ram.


; so like
    ; i have current game obj ID
    ; i use that to call this function from gameobjectDrawHi and Lo or whatever i named it 
    ; i have some variable that is the offset to which text it is. which would be set when the text is created.
    ; I then do what....
    ; this would make sense to fill a buffer with y tile att x for each letter. then the function that calls this goes through that buffer and puts it into spriteram
        ; for text that never moves i could hard code the locations maybe
    

    ; ok ima just make this one static. and then make one for text tat moves cause i need to start somewhere.
    ; so basically i need to fill a buffer with the this info. and then in the function that calls this one, read that buffer into the correct spot in mem
    ;   cause idk where this will be written here. this is just to get the info and order it so its all ready to be easily copied over.

    ; so we are going to use the x and y pos as the anchor. and then build from there. we can use $00 and $01 to indicate a space and a newline?
        ; since we will have a count so we wont accidently read a $00 as end of buffer.
            ; so the buffer in my head will look like <count>, data, data, data, count, data, count, data, data, data, data, $00 (end of buffer)
                                                    ;   3, data, data, data, 1, data, 4, data, data, data, data, 00 ( the count is 0 i guess which also means end of buffer)
    
    
    
    
    ; lets actually do some shit my girl

    ; lets get the anchor coords and store in what temp1 and temp2?
        ; wait the anchor is just xpos ypos. will need to edit when i add float


    ; ok so i should have the variables be like the offset for the text table. lets say var 2 for now. might have to make it part of var1 depending
    lda objectXPos,x
    sta temp1
    lda objectYPos,x
    sta temp2

    ldy objectVar2,x
    lda TextTableHi,y 
    sta pointerHi
    lda TextTableLo,y 
    sta pointerLo

   ; tya         ; im going to push y on to the stack so i can get it back before leaving because its the gameobject offset and i can't lose it cause linked list
    ;pha 
    ldy #00     ; setting y to 0 to get the count from the text image data table
   ; lda (pointerLo),y   ; this should be the count 
 ;   tax         
    
@DrawTextLoopStart:
    lda #$40
    cmp spriteBufferLo
    bcc @HaveNotLooped
    sta spriteBufferLo
@HaveNotLooped:
    lda (pointerLo),y           ; this is either the y offset or FF to terminate
    cmp #$FF                  ; when the count reaches 0 we are at the end of the text data
    beq @DrawLoopEnd 
    ; we need to get the y pos offset from the text table,
    ;lda (pointerLo),y 
    ; then add it to the anchor y pos (temp2)
    clc 
    adc temp2 
    ; store it in buffer                                ; ok wait. so i need to use an offset to store it in buffer. but i think i have to use y. maybe i can use x? that would be nice but thats my count
    sta (spriteBufferLo),y
    ; inc y 
    iny 


    ; we have to grab the tile from the image data
    lda (pointerLo),y
    ; store that in buffer
    sta (spriteBufferLo),y 
    ; inc y for next tile data
    iny 
    
    ; next is att. im going to grab it from ROM for now
    lda (pointerLo),y 
    ; store it in the buffer
    sta (spriteBufferLo),y 
    iny 

    ; we need to get the x pos from the text table
    lda (pointerLo),y 
    ; add it to the x anchor
    clc 
    adc temp1
    ; store that in buffer
    sta (spriteBufferLo),y 
    iny 
    ; i think thats it 
    jmp @DrawTextLoopStart
@DrawLoopEnd:

    ; at this point. each character should be ready to be displayed. so now i just have to save where the buffer pointer is and then im done
    tya 
    clc 
    adc spriteBufferLo
    sta spriteBufferLo 
 ;   ; oh and get the gameObject offset back off the stack
 ;   pla 
  ;  tay 
    rts 

DrawTextStatic2:
    ; ok this is the improved version
    ; we are going to use the animation offset that all game objects have to know which text table we want to display.
                                        ; random thought, when do animation timers and such get updated? during this draw function? or when i guess its more important with like player character. this is def a thing that i'll understand better when im working on more complicated game objects
    ldy objectAnimationOffset,x
    lda TextTableLo,y 
    sta pointerLo
    lda TextTableHi,y 
    sta pointerHi

    ; this function should either return, or set one of my pointers to the address of the text table
    rts 


 


; so i have no clue how to do this smartly
; my only idea right now is just shifting things like 3 % objectmax indexes? idk
ShuffleGameObjects:

    ; first we need some sort of rng
    lda vblankCounter   ; right now we using the global timer. im not sure what a better way right now if there is one
  ; im too tired ill finish this laters    ; this will make it be within range
    rts 



