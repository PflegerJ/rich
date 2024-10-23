.segment "HEADER"
	
    ; These values tell the emulator what type of cartirdge we are using with what extra features
    ;   the cartridge has. lets start with 0 features and see if we need to learn about them for this game

    ; 16 byte header
	.byte	"NES" 
    .byte   $1A	        ; iNES header identifier
	.byte	$02		    ; 2x 16KB PRG code - lists how much program rom you have
	.byte   $01		    ; 1x  8KB CHR data - lists how much chr rom you have
	.byte   $01         ; mapper 0
    .byte   $00	        ; vertical mirroring off
    .byte   $00         ; iNES_SRAM
    .byte   $00         ; iNES Mapper?
    .byte   $00         ; iNES Mapper?
    .byte   $00, $00, $00, $00, $00  ; padding
.segment "STARTUP"
.segment "ZEROPAGE"
    ;; Variables
    pointerLo:      .res 1  ; pointer variables are declared in RAM
    pointerHi:      .res 1  ; low byte first, high byte immediately after
    jumpLo:         .res 1
    jumpHi:        .res 1
    gameObjectLo:   .res 1  ; pointer variables for storing the game objects in ram
    gameObjectHi:   .res 1
    spriteBufferLo: .res 1  ; pointer to where i'm placing sprite ram for drawing system. lo should be the value of the last object inserted (target address - 1)
    spriteBufferHi: .res 1
    spriteBuffer:   .res 2
    spriteBufferOffset:     .res 1
    spriteBufferOffsetStart: .res 1
    deleteBufferOffset: .res 1
    deleteFlag:         .res 1
    controller1:    .res 1  ; controller 1 byte to store what buttons are pressed each frame
   ; playerXpos:     .res 1  
   ; playerYpos:     .res 1
    nameTable:      .res 1  ; which nametable to load
    roomIndex:      .res 1  ;;;; could make it bits 765, for previous roomIndex. and bits 210 are for current roomIndex?
    spriteCount:    .res 1
    frameCounter60: .res 1
    gameTime:       .res 2
    fifteenSeconds:      .res 1
    score:          .res 1
    
    temp1:          .res 1
    temp2:          .res 1
    stupidTemp:     .res 1
     ; am i dumb or is this needed. like who fucking knows at this point. wait. other way is having tables 
        ; its always fucking tables. 
        ; should i just yolo table style first?
    ; ok yoloing tables

    bathroomFlag:   .res 1  ; this is so dumb but idc right now just bear with me
    flag1:        .res 1    ; bits: X - X - X - X - X - X - NMI Flag - Lag Frame Flag

        ;; should I even have this? or should I keep if scott is active with scott?
        ;; or i guess I could update scott every frame. which is insane. I already feel like having him have a random chance to change rooms every x time 
                ;; not sure how i would do it. like per frame? or every x? i do ahve that frame counter thing for the timers. which I might want to do in a different way now. 
                    ; a more general frame counter. and then the timers use it. instead of it linked with the timers. 

   ; livingRoomFlag:     .res 1  ; bits: X - X - X - X - X - X - Scott Active
        ; kind of fuck this flag idea. i'll keep where scott is with scott....I guess its with the room logic right?
            ; so living room would call ScottLogic, and then scott would be like im not in there byeee, or i am and this what im doing.
                ; and the balcony wouldn't even call it so it doesn't matter.
    ; ok lets just do a scott var then

    scottState:                 .res 1  ;   - States 3 bits - direction facing 2 bits - roomIndex? 3 bits       I think its 3 bit right ( 0 - 7 for values? at least for now)
                                        ;  i also need to save scotts last position? so thats 4 bits right there. might need another byte for that. cause eventually it would be cool to do a RAM thing where i only load in the sprites im using, not just turn them to behind background or whatever 
    controller1PreviousInput:   .res 1
    controller1Pressed:         .res 1
    controller1Held:            .res 1

    playerState:                .res 1 ; Drinking - Smoking - Peeing - Walking - (5-8) Beers in Inv ;; this is so wrong im not sure whats right. but im pretty sure 0 and 1 are facign dir idk which... 
    playerAnimationCounter:             .res 1  
    ;; Constants

    distanceTestValueX:       .res 1
    distanceTestValueY:       .res 1
    distanceTestResult:       .res 2
    scoreIncrementOnes: .res 1
    scoreIncrementTens: .res 1
    beerCount:                   .res 1      ; first half is cigs, 2nd beer bits: 0123 | 4567
    cigCount:                   .res 1

    OBJECT_DELETE_BUFFER  = $30
    PPU_CTRL_REG1         = $2000
    PPU_CTRL_REG2         = $2001
    PPU_STATUS            = $2002
    PPU_SPR_ADDR          = $2003
    PPU_SPR_DATA          = $2004
    PPU_SCROLL_REG        = $2005
    PPU_ADDRESS           = $2006
    PPU_DATA              = $2007

    SPR_DMA               = $4014
    JOYPAD_PORT           = $4016
    JOYPAD_PORT1          = $4016
    JOYPAD_PORT2          = $4017

    SPRITE_RAM            = $0200
    playerXpos            = $0207       ; sprite 1 x pos
    playerTile            = $0205
    playerAtt             = $0206
    playerYpos            = $0204       ; sprite 1 y pos

    timerSpriteYpos       = $0208
    timerSpriteTile       = $0209
    timerSpriteAtt        = $020A  
    timerSpriteXpos       = $020B

    timerSpriteTensYpos     = $020C
    timerSpriteTensTile     = $020D
    timerSpriteTensAtt      = $020E
    timerSpriteTensXpos     = $020F

    timer2Ypos              = $0210
    timer2Tile              = $0211
    timer2Att               = $0212
    timer2Xpos              = $0213

    timer2TensYpos          = $0214
    timer2TensTile          = $0215
    timer2TensAtt           = $0216
    timer2TensXpos          = $0217

    score1sYpos             = $0218
    score1sTile             = $0219
    score1sAtt              = $021A
    score1sXpos             = $021B

    score10sYpos            = $021C
    score10sTile            = $021D
    score10sAtt             = $021E
    score10sXpos            = $021F

    score100sYpos           = $0220
    score100sTile           = $0221
    score100sAtt            = $0222
    score100sXpos           = $0223

    score1000sYpos          = $0224
    score1000sTile          = $0225
    score1000sAtt           = $0226
    score1000sXpos          = $0227

    score10000sYpos         = $0228
    score10000sTile         = $0229
    score10000sAtt          = $022A
    score10000sXpos         = $022B

    distanceTestYpos        = $022C
    distanceTestTile        = $022D
    distanceTestAtt         = $022E
    distanceTestXpos        = $022F

    scottYpos               = $0230
    scottTile               = $0231
    scottAtt                = $0232
    scottXpos               = $0233

    aButtonTestYpos         = $0234
    aButtonTestTile         = $0235
    aButtonTestAtt          = $0236
    aButtonTestXpos         = $0237

    bathroomToiletSpriteStart = $0238
    
    SPRITE_RAM_START        = $0240  ; which sprite ram start is this? god damn it jabes, this is where i put the sprite data from game objects
    SPRITE_BUFFER_START     = $0200     ; this is what I'm using now. its where the sprites for game objects starts
    scottDataStartLo: .res 1
    scottDataStartHi: .res 1
    ;; Game Engine shit
    spriteRamStart = $0300  ; what the fuck is this naming convention? this is where i store game object data
    objectMax = 8
    variableCount = 16
  ;  object_y_pos = spriteramstart + object_max*0
   ; object_tile = spriteramstart +object_max*1
   ; object_att = spriteramstart + object_max*2
   ; object_x_pos = spriteramstart + object_max*3
   ; object_script_lo = spriteramstart + object_max*4
   ; object_script_hi = spriteramstart + object_max *5
   ; object_var_1 = spriteramstart + object_max * 6
   ; object_var_2 = spriteramstart + object_max * 7
    
    ; in the future will I need all these vars? 
    ; i guess will draw functions help reduce the vars needed? or maybe i can just assume Att is 00 or something
        ; but also each sprite won't take up a game object slot.


    ; so part of the overhead of using this system of memory managment is each game object requires an additional variable. which limits how much data they can store
        ; cause right now i can only have 1 var slot
    objectNext = spriteRamStart + objectMax * 0 ; ok we are going to try this implementation i guess
    objectXPos = spriteRamStart + objectMax * 1
    objectYPos = spriteRamStart + objectMax * 2
    objectTile = spriteRamStart + objectMax * 3
    ; objectVar2 = spriteRamStart + objectMax * 3     this should replace tile since we will have a draw function so no tile needed
    objectAtt = spriteRamStart + objectMax * 4
    objectVar1 = spriteRamStart + objectMax* 5
    objectHi = spriteRamStart + objectMax * 6
    objectLo = spriteRamStart + objectMax * 7

    ; these aren't implemented into the game engine side but i want them here while i start thinking about it with scott stuff
    
    objectXPosFloat = spriteRamStart + objectMax * 8
    objectYPosFloat = spriteRamStart + objectMax * 9
    objectDrawHi = spriteRamStart + objectMax * 10
    objectDrawLo = spriteRamStart + objectMax * 11
    objectVar2 = spriteRamStart + objectMax * 12
    objectAnimationOffset = spriteRamStart + objectMax * 13    ; im thinkin
    objectAnimationTimer = spriteRamStart + objectMax * 14
    objectState = spriteRamStart + objectMax * 15
    ; I might need 16 bytes per game object. or use some zero page values to store the info of the current object I'm working on. but like speed and shit i never even considered.
        ; In my shit current game speed isn't a thing. but having 16 ( 15 really cause 1 is next ) variables would give me any wiggle room
            ; but it might just be too much space
        ; I could maybe get away with 12 variables? that would give me 21 game objects instead of 16
        ; but lets not worry about that right now
    firstFreeSlot:      .res 1
    firstOccupiedSlot:  .res 1
    lastOccupiedSlot:   .res 1
    freeRAMStart = $04      ; linked list of Free RAM slots stored 0400 - 04FF?

.segment "CODE"

    ;; Subroutines
vblankwait: 
    bit PPU_STATUS   
    bpl vblankwait
    rts

loadSprites:
    ;lda spriteCount   ; this will be used when each map knows how many sprites it has on load
   ; asl
    ;asl
    ldx #$00
spriteLoop:
    lda sprites, X
    sta SPRITE_RAM, X
    inx
    cpx #$34
    bne spriteLoop
    rts

updateSprites:
    lda #<SPRITE_RAM
    sta PPU_SPR_ADDR
    lda #>SPRITE_RAM
    sta SPR_DMA
    rts

loadpalettes:
    LDA PPU_STATUS
    LDA #$3f
    STA PPU_ADDRESS
    LDA #$00
    STA PPU_ADDRESS
    LDX #$00
loadpalettesloop:
    LDA Palette,X   ; load data from adddress (palette + X)
    STA PPU_DATA
    INX 
    CPX #$20
    BNE loadpalettesloop
    Rts

;;; Using nested loops to load the background efficiently ;;;
loadbackground:
    LDA #%00010000  ; disable NMI, sprites from pattern table 0, background from 1
    sta	PPU_CTRL_REG1		        ; disable NMI
    lda #$00
    sta PPU_CTRL_REG2

    jsr vblankwait

    LDA PPU_STATUS               ; read PPU status to reset the high/low latch
    LDA #$20
    STA PPU_ADDRESS              ; write high byte of $2000 address
    LDa #$00
    STA PPU_ADDRESS             ; write low byte of $2000 address

    ;lda roomIndex
    ;and #%00000111
    ;tax
    ldx roomIndex 
    lda BackgroundLo, x
    sta pointerLo
    lda BackgroundHi, X
    sta pointerHi

    LDX #$00                ; start at pointer + 0
    LDY #$00
outsideloop:

insideloop:
    LDA (pointerLo),Y       ; copy one background byte from address in pointer + Y
    STA PPU_DATA              ; runs 256*4 times

    INY                     ; inside loop counter
    CPY #$00                
    BNE insideloop          ; run inside loop 256 times before continuing

    INC pointerHi           ; low byte went from 0 -> 256, so high byte needs to be changed now

    INX                     ; increment outside loop counter
    CPX #$04                ; needs to happen $04 times, to copy 1KB data
    BNE outsideloop    

    jsr vblankwait

    jsr loadattribute

    LDA #$00
    sta PPU_SCROLL_REG
    sta PPU_SCROLL_REG
        
    LDA #%10010000  ; enable NMI, sprites from pattern table 0, background from 1
    STA PPU_CTRL_REG1
    LDA #%00011110  ; background and sprites enable, no left clipping
    STA PPU_CTRL_REG2
    rts

loadattribute:
    LDA PPU_STATUS
    LDA #$23    ; high byte of $23C0
    STA PPU_ADDRESS
    LDA #$C0    ; low byte
    STA PPU_ADDRESS
    LDX #$00

    ;lda roomIndex
    ;and #$07
    ;tax
    ldx roomIndex 
    lda AttributeTableLo, X
    sta pointerLo
    lda AttributeTableHi, x
    sta pointerHi

    ldy #$00
:
    LDA (pointerLo),y
    STA PPU_DATA   ; write to PPU
    INy 
    CPy #$40    ; copying 64 bytes of data
    BNE :-
    rts

ReadController1:
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016
    LDX #$08
ReadController1Loop:
    LDA $4016
    LSR A           ; Logical shift right - all bits in A are shifted to the right, bit7 is 0 and whatever is in bit0 goes to Carry flag
    ROL controller1    ; Rotate left - opposite of LSR
    ;; used as a smart way to read controller inputs, as when each button is read, the button data is in bit0, and doing LSR puts the button 
    ;; in the Carry. Then ROL shifts the previous button data over and puts the carry back into bit0
    DEX 
    BNE ReadController1Loop
    RTS 

setPlayerStartingPos:
    ldy roomIndex
    lda StartingPosLo, y
    sta pointerLo
    lda StartingPosHi, Y
    sta pointerHi
    ldy #$00
    lda (pointerLo), Y
    sta playerXpos
    iny
    lda (pointerLo), Y
    sta playerYpos
    rts

moveUp:
    dec playerYpos
    lda playerYpos
    clc
    adc #$01
    tay
    ldx playerXpos
    jsr check_background_collision
    beq @checkRightPixel
    inc playerYpos

@checkRightPixel:
    lda playerYpos
    clc
    adc #$01
    tay
    lda playerXpos
    clc
    adc #$07
    tax
    jsr check_background_collision
    beq @noCollision
    inc playerYpos
    rts

@noCollision:
    jsr checkLoadingZone
    lda temp1
    cmp #$FF
    bne @NoLoadingZoneFound
    jsr LoadRoom
   ; jsr loadbackground
@NoLoadingZoneFound:
    rts

moveDown:
    inc playerYpos
    lda playerYpos
    clc
    adc #$08
    tay
    ldx playerXpos
    jsr check_background_collision
    beq @checkRightPixel
    DEC playerYpos

@checkRightPixel:
    lda playerYpos
    clc
    adc#$08
    tay
    lda playerXpos
    clc
    adc #$07
    tax
    jsr check_background_collision
    beq @noCollision
    dec playerYpos
    rts

@noCollision:
    jsr checkLoadingZone
    lda temp1
    cmp #$FF
    bne @NoLoadingZoneFound
    jsr LoadRoom
   ; jsr loadbackground
@NoLoadingZoneFound:
    rts

;; x + 7, y + 1 to deal with position being x (x, y - 1) of where the sprite is drawn
moveRight:
    inc playerXpos
    lda playerXpos
    clc
    adc #07
    tax 
    lda playerYpos
    clc
    adc #$01
    tay
    jsr check_background_collision
    beq @checkBottomPixel
    dec playerXpos

@checkBottomPixel:
    lda playerXpos
    clc
    adc #$07
    tax
    lda playerYpos
    clc
    adc #$08
    tay
    jsr check_background_collision
    beq @noCollision
    dec playerXpos
    rts 
@noCollision:
    jsr checkLoadingZone
    lda temp1
    cmp #$FF
    bne @NoLoadingZoneFound
    jsr LoadRoom
    ;jsr loadbackground
@NoLoadingZoneFound:
    rts 



moveLeft:
    dec playerXpos
    ldx playerXpos
    lda playerYpos
    clc
    adc #$01
    tay
    jsr check_background_collision
    beq @checkBottomPixel
    inc playerXpos

@checkBottomPixel:
    ldx playerXpos
    lda playerYpos
    clc
    adc #$08
    tay
    jsr check_background_collision
    beq @noCollision
    inc playerXpos
    rts

@noCollision:
    jsr checkLoadingZone
    lda temp1
    cmp #$FF
    bne @NoLoadingZoneFound
    jsr LoadRoom
   ; jsr loadbackground
@NoLoadingZoneFound:
    rts 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Checks if player is making a collision with a background tile using the background tile collision map
;;;   
;;;     Formula for looking up if background tile has collisions on: ( X / 64 ) + (( Y / 8 ) * 4 ) =  offset
;;                                      X: player x pos      Y: player y pos
;;
check_background_collision:
    TXA         ; load player x position into A
    lsr         ; divide by 64 -> lsr 6 times
    lsr         ; / 4
    lsr         ; / 8
    lsr         ; / 16
    lsr         ; / 32
    lsr         ; / 64 
    sta temp1    ; store into temp variable

    tya         ; load player y position into A
    lsr         ; divide by 8 -> lsr 3 times
    lsr         ; / 4
    lsr         ; / 8
    asl         ; multiply by 4 -> asl 2 times
    asl         ; * 4

    clc         ; clear carry for adding values together
    adc temp1    ; adding to ( X / 64 )
    sta temp1         ; store value in Y

    TXA
    lsr
    lsr
    lsr
    and #%0111
    Tax

    ldy roomIndex
    lda HitTableLo, y
    sta pointerLo
    lda HitTableHi, y
    sta pointerHi
    ldy temp1
    lda (pointerLo), y
    and bitMask, x      ; beq means not collide         bne means collide

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checks if the player has enterered a loadingzone
;; uses the roomIndex to get the LoadZone
;; loadzone table structure: 
;;                  0th byte is the count
;;                  1st byte is x pos
;;                  2nd byte is y pos
;;                  3rd byte is next roomIndex
;;                  4th byte is starting x pos
;;                  5th byte is starting y pos
;;  
;;  x reg holds the count of loading zones
;;  y reg holds the offset to access the bytes of each loading zone
checkLoadingZone:
    ldx roomIndex           ; uses roomIndex for set pointer to the array of
    lda LoadZoneLo, X       ;   loading zones for the room we are in
    sta pointerLo
    lda LoadZoneHi, X
    sta pointerHi

    lda playerXpos          ; remove last 8 bits - each tile is 8x8 bits so no need to check last 8
    and #%11111000
    sta temp1
    lda playerYpos
    clc
    adc #$01
    and #%11111000
    sta temp2
    ldy #$00                ; y is used as our offset to go through the loop
    lda (pointerLo), y      ; y = 0
    tax                     ; first value in loadingzone table is the counter
    iny                     ; store the counter in x



;; all this shit is a wild mess and will be a pain the ass later. 
;; well its later. not really that much later. but i think its time to fix this bullshit


;; ok. so the main things is. i want the room we are loading into to handle where the player starting pos is, and to call for the background to be loaded and all that.
        ;; so the loading zone should just have its location. check if that matches the player, and then have which room it loads into. thats it? the rest are handled by room unloading subroutine and room loading sub?
        ;; so i could have 2 tables indexed by roomIndex. one for loading and one for unloading. 
checkLoadingLoop:
    lda (pointerLo), y          ; y = 1
    cmp temp1                   ; compare x pos of player with x pos of loading zone
    bne @bottomOfLoopNoYCheck
    iny                         ; increment offset
    lda (pointerLo), y          ; y = 2
    cmp temp2                   ; compare y pos of player with y pos of loading zone
    beq loadingZoneFound        ; both x and y pos must be equal so player is in loading zone
    bne @bottomOfLoop
@bottomOfLoopNoYCheck:
    iny                         ; y = 2
@bottomOfLoop:                  ; increment y if for next loop iteration if there are multiple possible loading zones in this map
    iny                         ; y = 3
    iny                         ; y = 4
    ;iny                         ; y = 5
    ;iny                         ; y = 6
    ;iny 
    ;iny 
    dex                         
    cpx #$00
    BNE checkLoadingLoop
    rts                         ; loop ended no loading zones found

loadingZoneFound:
    iny                     ; y = 3
    lda (pointerLo), y      ; storing roomIndex that loading zone loads into
    and #%00000111
    sta roomIndex
    lda (pointerLo), y 
    and #%11110000      ;; this is cause i can't figure out how to call loadbackground after using the front half of roomindex. i could call it in each rooms specific load function. but i kind of don't want to... will probably change later but fuck ti for now
    lsr 
    lsr 
    lsr 
  ;  lsr 
    sta temp2
   ; iny                        ; Setting starting pos of player based of what loading zone triggered
  ; lda (pointerLo), Y      ; y = 4
   ; sta playerXpos
   ; iny
   ; lda (pointerLo), Y      ; y = 5
   ; sta playerYpos
   ; iny 
    

    ;; ok so i need to call the loading function? of the room. cause just setting the player pos and then loading new background and collisoin is wack. 
    ;; I would say maybe i do room specific things in background, but thats dumb. cause its the laoding trigger that is making shit change. so it should call the shit to change is my guess on how it should work.
   ; lda (pointerLo), y 
  ;  pha 
   ; iny 
   ; lda (pointerLo), y 
   ; pha 
    lda #$FF
    sta temp1
    rts 

    


Timer:
    ;; increase time value each frame
    lda frameCounter60         ; keeps track of how many frames have passed
    clc                         ; increment each frame
    adc #$01
    cmp #$3C                    ; compare with 60 
    beq IncreaseTime            ; 60 frames have passed
    sta frameCounter60         ; 60 frames have not passed. exit subroutine
    rts
 
IncreaseTime:                   ; 60 frames have passed. need to increment seconds
    lda #$00                
    sta frameCounter60         ; first storing 0 to reset 60 frame counter
    lda timerSpriteTile         ; increment the tile
    clc
    adc #$01
    cmp #$0A                    ; compare with 10
    bne LessThan10              ; value less than 10 no need to increment 10s place
    lda timerSpriteTensTile     ; incrementing 10s place 
    clc
    adc #$01
    cmp #$06                    ; compare with 6 to see if a minute has passed
    bne LessThan6               ; branch if minute has not passed
    lda #$00                    ; storing 0 in both 10s and 1s place since a full minute has passed
    sta timerSpriteTensTile
    sta timerSpriteTile
    rts

LessThan10:                     ; 1s place has not rolled over. simply store value and move on
    sta timerSpriteTile
    rts

LessThan6:                      ; 1s place as rolled over, 10s place has not
    sta timerSpriteTensTile     ; store incremented 10s place
    lda #$00                    
    sta timerSpriteTile         ; store 0 in 1s place 
    rts

Timer2:
    lda frameCounter60         ; keeps track of how many frames have passed
    clc                         ; increment each frame
    adc #$01
    cmp #$3C                    ; compare with 60 
    beq @IncrementTime
    sta frameCounter60
    rts

@IncrementTime:
    lda #$00
    sta frameCounter60
    lda gameTime+0
    clc
    adc #$01
    cmp #$00            ; check if rolling over to increment byte 2 of timer variable
    bne @NoRollover
    lda gameTime+1
    clc
    adc #$01
    sta gameTime+1
    lda #$00
    
@NoRollover:
    sta gameTime+0
    and #%00000011
    cmp #$00
    bne @Done
    lda fifteenSeconds
    clc
    adc #$01
    cmp #$04
    beq @SetItToZero
    cmp #$01
    beq @Setting15
    cmp #$02
    beq @Setting30
    sta fifteenSeconds
    lda #$05
    sta timer2Tile
    lda #$04
    sta timer2TensTile
    jmp @Done

@Setting15:
    sta fifteenSeconds
    lda #$05
    sta timer2Tile
    lda #$01
    sta timer2TensTile
    jmp @Done

@Setting30:
    sta fifteenSeconds
    lda #$00
    sta timer2Tile
    lda #$03
    sta timer2TensTile
    jmp @Done

@SetItToZero:
    lda #$00
    sta fifteenSeconds
    lda #$00
    sta timer2Tile
    sta timer2TensTile

@Done:
    rts
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; increases the score
;   Must set scoreIncrementOnes and scoreIncrementTens variable before calling subroutine.
;   Each digit is stored in a sprite tile byte.
;       Clock Cycles    used:
;       memory          used:
;       registers       used: A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IncrementScore:
    lda score1sTile             ; load 1s digit tile
    clc
    adc scoreIncrementOnes      ; add 1s digit score increment to 1s digit tile
    cmp #$0A                    ; checking for rollover
    bcs @Rollover1s             ; branch on carry set (carry set if score1sTile is >= $0A)
    sta score1sTile             ; no rollover, store and move on
    jmp @IncrementScore10s

@Rollover1s:                    ; 1st digit roll over
    sec
    sbc #$0A                    ; subtract 10 to isolate 1s digit
    sta score1sTile             ; store new value in 1s digit
    lda scoreIncrementTens      ; increase the amount adding to 10s digit by 1
    clc
    adc #$01
    sta scoreIncrementTens      ; probably a better way to do this than loading variable, incrementing to then store and then use like 5 lines later

@IncrementScore10s:             ; adding to 10s digit
    lda score10sTile
    clc
    adc scoreIncrementTens
    cmp #$0A                    ; checking for rollover
    bcs @Rollover10s
    sta score10sTile
    rts

@Rollover10s:                   ; 10s digit rollover, increment 100s digit by 1
    sec                         ; first store 10s digit subtracted by 10
    sbc #$0A            
    sta score10sTile

    lda score100sTile           ; increment 100s digit by 1
    clc
    adc #$01
    cmp #$0A                    ; check for rollover
    beq @Rollover100s
    sta score100sTile
    rts

@Rollover100s:                  ; 100s digit rollover, increment 1000s digit by 1
    lda #$00
    sta score100sTile
    lda score1000sTile
    clc
    adc #$01
    cmp #$0A
    beq @Rollover1000s
    sta score1000sTile
    rts

@Rollover1000s:                 ; 1000s digit rollover, increment 10000s digit by 1. no further rollover checks
    lda #$00
    sta score1000sTile
    lda score10000sTile
    clc
    adc #$01
    sta score10000sTile
    rts


DecrementScore:
    lda score1sTile
    cmp scoreIncrementOnes
    bcc @TakeFrom10s                            ; branch if score value is less than decrement value for rollover math 
    sec 
    sbc scoreIncrementOnes
    sta score1sTile
    jmp @SubTensPlace

@TakeFrom10s:
    clc 
    adc #$0A
    sec 
    sbc scoreIncrementOnes
    sta score1sTile

    lda scoreIncrementTens
    clc 
    adc #$01
    sta scoreIncrementTens


@SubTensPlace: 
    lda score10sTile
    cmp scoreIncrementTens
    bcc @TakeFrom100s
    sec 
    sbc scoreIncrementTens
    sta score10sTile
    rts 

@TakeFrom100s:
    lda score10sTile
    clc 
    adc #$0A
    sec 
    sbc scoreIncrementTens
    sta score10sTile
    lda score100sTile
    cmp #$00
    beq @TakeFrom1000s
    sec 
    sbc #$01
    sta score100sTile
    rts 

@TakeFrom1000s:
    lda #$09
    sta score100sTile
    lda score1000sTile
    cmp #$00
    beq @TakeFrom10000s
    sec 
    sbc #$01
    sta score1000sTile
    rts 

@TakeFrom10000s:
    lda #$09
    sta score1000sTile
    lda score10000sTile
    sec 
    sbc #$01
    sta score10000sTile
    rts 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Proximity check
;   checks player proximity to certain objects
;   hoping its based off roomIndex?
;   how can i use pointers to call subroutines...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Proximity:
    jsr Distance1
    lda distanceTestResult + 1
    cmp #$05
    bcc @FirstRange ; i always forget when the stupid carry flag gets set on cmp
    rts 

@FirstRange:
    lda #$00
    sta scoreIncrementTens
    lda #$05
    sta scoreIncrementOnes
    jsr DecrementScore
    rts 
    ; pretend i've calcualted some threshholds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Distance formulas
; 
; Distance1: 
;       Using a^2 + b^2 to find relative distance between player position and distanceTest sprites pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Distance1:
    ; first find a^2
    lda playerXpos
    cmp distanceTestXpos    ; this needs to be changed to scotts sprite? hmmmm
    bcc @DistanceXBigger ; branch if distanceTestX is bigger
    sec 
    sbc distanceTestXpos
    sta distanceTestValueX
    jmp @CalcYDiff

@DistanceXBigger:
    lda distanceTestXpos
    sec 
    sbc playerXpos
    sta distanceTestValueX

@CalcYDiff:
    lda playerYpos
    cmp distanceTestYpos
    bcc @DistanceYBigger ; brance if distanceTestY is bigger
    sec 
    sbc distanceTestYpos
    sta distanceTestValueY
    jmp @Squaring

@DistanceYBigger:
    lda distanceTestYpos
    sec 
    sbc playerYpos
    sta distanceTestValueY

@Squaring:
    lda distanceTestValueX
    sta temp1
    sta temp2
    jsr DumbMultiply

    lda temp1
    sta distanceTestResult
    lda temp2
    sta distanceTestResult + 1

    lda distanceTestValueY
    sta temp1
    sta temp2
    jsr DumbMultiply

    lda distanceTestResult
    clc 
    adc temp1
    sta distanceTestResult

    lda distanceTestResult + 1
    adc temp2
    sta distanceTestResult + 1
    
    rts 



; assume both numbers will be 0 - FF so largest value cannot exceed 2 bytes of storage
; assume both values are in temp1 and temp2 idk the best way to do this and im getting paralyzed with indecision so im yoloing it
; store 2nd byte in temp1?
DumbMultiply:
    lda temp2
    cmp #$00
    beq @MultiZero
    lda temp1
    cmp #$00
    beq @MultiZero
    cmp temp2
    bcc @Temp2Bigger 
    ; if temp1 is bigger i want to use temp2 as the loop counter and store it X
    ldx temp2
    jmp @StartMulti

@Temp2Bigger:
    lda temp2
    ldx temp1
    lda temp1

@StartMulti:
    ldy #$00

@MultiLoop:
    cpx #$01
    beq @MultiDone
    clc 
    adc temp1
    bcs @MultiOverFlow
    dex 
    jmp @MultiLoop

@MultiOverFlow:
    iny
    dex  
    jmp @MultiLoop

@MultiZero:
    lda #$00
    ldy #$00

@MultiDone:
    sta temp1       ; low byte of the result
    sty temp2       ; high byte of the result
    rts 
    ; at this point the larger number should be in A. the smaller number should be in temp1 to use as loop counter


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LivingRoomTestFunction:
    jsr Proximity
    jsr ScottLogic
    ; ok lets just call scott function. and have that function check if scott is there and needs to update/do something or if he not there and we can skip
    rts 
DoAction:
    lda roomIndex
    tax 
    lda RoomBasedEventsHi,x
    pha 
    lda RoomBasedEventsLo,x
    pha  
    rts 

BathroomBasedEvents:
    ; this is so wrong but i will deal. im making the bathroom in charge of the text. and then turn it off. which the sprites should be in charge of themselves
        ; but that would require a lot of code to set up and thats for another time. simple and clean right now

            ; when you walk away
                ; you don't hear me say
                    ; please
                        ; oh baby
                            ; don't go
    rts 
    lda bathroomFlag
    cmp #$00
    beq @BathroomBasedEventsDone
    cmp #$01
    beq @BathroomRemoveText
    sec 
    sbc #$01
    sta bathroomFlag
    jmp @BathroomBasedEventsDone

@BathroomRemoveText:
    ldy #$00
    sty bathroomFlag
    ldx #$00
    lda #$FE

@BathroomLoopStart:
    cpx #$28
    beq @BathroomBasedEventsDone
    sta bathroomToiletSpriteStart, y
    iny 
    inx 
    jmp @BathroomLoopStart

@BathroomBasedEventsDone:
    rts 

DoNothing: 
    rts 

PlayerLogic:
    lda playerState         ; Drinking - Walking - Peeing - Smoking - interacting - X - Facing Direction ( 0: Down    1: Left     2: Up   3: Right )
    asl 
    bcc @NotDrinking        
    jsr DrinkingLogic
    jmp @DoneState
@NotDrinking:
    asl 
    bcc @NotWalking
    jsr WalkingLogic
    jmp @DoneState
@NotWalking:
    asl ; peeing
    asl ; smoking
    jsr StandingLogic
    jmp @DoneState

@DoneState:
    rts 

StandingLogic:

    lda playerAnimationCounter       ; load animation counter: used to determine where in the animation cycle the player is                             
    and #%00001111                   ; animation cycles every 16 frames i guess?
    clc 
    adc #$01                         ; i did it this way cause i was doing other shit that should have been done elsewhere. might have to edit this code i hope it still works for now
    cmp #$10
    sta playerAnimationCounter          
    bne @ControllerChecking          ; not updating sprite
  ;  lda playerTile                   ; flipping between sprites 0 and 1 for now
  ;  eor #%00000001
  ;  sta playerTile

  ; ok using some placeholder bs to make it more complex and show facing angle
    ; i am under the assumption that we can just use whatever register
   ; lda playerState
   ; and #$03            ; isolates facing angle

    ; ok this is a really bad way to do this
        ; BUT
    ; I think that it doesn't fucking matter. cause like. i will have to redo all this shit when i have actual
        ; animation cycles. so who cares. lets get it barebones and gogogo
    
    ; i don't even need the facing angle. all i care about is changing the first 4 bits from 1 to 2 or 2 to 1
        ; based on how the tile map is right now
        ; should I have code that is hyper dependant on the layout of the tilemap? 
            ; probably not but again i'll fix it later
    lda playerTile

    eor #%00110000
    sta playerTile
   ; and #$F0
    ;cmp #$10
    ;beq @changeItToTwo
    ;lda playerTile
    ;sec 
    ;sbc #$10
    ;jmp @ControllerChecking

;@changeItToTwo:
   ; lda playerTile
   ; clc 
   ; adc #$01

@ControllerChecking:                    ; check input
    lda controller1Pressed
    asl     ; a
    bcs @StandingAPressed
    asl     ; b
    asl     ; select
    asl     ; start i need to code more often cause i should remember the fucking order of user inputs. i looked at reference this should be correct now
    asl ; up
    bcs @StandingUpPressed      ; right now i only read one input. also need to incorperate the actual player movement into this instead of having it be in this old function that sucks balls
    asl ; down
    bcs @StandingDownPressed
    asl ; left
    bcs @StandingLeftPressed
    asl ; right
    bcs @StandingRightPressed
    jmp @DoneStanding
@StandingAPressed:
    jsr StandingAPressedTest
    jmp @DoneStanding
    ;jsr Interact   ; might be this call down the road. right now just making sure i can press A while standing and something happens
@StandingUpPressed:
    lda #%01000010
    sta playerState
    jsr WalkingLogicStart
    jmp @DoneStanding 
@StandingDownPressed:
    lda #%01000000
    sta playerState
    jsr WalkingLogicStart
    jmp @DoneStanding
@StandingRightPressed:
    lda #%01000011
    sta playerState
    jsr WalkingLogicStart
    jmp @DoneStanding
@StandingLeftPressed:
    lda #%01000001
    sta playerState
    lda playerAtt
    and #%10111111
    sta playerAtt
    jsr WalkingLogicStart
                                  ; standing animation sprite update
@DoneStanding:        
    rts 

StandingStateStart:
    lda #$00  
    sta playerAnimationCounter
   ; lda #$00                        ; first sprite of standing animation (obvi temp idk what ima do for animation but make it bad at first tand then better lets goooo)
    
    
    lda playerState
    and #$03        ; isolate facing angle
    cmp #$00
    bne @notFacingDown2
    lda #$10
    sta playerTile
    jmp @DoneStandingStateStart

@notFacingDown2:
    cmp #$01
    bne @notFacingLeft2
    lda #$11
    sta playerTile
    jmp @DoneStandingStateStart

@notFacingLeft2:
    cmp #$02
    bne @notFacingUp2
    lda #$12
    sta playerTile
    jmp @DoneStandingStateStart

@notFacingUp2:
    lda #$11
    sta playerTile
    lda playerAtt
    ora #%01000000
    sta playerAtt

@DoneStandingStateStart:
    rts 
DrinkingLogic:
    rts 
DrinkingLogicStart:
    rts 
WalkingLogic:

    ;; check a b start first. those actions take prio over moving
    jsr CheckWeStillWalking
    beq @NotWalkingAnymore
    jsr StillWalking
    jmp @DoneWalking

@NotWalkingAnymore:
    lda playerState     ; not moving so standing state starts
    and #%00000011
    sta playerState
    jsr StandingStateStart
    ;; read no movement input. go back to standing
@DoneWalking:
    rts 

;;;; could i just assume start - select - A - B are all not pressed? cause i was thinking of prioing them. so if they were pressed then i would be in a different state already. 
            ; wait unless they hit A with nothing to interact with. unless I want to make that a state
            ; well lets do it the safe way for now and if i notice that i its always going to be 0000XXXX at this point then yay more clock cycles for other bad code 

;; ok i optimized this funciton. it used to be (p and q) or (s and q). now its (p or s) and q. saved like 8 - 10 clock cycles per call nbd.
CheckWeStillWalking:            ; this function will return 1 if we still walking 0 if we have stopped in the A reg
         ; the value in A has to be 0 already so it works as the return value! yay optimization. im so good at saving 2 clock cycles and terrible at saving hundreds
    lda controller1Pressed    ; 4
    and #%00001111            ; 2
    ora controller1Held       ; 4
    and #%00001111            ; 2
    rts 

    ;; nothing is pressed so go back to standing

    lda playerState
    and #%00000011          ; set player state to standing while preserving facing direction
    sta playerState

@StoppedWalkingStartStanding:
    jsr StandingStateStart 
    rts 
    ; i think its best to check if im still holding the same direction as im facing. 

StillWalking:

    ;;; ok first ima check if any movement buttons were held. cause if so, i can just do the animation 
                ; right now the animation is don't change, but future obvi it will cont whatever.
                ; I think i can also just do the movement and call it a day.  wait. so. when am i changing direction? cause the direction is already set from standing state subroutine.
                    ; so like. and the animation counter has been reset from the startwalking function call right? i should make sure thats how i set it up.
                    ; but like don't i just need to make sure no non-movement buttons were pressed. change playerState accordingly if so, and then
                    ; just continue the animation, and apply the movement of the held and pressed buttons? so not even held or pressed right?
                    ; just like controller1PreviousInput. right? just read that and call it a day? ok lets fucking yolo it and pray.


    ;; ok so it mostly works -  the issue is if you are always holding a d-pad button down, you will never change your facing. so i guess that means you can walk backwards.
            ; and i kind of was doing this so you could straif... maybe it could be with a held button. but you only have a and b. so it would have to be b. maybe b isn't drink...
                ;; idk i only drank in my room. so A could be interact and just be at the computer passively drinking. doing an animation and decrementing inv. upping bladder scoring points.

    ; ok wait. i could just check if a button matches the facing. and if not, hmmm, first try to flip? or check pressed?
    ;; is this even neccisssarry? idk. i was trying to think of way a to not just walk backwards every where cause thats what i've been doing so far as i test..
    ;; maybe I would have to add a straiffing sub state in one of the empty bits in playerState...
    ; to have reduced movement speed. I guess i could just check if the move<Dirction> fucntion im about to call matches the direction.
    ;; if not reduce speed. but thats for another day for sure.



    ;; first animation.
    lda playerAnimationCounter
    clc 
    adc #$01
    sta playerAnimationCounter
    ;; this is where animation shit would go? maybe just have like
    ; jsr WalkingAnimation?
    ;; but I want to at least inc the counter cause i have it so might as well think about it.
        ;; wait maybe this should be at the end... if i want to swap directions I guess...


    ;; ok now just apply the movement?
    lda controller1PreviousInput
    ror ; right?
    bcc @NotMovingRight
    ;; ok im yolo trying to use the stack to save the controller1PreviousInput since im roring bits off it. yolo you know
    pha 
    jsr moveRight
    pla 

@NotMovingRight:   
    ror ; left?
    bcc @NotMovingLeft
    pha 
    jsr moveLeft 
    pla 

@NotMovingLeft:
    ror ; down?
    bcc @NotMovingDown
    pha 
    jsr moveDown
    pla 

@NotMovingDown:
    ror ; up?
    bcc @NotMovingUp
    pha 
    jsr moveUp
    pla 

@NotMovingUp: 
    rts 

WalkingLogicStart:
    lda #$00                    ; first reset animation counter
    sta playerAnimationCounter
    lda playerState             ; load in the player state and then AND it to get facing direction
    and #%00000011
    ; cmp #$00              ; I don't need to cmp here for 0. cause the AND will set the zero flag. and beq branches if the zero flag is 0. i think. or its the other way around and i'll figure out the bug eventually
    beq @WalkingDownStart ; walking down

    cmp #$01
    beq @WalkingLeftStart
    
    cmp #$02
    beq @WalkingUpStart

@WalkingRightStart:

    lda #$83
    sta playerTile
    jsr moveRight
    jmp @WalkingStartDone

@WalkingDownStart:
    lda #$80
    sta playerTile
    jsr moveDown
    jmp @WalkingStartDone

@WalkingLeftStart:
    lda #$81
    sta playerTile
    jsr moveLeft
    jmp @WalkingStartDone

@WalkingUpStart:
    lda #$82
    sta playerTile
    jsr moveUp
    jmp @WalkingStartDone

@WalkingStartDone:
    rts 

StandingAPressedTest:
    ; frist lets just set the sprite tile and position cause this is a test so im not going to initialize it elsewhere thats dumb
    lda aButtonTestTile
    cmp #$70
    beq @aButtonTestInit
    lda #$70
    sta aButtonTestTile
    lda #$90
    sta aButtonTestXpos
    lda #$80
    sta aButtonTestYpos
    lda #$00
    sta aButtonTestAtt

@aButtonTestInit:
    

    jsr CheckTileInFront
    rts 

CheckTileInFront:
    ; i want to check the tile in front of the player to see if that tile contains something the player can interact with
    ; so what do i need to do......
    ; First i need to get the player's position and reduce it into a tile. so what... just remove the last 3 bits? 
    ; where is the actual position? isn't it like 1 pixel above the sprite? 

    ; lets yolo
    ; lets grab players pos, adjust to reflect where the sprite is and remove last 3 bits.
    ; just store into temp1 and temp2 for now

    

    lda playerYpos
    clc 
    adc #$01
    and #$F8
    sta temp2   ; this gets the player y pos, adjusts cause its 1 pixel higher than the sprite, then clears the last 3 digits. 
                    ; i will have to change this cause it will def be weird when trying to interact with something either slightly above or below idk which but one of them for sure
    lda playerXpos
    
    

    and #$F8            ; don't need to adjust y? wait i only need to adjust y
    sta temp1

; this is taking player facing direction into account. so ima store the value as one tile over in the direction the player is facing
        ; again this will have to be updated if i don't AND the last 3 bits to 0 so that i can interact with things inbetween tiles
        ; Facing Direction ( 0: Down    1: Left     2: Up   3: Right )
    lda playerState
    and #$03
    cmp #$00        ; facing down
    bne @notFacingDown
    lda #$08
    clc 
    adc temp2
    sta temp2
    jmp @doneWithFacing

@notFacingDown:
    cmp #$01        ; facing left
    bne @notFacingLeft
    lda temp1
    sec 
    sbc #$08
    sta temp1
    jmp @doneWithFacing

@notFacingLeft:
    cmp #$02        ; facing up
    bne @notFacingUp
    lda temp2
    sec 
    sbc #$08
    sta temp2
    jmp @doneWithFacing

@notFacingUp:
    lda #$08        ; facing right
    clc 
    adc temp1
    sta temp1

@doneWithFacing:
    ldy roomIndex
    lda RoomInteractLo, y
    sta pointerLo
    lda RoomInteractHi, y
    sta pointerHi
    ldy #$00            ; y is now the loop counter thing
    lda (pointerLo), y  ; this grabs the count and can be used at the loop count limit
    tax                 ; so lets put it in x


@interactLoop:
    iny         
    lda (pointerLo), y  ; this is the x pos of the interactable obj
    cmp temp1
    bne @interactLoopXNotMatch
    iny 
    lda (pointerLo), y 
    cmp temp2
    bne @interactLoopYNotMatch
        ; if we here then both x and y pos match
        ; this is where we would call the code for that specific obj. probably store the pointers in the location table
    
    ; interactable object found in front of the player. gets the hi and then low - 1 byte of that objects interact function
    ; pushes it on to the stack and then rts will go to that function 
    iny 
    lda (pointerLo), y
    pha 
    iny  
    lda (pointerLo), y
    pha 
    jmp @interactLoopEnd
    ; ok so temp1 is player x pos rounded to nearest tile, temp2 is adjusted then rounded to nearest tile
@interactLoopXNotMatch:
    iny 
@interactLoopYNotMatch:
    iny 
    iny   
    dex   
    cpx #$00
    bne @interactLoop 
@interactLoopEnd:
    rts 


InteractTestFunction1:
    lda aButtonTestAtt
    eor #%10000000
    sta aButtonTestAtt
    rts 

InteractTestFunction2:
    lda #$02
    sta roomIndex
    jsr LoadRoom
    rts 

ToiletInteract:
                                                                ; so like how the fuck do i have text or something flash on the screen and then go away? 
                                                                ; cause like how do i keep track of how long its on the screen and when it knows to go away?
                                                                    ; couple ideas
                                                                        ; iterate through sprites somehow and do any code that pertains to them. like scott moving or something
                                                                        ; have the room be in charge? cause i think i go through the room code?

    ; so i figured this out ^^^^^^^^^^^^^^^^
    ; fuck all this shit code. it did it's job though so thank you!!!!

    ; ok so what I need to do. right now ima see if i can create the text when interacting with the toilet
            ; i still need to iterate through each game object each frame so ugh i'll set that up next
            ; lets just get it to display like 4 things and then after x time deletes 2, then y time deletes the other 2?
                ; well i guess it doesn't do the delete. 
                    ; I need to have a text box function that each text uses as its game object function
                        ; the generic version can have like, uses a pointer to get all the characters and placements and how long it will stay out/goes away from another source
                            ; wait thats more a generic create function
                        ; generic game loop function would just decrement the counter if it had it.

    ; ok focus on this function

    ; basically we will grab the tables ToiletLetters1 - 4? and make the game objects.

    ; returning here after a couple of days rotting away and wasting my life
        ; did I even do anything last time?
    
    ; ok all this is trash I think I can just remove. I might need to check the bathroom function to see if it breaks but ima have to edit that anyways probably at least who the fuck know idk what code i wrote yesterday so no way i know what the fuck is going on in that function 
   ; lda bathroomFlag
   ; cmp #$00
   ; bne @ToiletInteractDone  ; text is still there. i don't want to reset the text timer cause idc
   ; jsr ToiletInteractSetSprites

    ; so
    ; wait 
    ; that code might not be useless...
    lda bathroomFlag        ; could make this room flag and just be used by whatever room is loaded. 
    cmp #$00
    bne @ToiletInteractDone  ; text is still there. i don't want to reset the text timer cause idc
    ; jsr ToiletInteractSetSprites
    ; fucking a man
        ; should the toilet be a game object? or should it be some static thing in the room. fuck
            ; thats for later but will have to change this depending on my choice but when do i not have to redo code when i learn more better
    
    ; i'll have the text set the bathroomflag back to 0 when they realize they about to be deleted. obvi that a shit way but again. also it isn't that bad when its just 1 game obj instead of 1 per char
    inc bathroomFlag

    ; i need some loop to create the 4 characters
        ; it needs to set pointerLo and PointerHi to the right toiletLettersX
        ; i think thats it? then just make sure toiletLettersX table is formatted correctly
        
    ldx #$00    ; idk which but i haven't used any registers so yolo
    ldy #$00
    ; ok 

    ;; so. i have in ROM. a table with the label ToiletLetters1. that label is for readablitity. computer no see label.
        ; so that's why I have to have store the address of the label somewhere with the .word ToiletLetters1
        ; then when I want to get the data from that table. 
            ; I use < for lo ( left l low ) and > for hi
            ; but I use #. because
                ; because I want the data. right?
                    ; the data is an address which I think is why its confusing
                ; but example:
                    ; .word ToiletLetters1 ( this is storign the address of the label which is the start of the table. )
                                                        ; $A000:    $02     
                                                        ; $A001:    $AB   
                    ; ToiletLetters1: ( this is just a label for me, it is not stored anywhere in mem when assembled. thats why we have either the hi lo tables or the .word of the label somewhere )
                        ; .byte $80, $10, $00, $68
                                                        ; $02AB:    $80
                                                        ; $02AC:    $10
                                                        ; $02AD:    $00
                                                        ; $02AE:    $68
        ; the data at $A000 is the address of the table
            ; so we use # to get the data

        ; i'll fuck this up again but its starting to sink in
            ; refrences and pointers might be easy after this project who knows   
        
        ; literally isntantly i don't get it
            ; why can i just do <ToiletLetterGameLoop no problem?
                ; um
                    ; so. when its a label to a table. its not stored anywhere in mem actually
                        ; but in this case. we are trying to use the not stored memory address,
                            ; we are storing the memory address of the label. 
                                ; if we used # then it would be getting the opcode byte values of the code from the function ToiletLettersGameLoop
                            ; wow way to work through it :)

    lda #<(ToiletLetters1)
    sta pointerLo
    lda #>(ToiletLetters1)
    sta pointerHi
    jsr CreateGameObject2

   
    ;; ok i still need to fix the data tables and create a gameloop function. but it seems like its working


@ToiletInteractDone:
    rts 

;; fuck ima need to make a more streamlined way to display text. cause this sucks.
    ;; def make defines for each tile for each letter cause this is annoying af
.word ToiletLetterGameLoop
.word ToiletLetters1
.word ToiletLetters2
.word ToiletLetters3
.word ToiletLetters4
.word ToiletLetters5
.word ToiletLetters6
.word ToiletLetters7
.word ToiletLetters8
.word ToiletLetters9
.word ToiletLetters10
ToiletLetters1: 
    ;      y  tile  att   x    hi  lo var ?
    .byte $10, $D0, $00, $40, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, >DrawTextStatic2, <DrawTextStatic2 - 1, $FF, $00, $00, $00, $00
ToiletLetters2:
    .byte $10, $D8, $00, $90, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $40 
ToiletLetters3:
    .byte $10, $D9, $00, $A0, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $A0 
ToiletLetters4:
    .byte $10, $D2, $00, $A8, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $A0 
ToiletLetters5:
    .byte $10, $DC, $00, $B0, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $A0 
ToiletLetters6:
    .byte $10, $DC, $00, $B8, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $A0 
ToiletLetters7:
    .byte $18, $D0, $00, $90, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $FF 
ToiletLetters8:
    .byte $18, $D2, $00, $98, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $FF
ToiletLetters9:
    .byte $18, $DB, $00, $A0, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $FF
ToiletLetters10:
    .byte $18, $D5, $00, $A8, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, $FF


;; i honestly need to write the game loop interation function first. cause idk how I'll access the object's variables. if some register should have the offset already
    ; or if i save the current offset to a variable or something.

; but the gist of this dummy version
    ; check var1 which is the timer 
    ; if 0 set bathroomflag to 0 and call to be deleted
    ; if > 0 dec var1 
        ; thats it
    
    ; also fuck im wondering if I should like queue up things that should be deleted
        ; one example:
                ; object A does it's code. it hit object B but also got hit and takes lethal dmg. it deletes itself.
                ; object b does it's code. it was hit by A but also hits A. but A is already deleted so it can't get any info from it
            ; maybe it would work where both objects are updated.
; but that update could affect other shit? this honestly is outside my current knowledge of all the systems required so i'll come back to this way later             

ToiletLetterGameLoop:
    dec objectVar1,x
    lda objectVar1,x 
    cmp #$00
    beq @DeleteThis
    sta objectVar1,x 
    rts 
@DeleteThis:
    lda #$01
    sta deleteFlag
    rts 
    ; so the offset is in x right now
    ; we following the logic in the comments above this function
    lda #$01
    sta bathroomFlag
    lda objectVar1,x
    sec 
    sbc #$01  
    cmp #$00
    bne @StillAlive
    sta bathroomFlag
    jsr DeleteGameObject
    rts 
@StillAlive:
    sta objectVar1,x
    rts 

ToiletInteractSetSprites:
    lda #$3C
    sta bathroomFlag

    ldy #$00


    rts 

Interact:
    ;check if there is something infront of the player that they can interact with
    ; what are ways we can do this?
    ; have a table of locations of interactable things in each room? I feel like that fits with how im setting up everything else?
    ;   so we'll have a table of tables (wow so original). this table will be indexed using the roomIndex to get the address of the table of interactable object locations for each room

    ; first using room index get interactable table address

    ; go through the interactable table for the specific room checking if any of them are in front of the player
        ; what is in front?
        ; i think ima keep everyting on 1 tile so i just check 1 tile in front.
                ; what if im in between tiles? i should have like a range. if im 1 pixel inside a tile, then no. so i guess 
                ; hmmm lets make it stupidly easy. i'll reduce my player pos to the tile. and check the tile in front. i think it will be kind of shitty but that is something for future me to fix
    
    ; so if we get to the end of the list we just rts and call it a day.

    ; if we can interact. how do we handle this?
        ; is the object in charge of handling what happens? hmmmm think simple at first. then when it inevitably is wrong i know why or i have a better understanding of all the things i need to account for
        ; computer
            ; does a menu pop up?
            ; do i do an animation?
            ; does it affect time? or score?
            ; does it change my state?
    
    ; after all that fun stuff i need to make sure to know if i can i need to end playerlogic cause im stuck in that interaction and for how long or if i can keep moving or something
        ; i think im getting ahead of myself. god im just so fried but im glad im at least thinking about it. i'll code it out tomorrow and it will be rough cause i'll have to remmeber everything but thats ok i got this
    rts 

ControllerLogic:
    jsr ReadController1
    
    ; compare controller inputs with previous to determine pressed and held buttons.
    lda controller1PreviousInput    ; storing newly pressed buttons this frame
    eor #$FF    ; bitwise not
    and controller1
    sta controller1Pressed


    lda controller1PreviousInput    ; storing held buttons this frame
    and controller1
    sta controller1Held

    lda controller1
    sta controller1PreviousInput
    rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I do not remember any of this scott code. 
; right now I basically just render or don't render his sprite. 
; what do I need
    ; I need a couple tables, for when he enters. x y pos. facing direction.
    ; I need a table for his pathing
        ; could do like a value for every turn.
        ; and either do like. which way to turn, or have a check when figuring how his next position, and change dir based on that but that sounds more complicated than it needs to be writing it out
    
    ; I need to figure out collision
    ; I need to figure out if he going in objram or his own special spot
        ; the only reason for specialk spot is cause he does shit when off screen ( I want him to continue his pathing to bed even if i'm in my room. )
            ; or another thing is store his posisiton. then when I re enter room, figure out how far he would have traveled in the time but idk if that is worth it
    ; he obviously needs a draw function god
    
    ; like. where does scott go in the order of calls?
        ; does he go in like main? and i check if i should do anything first each time
        ; does he go in specific room stuff? like living room function has jsr scott
        ; does he go in obj ram and the rooms or something just check if he needs to be created?
    
    






ScottLogic:

    ;; check scotts state to see if he is already loaded 
    lda scottAtt
    and #%00100000      ;; this bit should be a 0 if scott is in front of background. i will need to change all this shit if i figure out how to actually unload instead of turn off
    cmp #$00            ;; could possibly skip this step i forget how and sets the flags
    beq @ScottIsActive              ;; scott sprite is "loaded"
    jsr ScottEntrance
    ;; now check if scott should enter

    rts     

@ScottIsActive:
    ;; do actions depending on state
    ;; check if state needs to change? idk if with time? or if flags get set or something

    rts 


;; this should check if scott should enter and maybe handle where he appears? how will i have him walk... i guess i'd need a movement script or something. idk i've just heard that term before idk how to actually implement it
;; right now im just doing it based off time. i really don't know what better way.
ScottEntrance:

    ; i need to figure out what range of numbers ima use. right now time is just what ever it is as proof of concept. and i don't want to balance the time. so since it updates every x frames i could use that. idk ima set it for short intervals and then go from there

    ; check first byte

    ; check 2nd byte

    ; depending on time enter. would multiple rooms call this same function? i need to make sure it works in living room and scottroom i believe.
    rts 
ScottLoad:
    lda scottState
    and roomIndex
    cmp roomIndex       ; i need to figure out the state after the and cause i think i can just branch based off z flag or somethign
    beq @Activate11
    rts 

    ; ok this is trying to load scott into the living room or the bedroom when you enter those rooms. I'm not taking into account scott entering those rooms while you are in them
        ; so i'm thinking. check roomIndex. depending on if its livingRoom or scottRoom, I then check the time. if its between certain ranges I load scott into a set position.
                    ; this set position will have to be updated cause if scott is not in that spot he shouldn't teleport there if i go out and back in but it should be good enough for now
    
    lda roomIndex
    cmp #$01        ; checking if we are loading into the living room
    beq @LivingRoomScottLoad
                    ; must be loading into scotts room if we aren't in the living room and we have called this function
    ; next i need to check the time and see if i'm between the right range. i have no clue what the time variable looks like at any given point in the game. so maybe i need to do some math to see...
    ; i honestly don't remember what the time functions even do at this point. so i guess i should review those and pick one i like and comment out the other just in case its actually better later on
    
@LivingRoomScottLoad:



@Activate11:
    lda #$00
    sta scottAtt
    rts 

ScottUnload:
    lda #%00100000
    sta scottAtt
    rts 



;; function to call to load a room. Assumes the roomindex as already been set
LoadRoom:

    ;jsr loadbackground
    lda roomIndex
    tay 
    lda RoomLoadingHi, y 
    pha 
    lda RoomLoadingLo, y 
    pha  
   ; lda roomIndex
    ;and #%00000111
    sty roomIndex       ; this should clear the doorIndex at the start of the roomIndex byte and then we don't have to clear it everywhere yay
    jsr loadbackground
    rts     ; this rts will jump into the correct RoomLoad function. then the program will return back to the movement fucntion where it saw it had a loading zone hit.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What if... i do the background, hitmap and all the other bullshit in the RoomLoad function instead of having it set in the loadzone table...
    ;; like i don't need player starting pos and ish in that table. just the position of the loading zone and what room they load into?
    ;; cause i could use the roomIndex to know what room i can from to set player pos. then use the new roomIndex to load the background and other crap,
    ;; while also loading in any other sprites (living room load scott if applicable, store load customers and crap)
    ;; that would also make it so its not the loading zone's job to know shit about what happens in the zone it loads into, it just cares hey you touched me this is where you going now)
    ;; cool i think this is better. idk if i'll change it right now but i'll get to it cause i think it will be a headache when more rooms actually have specific shit they doing
LivingRoomLoad:
    ;; ok so. what i need to run scott or something.
    ;; would i call like generic scottLogic func? i don't feel like it. but if i just read if i only call it once then, i guess. no i would call it for every room scott could be in which is 2 but still
    
    ;; ok i need to set player pos. i need to load background and hitmap. i will eventually need to load pallette. i need to load scott.
    jsr ScottLoad
    jsr SetPlayerPositionMultipleOptions
    rts 

LivingRoomUnLoad:
    ;; do i need an unload function? cause like. when do we change what room scott is in? can't be the time function cause he would just disappear. unless i have him walk to his room at a certain time... which would work. but what if i'm not
            ;; in the living room when that would happen? i guess scott doing shit while he is loaded would be handled in scott logic. so i just need to think
            ;; about how to know when he changes rooms when he isn't loaded... which is based on time. so maybe instead of checking his previous roomIndex i check the time? idk.... i need to just yolo something and then fix it later if its wrong cause that is how i work but im kind of stuckkk
    jsr ScottUnload
    
    rts 

;; so im using the roomindex as an index to get the right player starting pos. i get room index, make sure its just the last 3 bits, then get the values at that index for starting pos.
JamesRoomLoad:  
    jsr SetPlayerPositionOneOption
    rts 
JamesRoomUnLoad:
    rts 

BathRoomLoad:
    jsr SetPlayerPositionOneOption
    rts 

ScottRoomLoad:
    jsr SetPlayerPositionOneOption
    rts 

BalconyLoad: 
    rts 
Outside1Load:
    rts 



AddToInventory:
    ; I'm going to make some random assumptions that might hold up but who knows
    ; lets assume that the value we want to add is in y, and either 1 or 0 is in x
    ; maybe lets assume the value we want to add is in temp1. cause i don't think you can add x to a
    ; if x is 0, we are adding beer
    ; if x is 1 we are adding cigs
    ; im just using 2 seperate variables cause idk if i want to cap it at 15 and idc right now cause fuck everyting ima die alone
    cpx #$00
    bne @AddingCigs
    lda beerCount
    clc 
    adc temp1
    cmp beerCount
    bcs @DoneAddingToInventory   ; the carry is set if what is in A is greater than the operand. i think
        ; im just going to not add to inventory if it overflows cause i don't want to deal with it. 
    sta beerCount
    jmp @DoneAddingToInventory

@AddingCigs:  
    lda cigCount
    clc 
    adc temp1
    bcs @DoneAddingToInventory
    sta cigCount
    
@DoneAddingToInventory:
    rts 

SubtractFromInventory:
 
@SubbingCigs:
    rts 

Random:
    rts
;; So i think i need to call this when i load a room. cause like. i don't think the loading zone needs to know the starting location anymore. esp since i have to call the room loading function anyways...
    ;; it makes more sense for the room to know where to load into. i think. but it would have to be based off what room we were coming from...
SetPlayerPositionOneOption: 
    ldy roomIndex 
    lda RoomStartPosLo, y
    sta pointerLo
    lda RoomStartPosHi, Y
    sta pointerHi

    ldy #$00
    lda (pointerLo), y 
    sta playerXpos
    iny 
    lda (pointerLo), Y
    sta playerYpos
    rts 

SetPlayerPositionMultipleOptions:
    lda roomIndex ; im assuming its already set for previous and current rooms? like load zone will set next room and previous room so it should be already what it is. might even already still be in the A reg? no i have to call this function with a pointer
    tay 
    lda RoomStartPosLo, y
    sta pointerLo
    lda RoomStartPosHi, Y
    sta pointerHi


    ldy temp2    
    lda (pointerLo), y 
    sta playerXpos
    iny 
    lda (pointerLo), Y
    sta playerYpos 
    rts 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; oh boy pray for me
; im just yoloing. i made a new branch
; ok so i have 4 temp vars now. im not sure how to like. make this the best obvi but make it at all
; i have to set all the positioning and variables and tile and att and script mem address
; each game object has 8 bytes to it right now. with 4 temp vars. idk if i need 4 more temp vars so i can just temp1-8 or what is best
; that is what makes sense to me at the moment, because i read somewhere you shouldn't pass variables on the stack so like how else
; am i supposed to do it unless i write 2 functions that do 4 things each
; if i run out of memory i will adjust. lets get our hands dirty 

; learn by fucking doing my girl

; ok here is the plan. we are using tables big surprise. so what we are going to do is assume that we have either
; fuck
; ok option 1 is assume pointerLo and pointerHi have been set before the call. that makes the most sense to me?
; option 2 is that hi and lo address is stored in register or temp1 and temp2
; buts lets roll with option 1 and see why it doesn't work instead of decision paralysis



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




; ok i took way too long of a break
; i think i need to restart this game engine shit from scratch

; ok creating a gameobj in ram
; i need to take the data from sometable in rom and put it in ram
; i need to keep track of where im putting the next game object
; i need to keep track of what blocks in obj ram are being used to iterate through


; this is assuming I have the pointer pointing to where I'm going to place the data
; and assuming I have the pointer pointing to where the data is im going to store


; this function should run at start up. and should set up all the free slots in objectRAM to point at the next slot so
; when i need to put an object in RAM i just take the first free slot in this linked list and have next free slot point to the first slot's NEXT

; so each object in this linked list has 2 things
    ; the index of where the object starts in ram 00 - FF?
    ; the index of next

; picking random place to put this shit? 0400 work?
; I have a FreeRamFirst pointer?
; and I have freeRAMStart defined as 04
InitializeGameObjectRam:

    ; spriteRamStart = $0300
    ; objectMax = $20  
    ; objectNext
    ; firstFreeSlot:      .res 1
    ; firstOccupiedSlot:  .res 1
    ; lastOccupiedSlot:   .res 1

; ok so we are going back to the old method i first read about using the offset stuff whatever god im so fucking hungry i can't think
; 
; 
    
    ldy #$00
    lda #$01
    sty firstFreeSlot   ; setting firstFreeSlot as 0 because that is the first free slot at startup. 
    ; then lets do the loop and fill each objectNext value to point at the next object
    ; if an obj points to objectMax then it points to null

    ; objectMax is the same thing as Null for this linkedList!!!!
    ; do a loop setting each objectNext to equal 1 more than that object's num until we reach the end which is max_objects or whatever which is like 20 right now. so pointing to null is pointing to maxObjects + 1 since there are no negative numbers.

@StartInitGameObjRamLoop:
    ; we are going to store a at objectNext,y
    ; so objectNext,0 is at $0300. It's "obj ID" is 0 or the offset. and will set its next as +1
    sta objectNext,y 
    clc                 ; incrementing a and y. so next loop we store 2 at objNext,1 all the way to storing 20 at objNext,19 but not really 19 whatever 19 is in hex
    adc #$01
    iny 
    cmp #objectMax       ; if a is the same as objMax then we have reached the end? or do we need to do one more time to put 20 at slot 19? i think we need one more time
    bcc @StartInitGameObjRamLoop

    sta objectNext,y    ; this is storing the value ObjectMax in the last object's next value which is the same as it pointing to null and represents the end of available memory

    ; I also need to set firstOccupiedSlot and lastOccupiedSlot as objectMax (null) and that really should be it for init the game obj memory.
    ; if this logic works then we gucci please logic be right

    sta firstOccupiedSlot
    sta lastOccupiedSlot
    rts 



; This logic should work for adding. I am now working on deleting which may cause some bugs here when the list gets populated, then deleted and then repopulated and then deleted.
; im mostly concered with the list getting back to empty and making sure there are no breaks in the linkedlist 


; this is purely for getting the memory management down. I'm not caring about any variables or pointers or whatever. we are just going to call this along side deletegameobject
; to make sure that shit is added and deleted and then properly iterated through. 

; also not sure about trying to add to a full list or adding last possible element



;; ok now its time to think about adding the actual game object data to the other variables. the main thing is I need to know what my offset is which is what the whole linked
        ; list bullshit is about. but after i figure out where I'm putting it and make sure all the pointers are pointing, I can use that slot as offset to just blast through
        ; the table that some pointer is pointing at and move it to ram.
 CreateGameObject2:
    lda firstFreeSlot
    cmp #objectMax
    beq @DoneAddingNewElement   ; if firstFreeSlot is pointing to objectMax then the list is full and we can't add more objects to it.
    ; i feel like to make a general function. we need to check if the first and the last both equal 0? or each other?
    ; wait thats dumb firstOccupiedSlot would be null thats all we have to check

    ; checking if firstOccupiedSlot is equal to ObjectMax to see if RAM is empty

    ; might also need to check if first and last are equal to see if list is empty or full?

    ;; i could also add the data here. before i do anything with pointers. not sure which is better but i can optimize later. lets do it here so i don't have to think
    ; lets just do it here
    ; we doing dumb shit. the order before actually setting all the variables
    ; y pos | tile | att | x pos | game routine hi | game routine lo | draw function hi | draw function lo | var 1 | var 2 | animation offset | animation timer | object state
    ldx firstFreeSlot           ; this is the offset for putting the data
    ldy #$00                    ; this is the offset of the data table we grabbing the data from
    lda (pointerLo),y   ; y pos
    sta objectYPos,x 
    iny 
    lda (pointerLo),y   ; tile
    sta objectTile,x 
    iny 
    lda (pointerLo),y   ; att
    sta objectAtt,x 
    iny 
    lda (pointerLo),y   ; x pos
    sta objectXPos,x
    iny  
    lda (pointerLo),y   ; HI byte of gameLoop function
    sta objectHi,x 
    iny 
    lda (pointerLo),y   ; Lo
    sta objectLo,x 
    iny 
    lda (pointerLo),y   ; draw function hi
    sta objectDrawHi,x 
    iny 
    lda (pointerLo),y   ; drawfunction lo
    sta objectDrawLo,x 
    iny 
    lda (pointerLo),y   ; the one variable my objects have right now
    sta objectVar1,x 
    iny 
    lda (pointerLo),y 
    sta objectVar2,x    ; var 2
    iny 
    lda (pointerLo),y 
    sta objectAnimationOffset,x    ; animation offset
    iny 
    lda (pointerLo),y 
    sta objectAnimationTimer,x    ; animation timer
    iny 
    lda (pointerLo),y 
    sta objectState,x    ; state
    iny 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;objectNext = spriteRamStart + objectMax * 0 ; ok we are going to try this implementation i guess
    ;objectXPos = spriteRamStart + objectMax * 1
    ;objectYPos = spriteRamStart + objectMax * 2
    ;objectTile = spriteRamStart + objectMax * 3
    ;objectAtt = spriteRamStart + objectMax * 4
    ;objectVar1 = spriteRamStart + objectMax* 5
    ;objectHi = spriteRamStart + objectMax * 6
    ;objectLo = spriteRamStart + objectMax * 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; we need 
        ; objectVar1
        ; objectHi
        ; objectLo
        ; objectVar2

    ; I don't need to decide on an order yet. lets make sure I can create and delete and iterate game objects with this bullshit first
        ; But i will need to decide on an ordering
        ; fuck do I need to start doing documentation?
            ; is that me? I feel like i always start it and then don't update it and then its too much to update and it will just change so I might as well just wait till im done to do it 
                ; yeah that is more me
            ; ok random comment buried in the code it is    

    ; I THINK THIS IS RIGHT? just taking fromt table and placing into var.



    lda firstOccupiedSlot
    cmp #objectMax
    beq @TheCurrentListIsEmpty           ; not sure if i should branch if equal or if not equal yet. lets see what things have to happen and what order would make more sense



    ldx lastOccupiedSlot                 ; this is setting up offsets for adding nth element
    lda firstFreeSlot
    sta objectNext,x                        ; this is storing the newely added element's slot as the next value of the previous end of the list's next value
    jmp @TimeToAddNewElement

@TheCurrentListIsEmpty:
    lda firstFreeSlot
    sta firstOccupiedSlot 
    ; this is assuming we will use y as the offset to add all the data to the the other variable tables  
    ; this should work for both adding 1st and adding nth element. only difference is how to load x and y registers since lastOccupiedSlot is null when empty

@TimeToAddNewElement:
    ; before we use y register. we should add all the data from the pointer to the variables. cause we haven't needed to move any pointers around
        ; x is lastOccupiedSlot but we can re ldx to get that value back. and we are placing in firstFreeSlot. so we can do all the adding using y or x to offset the
            ; pointer for the data. then reset x and y for doing the pointer moving to keep the lists working
    

    ldy firstFreeSlot  
    lda objectNext,y        ; this is storing the current FirstFreeSlot's next value as the new firstFreeSlot
    sta firstFreeSlot

    lda #objectMax
    sta objectNext,y        ; this is storing null in the slot we are adding the new element to. because we always add to the end so it should point to null
      
    sty lastOccupiedSlot    ; this is setting the lastOccupiedSlot as firstFreeSlot cause thats where we put the newely added element

@DoneAddingNewElement:    
    rts 


; so for deleting it should be maybe easy? what we need is the object number we are deleting. which is just its slot number so it should be easy. i think the tricky part is connecting
; the list back together. cause 1 -> 2 -> 3. if we are deleting 2 we need 1 to point to 3. which means we might have to walk down the list until we reach 1 before 2. so O(n) instead of 0(1) of inserting



; THIS ASSUMES X REG already has the slot we are deleting
DeleteGameObject:

    ; WE ARE ASSUMING X is the game object we are deleting?
    ; so what are the scenarios?

    ; deleting first element (n) (order probably wrong)
    ; also didn't consider if deleting object is before or after the current FOS
        ; FOS -> FOS.next
        ; if n < FFS: FFS -> FOS
        ; FOS.next -> FFS

    ; n = FirstOccupiedSlot

    ; x reg is going to hold the slot we are deleting
        ;ldx firstOccupiedSlot       ; this could also be n instead which it probably should be
    cpx firstOccupiedSlot
    bne @DeletingMiddleOrLast
    ;; THIS IS FOR DELETING FIRST OBJECT IN LIST
@DeletingFirstElementInList:                                                                                                     
    ldy firstFreeSlot

    lda objectNext,x        ; FOS -> FOS.next
    sta firstOccupiedSlot
    cpx firstFreeSlot
    bcs @TargetGreaterThanFSS
    
    stx firstFreeSlot       ; if n < FFS: FFS -> FOS   
    tya 
    sta objectNext,x        ; FOS.next -> FFS
    jmp @DoneDeleting

@TargetGreaterThanFSS:
    lda objectNext,y        ; FOS.next -> FFS.next
    sta objectNext,x 

    txa 
    sta objectNext,y         ; FFS.next -> FOS
    jmp @DoneDeleting

@DeletingMiddleOrLast:
    ; ok so we need to iterate through the list and set n.previous.next to n.next
    ; we also need to add this slot into the free slot list
    ; so we need to set up prev which we will use temp? could also possibly use a reg but i think thats worse
    ; we also can assume we are not deleting the first element. so we can start looking at FOS.next and use FOS as prev
    
    stx temp1               ; temp1 is target
    ldx firstOccupiedSlot   ; setting x to be prev


    ; i think i can move this down into the loop and remove the bottom ldy since its the same thing but i'll do that after it works
    ldy objectNext,x        ; setting y to be current ( we start 1 element in since we KNOW we aren't deleting the first element )                       ; 
@WalkingThroughElementListStart:
    ; first we must compare current to target    
    tya 
    cmp temp1
    beq @TargetFound

    ; target is not found lets iterate to next
    tax 
    ldy objectNext,x
    jmp @WalkingThroughElementListStart 
@TargetFound:
    ; i need to move prev.next to current.next
    lda objectNext,y   ; getting curr.next
    sta objectNext,x    ; storing curr.next in prev.next

    cmp #objectMax
    beq @UpdateLastOccupiedSlot
    jmp @UpdateFreeSlotList

@UpdateLastOccupiedSlot:
    stx lastOccupiedSlot
    ; i also need to add the deleted slot into the freeSlot list and update firstFreeSlot if applicable

@UpdateFreeSlotList:
    ; so honestly i shouldn't keep track of keeping FFS lower for literally no reason
    ; so really all I have to do when I delete something, is just add it to the front of FFS

    ; so i make the target point to FFS
    ; and then set FSS as the target
    lda firstFreeSlot
    sta objectNext,y    ; setting the target.next to point to the old FFS     
    sty firstFreeSlot


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAVING THIS INCASE I FUCK IT UP TRYING THE OTHER IMPLEMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    
    ; target < FFS
    ; first we need to say target.next points to FFS
        ; y is current and is the target since we have found target
        ; and a might be target but lets just do each piece isolated and then work out the order and handling of registers
   ; lda firstFreeSlot
    ;cmp temp1           ; we need to branch depending on if target is before or after the current FFS
   ; bcc @FSSLessThanTarget
   ; sta objectNext,y
   ; sty firstFreeSlot     ; then we can move FFS to target and that should be it
   ; jmp @DoneDeleting

;@FSSLessThanTarget:
    ; target > FFS

    ; could maybe just do tax since we lda firstFreeSlot and wouldn't have changed it by this point
  ;  ldx firstFreeSlot   ; need to use FFS as an offset to get it's next so we have to use y or x. and since y is also target its already stored in temp1 if we need it
                        ; wait maybe its better to use x. since we might not need to care about prev if we already set up those pointers. 
  ;  lda objectNext,x    ; a now has FFS.next
   ; sta objectNext,y    ; storing old FFS.next as target.next
   ; tya 
   ; sta objectNext,x    ; storing target as new FFS.next
    ; I also need to update lastOccupiedSlot if applicable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@DoneDeleting:
    rts 


; so. i need to take into account where to start. cause things like timers and shit. unless i load those into ram but idk if i need to. i think they are their own thing?


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


; the big boy
; what do i need to do:
    ; start at first occupied slot
    ; get objectHi and objectLo and jmp to it using pha and pho? fuck what the other one i'll find is somehwere its pla
    ; which would go to that objects game loop function when i rts....
        ; so i would not go back to this function then...... i guess thats why they have the jump engine? cause anytime you need to do something like this.
            ; you can jsr to the jump engine. then pha the address of where you want to go. then when you rts you go there
                ; and then when you rts from there, you go back to the function that called jsr jumpengine. its almost like you need that middle man.
    
    ; i guess i could have just a little inner function. kind of like recurrsion. well only in the sense there is a smaller header function called and then it insta goes into the helper function that is the majority of the code

GameEngine:
    lda #$00
    sta deleteBufferOffset
    ; lets think about this sans jump engine. cause i think that is not what i need for the way this is set up. might be the thing i need later when im smarter. but right now my code aint set up for that bullllshit

    ; so. we need to iterate through all the game objects. and run their specific code.
        ; what makes sense to me. is loop through using gameObjNext != objectMax, then get there address and rts to it.
            ; so loop will call like jsr do game obj if we aren't at gameobjmax. so when that function rts's it comes back here? where we can keep looping
                    ; that makes sense to my dumb brain so thats what we going to do
    
    ; things i need to consider:
        ; what state the registers are in
        ; do I need to store the offset of the game object somewhere ( oh god i haven't thought about collisions or physics kill me )
        ; one thing at a time. lets get this working so i can keep going

    ; this is all just game object stuff. i'll figure the rest out later. gameobj iteration is the focus
    lda firstOccupiedSlot
@StartIteratingThroughGameObjects:

    cmp #objectMax
    beq @DoneIteratingThroughGameObjects
    ; so right now A contains the gameObject offset.
    tax ; now this is podracing. tax before cause we going to have to tax after too :)
    stx stupidTemp
    ;lda objectNext,x
    ;sta stupidTemp
    jsr GameObjectIteration ; this function will set up the pointer for the gameobject code and then rts. which will take it to that function, and then return back here
    lda deleteFlag
    cmp #$00            ; could maybe do a shift and branch on carry or whatever for optimization
    beq @DontDelete    
   ; lda objectNext,x    ; why are linked lists so sexy?
    lda stupidTemp
    ldx deleteBufferOffset
    sta OBJECT_DELETE_BUFFER,x
    inx 
    stx deleteBufferOffset

@DontDelete:
    ldx stupidTemp ; i have to do this cause im deleting the game object and then that fucks up the linked list
    lda objectNext,x
        ; so i'll def need a buffer to store the objects i want to delete and do that at the end
    jmp @StartIteratingThroughGameObjects

@DoneIteratingThroughGameObjects:       ; think thats literally it. for code here. 

    rts 



; so we have... the object's offset in x cause we are gamers
; if this is reused slightly differently i feel like it should be easy to make it more generic 
GameObjectIteration:
    lda objectHi,x ; push hi byte first
    pha 
    lda objectLo,x 
    pha 
    ; now rts should take us to the game objects code
        ; when that rts it should go back to GameEngine subroutine
    rts 

; ok do i make this for gameloop or do i try and make it generic. would making it generic benefit any of the functions that use the stack to jump to a function from an address table
    ; right now. i want it to work
    ; i don't think generic is that hard. i also annoyingly have seen the code for this so i feel kind of icky but whatever. i did organically come up with the idea even if i knew of the concept before hand
    ; still i like doing the discovery. i don't want to copy i want it to be my own but whatever
JumpEngine:
    ; i think its too late to start.
    
    ; but basically what.

    ; so i use some index to figure out what im jumping to which should be in A
    ; the jump addresses are either 
        ; if i taking the idea from mario then i take the return address and use that with the index
        ; not sure if there is a different way i'll need to think
            ; mostly just i need to structure everthing around this.

    ; if i pull the return address off the stack, i either need to re-push it back or i think what they do is store it as in a pointer. which im not sure why

    ; we are assuming that we are going to take the return address, use it as the pointer to grab the actual jmp address and then jump to it.
        ; assuming the offset is already in A
    asl     ; this shifts the index over 1 bit ( index * 2 ). this is because each address is 2 bytes, so if we want the 2nd address, the index would be 1 (cause 0 is for 1st). then * 2 to get index = 2 since its the 3rd byte we need. it makes sense why am i writing it out so poorly
    tay     
    pla     ; pulling first byte of return address on the stack. hihg byte on the stack first. then low so low is pulled first
    sta pointerLo   ; is the high or low byte on the stack first?
    pla 
    sta pointerHi
    iny                 ; you have to increment cause whats on the stack is -1 of the address of the next value. so we need to go up 1 to get to the address following the jsr jumpengine
    lda (pointerLo),y
    sta jumpHi          ; i think this is wrong. cause it seems like they store hi then low. but like my pointers always be lo then hi but is jump different?
    iny 
    lda (pointerLo),y
    sta jumpLo          ; god the amount of code i'll need to write to properly test this is bullshit
    jmp (jumpHi)
    rts 



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
    sta SPRITE_BUFFER_START,x
    iny   

    lda (pointerLo),y           ; this is the tile. should just take and place no problem
    sta SPRITE_BUFFER_START + 1,x  
    iny 

    lda (pointerLo),y           ; this is the attribute. same thing take and place
    sta SPRITE_BUFFER_START + 2,x
    iny 

    lda (pointerLo),y           ; this is the x offset. add it to fuck. negative numbers. I'll have to use the first byte to determine if its negative or not unless the position is always the top left but idk if that works
    clc 
    adc temp1 
    sta SPRITE_BUFFER_START + 3,x
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
    sta SPRITE_BUFFER_START,x 
    inx 
    sta SPRITE_BUFFER_START,x 
    inx 
    sta SPRITE_BUFFER_START,x 
    inx 
    sta SPRITE_BUFFER_START,x 
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


DeleteEngine:
    ldx #$00
@DeleteEngineLoopStart:
    cpx deleteBufferOffset
    beq @DoneDeletingGameObjects
    lda OBJECT_DELETE_BUFFER,x
    inx  
    stx stupidTemp
    tax 
    jsr DeleteGameObject
    ldx stupidTemp
    jmp @DeleteEngineLoopStart
@DoneDeletingGameObjects:
    rts 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RESET:
 
    sei			; disable IRQs
	cld			; disable decimal mode
	ldx	#$40
	stx	$4017		; dsiable APU frame IRQ
	ldx	#$ff		; Set up stack
	txs			;  +
	inx			; now X = 0
	stx	PPU_CTRL_REG1		; disable NMI
	stx	PPU_CTRL_REG2		; disable rendering
	stx	$4010		; disable DMC IRQs

	;; first wait for vblank to make sure PPU is ready
    jsr vblankwait

    TXA 
clearmem:
    STA $0000,X
    STA $0100,X
    STA $0300,X
    STA $0400,X
    STA $0500,X
    STA $0600,X
    STA $0700,X
    LDA #$fe
    STA $0200,X
    LDA #$00
    INX 
    BNE clearmem 

    JSR vblankwait

    LDA $02     ; high byte for sprite memory
    STA $4014
    NOP 

clearnametables:
    LDA PPU_STATUS   ; reset PPU status
    LDA #$20
    STA PPU_ADDRESS
    LDA #$00
    STA PPU_ADDRESS
    LDX #$08
    LDY #$00
    LDA #$24    ; clear background tile
:
    STA PPU_DATA
    DEY 
    BNE :-
    DEX 
    BNE :-

    jsr InitializeGameObjectRam
    

    ; this needs to be put in a initialize funtion my girl
    lda #$02
    sta spriteBufferHi
    sta >spriteBuffer
    lda #$40
    sta spriteBufferLo
    sta <spriteBuffer
    lda #$00
    sta deleteBufferOffset
    jsr loadpalettes 

    lda #$40
    sta spriteBufferOffset

    jsr loadSprites
    jsr updateSprites

    jsr loadbackground


    ; setting DMA pointer

        ; ok so lets try and initialize the spriteRam pointer
 ;   lda spriteramstart
   ; sta FirstFreeSlot      ; i think this is really stupid and wrong
   ; lda #$00
    ;sta NextFreeSlot

    ; so lets set the hi byte of the pointer to $03, and then the first and next slot to $00
    ; that should make it so when we add to next, it will start at address $0300, and then will be pointing at $0308, while first still points at $00, tbh idk if we need a first then... im kind of confused on why
    lda #<spriteRamStart
    sta gameObjectLo
    lda #>spriteRamStart
    sta gameObjectHi


;asdfasdf
     CLI 
    LDA #%10010000  ; enable NMI, sprites from pattern table 0, background from 1
    STA PPU_CTRL_REG1
    LDA #%00011110  ; background and sprites enable, no left clipping
    STA PPU_CTRL_REG2

    LDA #$00    ; reset scroll address 
    STA PPU_SCROLL_REG
    sta PPU_SCROLL_REG
    

Main:
    lda flag1
    and #%00000010          ; isolating nmi flag
    cmp #$00                ; nmi flag 0: nmi hasn't happened yet keep looping  1: nmi has happened start game logic for next frame
    beq Main

    lda flag1
    ora #%00000001          ; set lag frame flag
    and #%11111101          ; clear the nmi flag
    sta flag1

    jsr Timer2
    jsr ReadController1
    jsr PlayerLogic
    jsr GameEngine

    ldx #$02

    jsr DoAction

    jsr ControllerLogic

    jsr DeleteEngine
    jsr DrawEngine

    ;jsr CopyObjectRamToSpriteRam

    lda flag1           ; sets lag frame flag to 0: means game logic done and can update spriets on next vblank
    and #%11111110  
    sta flag1
    jmp Main

;;;;;; vblank loop - called every frame ;;;;;
VBLANK:
    lda flag1           ; checking if lag frame: 0: Not lag frame, draw updates     1: Yes lag frame, return to let shit code finish before drawing
    and #%00000001
    cmp #$00
    beq @VblankIsAGo
    rti 

@VblankIsAGo:
    lda flag1           ; setting nmi flag
    ora #%00000010  
    sta flag1

    jsr updateSprites
    ;jsr Timer

    
    RTI 


Palette:
    .byte $22, $29, $1a, $0F, $22, $36, $17, $0F, $22, $30, $21, $0F, $0f, $0f, $0f, $0F  ; background palette data
    .byte $22, $16, $27, $18, $22, $1A, $30, $27, $22, $16, $30, $27, $0f, $0F, $0f, $0f  ; sprite palette data 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Pointer Tables
;
;   roomIndex order
;       0: James    1: living   2: scott    3: bathroom    4: balcony   5: store    6: outside1   7: outside2 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BackgroundLo:
    .byte <JamesRoomBackground, <LivingRoomBackground, <ScottRoomBackground, <BathroomBackground, <BalconyBackground, <StoreBackground, <Outside1Background, <Outside2Background
BackgroundHi:
    .byte >JamesRoomBackground, >LivingRoomBackground, >ScottRoomBackground, >BathroomBackground, >BalconyBackground, >StoreBackground, >Outside1Background, >Outside2Background

AttributeTableLo:
    .byte <JamesRoomAttribute,  <LivingRoomAttribute, <ScottRoomAttribute, <BathroomAttribute, <BalconyAttribute, <StoreAttribute, <Outside1Attribute, <Outside2Attribute
AttributeTableHi:
    .byte >JamesRoomAttribute,  >LivingRoomAttribute, >ScottRoomAttribute, >BathroomAttribute, >BalconyAttribute, >StoreAttribute, >Outside1Attribute, >Outside2Attribute

HitTableLo:
    .byte <JamesRoomHitTable, <LivingRoomHitTable, <ScottRoomHitTable, <BathroomHitTable, <BalconyHitTable, <StoreHitTable, <Outside1HitTable, <Outside2HitTable
HitTableHi:
    .byte >JamesRoomHitTable, >LivingRoomHitTable, >ScottRoomHitTable, >BathroomHitTable, >BalconyHitTable, >StoreHitTable, >Outside1HitTable, >Outside2HitTable

LoadZoneLo:
    .byte <JamesRoomLoadZone, <LivingRoomLoadZone, <ScottRoomLoadZone, <BathroomLoadZone, <BalconyLoadZone, <StoreLoadZone, <Outside1LoadZone, <Outside2LoadZone
LoadZoneHi:
    .byte >JamesRoomLoadZone, >LivingRoomLoadZone, >ScottRoomLoadZone, >BathroomLoadZone, >BalconyLoadZone, >StoreLoadZone, >Outside1LoadZone, >Outside2LoadZone

StartingPosLo:
    .byte <JamesRoomStartPos, <LivingRoomStartPos, <ScottRoomStartPos, <BathroomStartPos, <BalconyStartPos, <StoreStartPos, <Outside1StartPos, <Outside2StartPos
StartingPosHi:
    .byte >JamesRoomStartPos, >LivingRoomStartPos, >ScottRoomStartPos, >BathroomStartPos, >BalconyStartPos, >StoreStartPos, >Outside1StartPos, >Outside2StartPos

RoomInteractLo:
    .byte <JamesRoomInteract, <LivingRoomInteract, <ScottRoomInteract, <BathroomInteract
RoomInteractHi:
    .byte >JamesRoomInteract, >LivingRoomInteract, >ScottRoomInteract, >BathroomInteract

RoomBasedEventsLo:
    .byte <DoNothing - 1, <LivingRoomTestFunction - 1, <DoNothing - 1, <BathroomBasedEvents - 1, <DoNothing - 1, <DoNothing - 1, <DoNothing - 1, <DoNothing - 1
RoomBasedEventsHi:
    .byte >DoNothing, >LivingRoomTestFunction, >DoNothing, >BathroomBasedEvents, >DoNothing, >DoNothing, >DoNothing, >DoNothing

RoomLoadingLo:
    .byte <JamesRoomLoad - 1, <LivingRoomLoad - 1, <ScottRoomLoad - 1, <BathRoomLoad - 1, <BalconyLoad - 1
RoomLoadingHi:
    .byte >JamesRoomLoad, >LivingRoomLoad, >ScottRoomLoad, >BathRoomLoad, >BalconyLoad

DefaultObjectStartLo:
    .byte <ScottStartingData
DefaultObjectStartHi:
    .byte >ScottStartingData


TextTableLo:
    .byte <SampleText
TextTableHi:
    .byte >SampleText

SampleText: ; count ( sets of 4 ), y pos offset, tile, att (prob 0), x pos offset, ...
    ; ok instead of count we read intil FE? cause i can't have y be 1
    .byte       $00, $D0, $00, $00
    .byte       $00, $D1, $00, $08
    .byte       $00, $D2, $00, $10
    .byte       $00, $D3, $00, $18
    .byte       $00, $D4, $00, $20
    .byte       $00, $D5, $00, $28
    .byte       $00, $D6, $00, $30
    .byte       $00, $D7, $00, $38
    .byte       $00, $D8, $00, $40
    .byte       $00, $D9, $00, $48
    .byte       $00, $E0, $00, $50
    .byte       $00, $E1, $00, $58
    .byte       $00, $E2, $00, $60
    .byte       $00, $E3, $00, $68
    .byte       $00, $E4, $00, $70
    .byte       $00, $E5, $00, $78 
    .byte       $FF
                ;     H                    I
StandingAnimation:
    .byte $00, $00, $00, $00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   LoadZone information
;   
;   byte 0:     count
;   byte 1-5:   x pos, y pos, next roomIndex, starting x pos, starting y pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

JamesRoomLoadZone:
    .byte $02
    .byte $28, $B0, %00000001 ;$01, $00  
    .byte $28, $B8, $01  
    ;.byte $28, $B0, $01, $C0, $44, >LivingRoomLoad, <LivingRoomLoad - 1  
    ;.byte $28, $B8, $01, $C0, $44, >LivingRoomLoad, <LivingRoomLoad - 1  

    ;.byte $28, $B0, $01  
   ; .byte $28, $B8, $01

LivingRoomLoadZone:
    .byte $0A
    .byte $C8, $40, $00
    .byte $C8, $48, $00
    ;.byte $C8, $40, $00, $38, $B6, >JamesRoomLoad, <JamesRoomLoad - 1 ;james room
    ;.byte $C8, $48, $00, $38, $B6, >JamesRoomLoad, <JamesRoomLoad - 1

    .byte $C8, $68, $02    ; scotts room
    .byte $C8, $60, $02

    .byte $B0, $28, $03    ; bathroom
    .byte $B8, $28, $03

    .byte $30, $B8, %00000100   ;   $04, $C0, $84, >BalconyLoad, <BalconyLoad - 1 ; balcony
    .byte $30, $B0, %00000100   ;$04, $C0, $84, >BalconyLoad, <BalconyLoad - 1

    .byte $78, $28, %00100100 ;$04, $C0, $50, >BalconyLoad, <BalconyLoad - 1 ; balcony lower
    .byte $70, $28, %00100100   ;$04, $C0, $50, >BalconyLoad, <BalconyLoad - 1

ScottRoomLoadZone:
    .byte $02
    .byte $28, $30, $21 ; $01  ; , $C0, $63, >LivingRoomLoad, <LivingRoomLoad - 1 ; living room
    .byte $28, $38, $21 ;$01  ; , $C0, $63, >LivingRoomLoad, <LivingRoomLoad - 1

BathroomLoadZone:
    .byte $02
    .byte $A0, $78, $31  ; , $B3, $30, >LivingRoomLoad, <LivingRoomLoad - 1
    .byte $A8, $78, $31  ; , $B3, $30, >LivingRoomLoad, <LivingRoomLoad - 1
BalconyLoadZone:
    .byte $02
    .byte $D0, $80, $41  ;, $38, $B4, >LivingRoomLoad, <LivingRoomLoad - 1
    .byte $D0, $88, $41   ;, $38, $B4, >LivingRoomLoad, <LivingRoomLoad - 1
Outside1LoadZone:
    .byte $00

Outside2LoadZone:
    .byte $00

StoreLoadZone:
    .byte $00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Player Starting Positions
;
;   x pos, y pos
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RoomStartPosLo: 
    .byte <JamesRoomStartPos, <LivingRoomStartPos, <ScottRoomStartPos, <BathroomStartPos, <BalconyStartPos
RoomStartPosHi:
    .byte >JamesRoomStartPos, >LivingRoomStartPos, >ScottRoomStartPos, >BathroomStartPos, >BalconyStartPos


JamesRoomStartPos:
    .byte $38,$B6
LivingRoomStartPos:     ; each pair is a starting pos coming from: James room, front door, scotts room, bathroom, balcony
    .byte $C0, $44, $C0, $44, $C0, $63, $B4, $31, $38, $B4
ScottRoomStartPos:
    .byte $3A,$33
BathroomStartPos:
    .byte $A5,$70
BalconyStartPos:
    .byte $80,$80
StoreStartPos:
    .byte $80,$80
Outside1StartPos:
    .byte $80,$80
Outside2StartPos:
    .byte $80,$80

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Interact Tables
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; James Room
; x, y, hi, low - 1
JamesRoomInteract:
    .byte $02       ; count
    .byte $80, $80, >InteractTestFunction1, <InteractTestFunction1 - 1  ; location of the interactable object
    .byte $A0, $88, >InteractTestFunction2, <InteractTestFunction2 - 1

LivingRoomInteract:
    .byte $00

ScottRoomInteract:
    .byte $00

BathroomInteract:
    .byte $01
    .byte $90, $30, >ToiletInteract, <ToiletInteract - 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Game Object Data
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.word ScottStartingData
ScottStartingData:
    .byte $55       ; y pos
    .byte $32       ; tile 
    .byte $00       ; att
    .byte $88       ; x pos
    .byte <ScottLogic       ; code lo
    .byte >ScottLogic       ; code hi
    .byte $60       ; var 1
    .byte $70       ; var2



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Attribute Tables
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JamesRoomAttribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

LivingRoomAttribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

ScottRoomAttribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

BathroomAttribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

BalconyAttribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

StoreAttribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

Outside1Attribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

Outside2Attribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Background Tile Maps
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

JamesRoomBackground:    ; roomIndex: 0
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

LivingRoomBackground:   ; roomIndex: 1
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$25,$25,$00,$00,$00,$00,$00,$00,$25,$25,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$00,$24,$24,$25,$25,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$25,$25,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

ScottRoomBackground:    ; roomIndex: 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24


BathroomBackground:     ; roomIndex: 3
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$30,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$25,$25,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24


BalconyBackground:      ; roomIndex: 4
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$25,$25,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24


Outside1Background:     ; roomIndex: 6
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$25,$25,$00,$00,$00,$00,$00,$00,$25,$25,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$00,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

Outside2Background:     ; roomIndex: 7
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$25,$25,$00,$00,$00,$00,$00,$00,$25,$25,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$00,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

StoreBackground:        ; roomIndex: 5
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$25,$25,$00,$00,$00,$00,$00,$00,$25,$25,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$00,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$00,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$25,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


sprites: ;  y  tile  att  x
    .byte $FE, $fe, $fe, $fe    ; 0 sprite off the screen (maybe status bar or something)
    .byte $80, $10, $00, $80 ; YCoord, tile number, attr, XCoord
    .byte $10, $00, $00, $10        ; 1's digit of timer sprite
    .byte $10, $00, $00, $08        ; 10s digit of timer sprite
    .byte $20, $00, $00, $10        ; 1s digit of timer2
    .byte $20, $00, $00, $08        ; 10s digit of timer 2

    .byte $20, $00, $00, $80        ; score 1s
    .byte $20, $00, $00, $78        ; score 10s
    .byte $20, $00, $00, $70        ; score 100s
    .byte $20, $09, $00, $68        ; score 1000s
    .byte $20, $00, $00, $60        ; score 10000s

    .byte $88, $00, $00, $A0
    .byte $80, $00, %00100000, $80        ; scott

        ;; weird idea. what if i just like yolo the sprites. like. is this what a buffer is? cause ive understood the conecpt but never the freaking impelmentation. 
                ;; so like. instead of writing directly to $02XX, is it better to write somewhere else... i guess as i write that out it seems like a no.. idk.
                ;; hmm let me think. I guess one thing thats semi related i guess. but like. right now every entity is hard coded. and for timers i guess that makes sense, and same for palyer.
                ;; but do I need every npc location known at all times. or at least taking up memory? 
                ;; so I'll need to write a better sprite and background shit probs. idk if I can with back ground, but at least... idk i think doing mapping can be saved for next proj
                ;; who knows though
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   
;   Background Hit tables
;      - each bit represent the tile in that location
;      - 0: collision off      1: collision on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

JamesRoomHitTable:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000011, %11111111, %11111111, %11000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00001110, %00000000, %00000000, %01000000
    .byte %00001000, %00000000, %00000000, %01000000
    .byte %00001000, %00000000, %00000000, %01000000
    .byte %00001111, %11111111, %11111111, %11000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

LivingRoomHitTable:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000111, %10000111, %10000000
    .byte %00000000, %00000100, %10000100, %10000000
    .byte %00000011, %11111100, %11111100, %11000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01110000
    .byte %00000010, %00000000, %00000000, %00010000
    .byte %00000010, %00000000, %00000000, %00010000
    .byte %00000010, %00000000, %01111110, %01110000
    .byte %00000010, %00000000, %10000010, %01110000
    .byte %00000010, %00000001, %00000010, %00010000
    .byte %00000010, %00000010, %00000010, %00010000
    .byte %00000010, %00000010, %00000011, %11110000
    .byte %00000010, %00000010, %00000000, %00000000
    .byte %00000010, %00000010, %00000000, %00000000
    .byte %00000010, %00000011, %11111100, %00000000
    .byte %00000010, %00000000, %00000100, %00000000
    .byte %00000010, %00000000, %00000100, %00000000
    .byte %00000010, %00000000, %00000100, %00000000
    .byte %00001110, %00000000, %00000100, %00000000
    .byte %00001000, %00000000, %00000100, %00000000
    .byte %00001000, %00000000, %00000100, %00000000
    .byte %00001111, %11111111, %11111100, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

ScottRoomHitTable:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00001111, %11111111, %11111111, %11000000
    .byte %00001000, %00000000, %00000000, %01000000
    .byte %00001000, %00000000, %00000000, %01000000
    .byte %00001110, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00111110, %00000000, %00000000, %01000000
    .byte %00100000, %00000000, %00000000, %01000000
    .byte %00100000, %00000000, %00000000, %01000000
    .byte %00100010, %00000000, %00000000, %01000000
    .byte %00100010, %00000000, %00000000, %01000000
    .byte %00100010, %00000000, %00000000, %01000000
    .byte %00100010, %00000000, %00000000, %01000000
    .byte %00100010, %00000000, %00000000, %01000000
    .byte %00100010, %00000000, %00000000, %01000000
    .byte %00111111, %11111111, %11111111, %11000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

BathroomHitTable:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000011, %00000011, %11111111, %11000000
    .byte %00000010, %00000010, %00100010, %01000000
    .byte %00000010, %00000010, %00000010, %01000000
    .byte %00000010, %00000010, %00000010, %01000000
    .byte %00000010, %00000010, %00000010, %01000000
    .byte %00000010, %00000010, %00000010, %01000000
    .byte %00000010, %00000010, %00000010, %01000000
    .byte %00000000, %00000011, %11110010, %00000000
    .byte %00000000, %00000000, %00010010, %00000000
    .byte %00000000, %00000000, %00010010, %00000000
    .byte %00000000, %00000000, %00010010, %00000000
    .byte %00000000, %00000000, %00011110, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

BalconyHitTable:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000011, %11111111, %11111111, %11000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %01000000
    .byte %00000000, %00000000, %00000000, %01000000
    .byte %00000000, %00000000, %00000000, %01000000
    .byte %00000000, %00000000, %00011111, %11000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

StoreHitTable:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000011, %11111111, %11111111, %11000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

Outside1HitTable:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000011, %11111111, %11111111, %11000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

Outside2HitTable:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000011, %11111111, %11111111, %11000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000010, %00000000, %00000000, %01000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bitMask:
    .byte %10000000
    .byte %01000000
    .byte %00100000
    .byte %00010000
    .byte %00001000
    .byte %00000100
    .byte %00000010
    .byte %00000001

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "mario.chr" 