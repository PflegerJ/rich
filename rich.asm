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
    pointer2Lo:     .res 1
    pointer2Hi:     .res 1
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
    nameTable:      .res 1  ; which nametable to load
    roomIndex:      .res 1  ;;;; could make it bits 765, for previous roomIndex. and bits 210 are for current roomIndex?
    spriteCount:    .res 1
    frameCounter60: .res 1
    gameTime:       .res 2
    fifteenSeconds:      .res 1
    score:          .res 1
    currentGameObjectOffset:    .res 1
    temp1:          .res 1
    temp2:          .res 1
    stupidTemp:     .res 1
    currentX:       .res 1
    currentY:       .res 1
    minuteCounter:   .res 1      ; this should just count to 60? 
    vblankCounter:  .res 2      ; this is the global timer. any system that requires times should use this to check if its time yet?
    timerMinuteOffset:  .res 1
    timerHourOffset:    .res 1
    globalTimerOffset:  .res 1
    GAME_OBJECT_OFFSET: .res 8 ; this should be object max, it will be the offsets for the game objects. this will be shuffled each frame to make drawing easier 
    gameObjectCounter:  .res 1
    ;TIMER_MASK  = %10000000
    
    VBLANKS_PER_MINUTE = 60
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
    
    
    distanceTestValueX:       .res 1
    distanceTestValueY:       .res 1
    distanceTestResult:       .res 2
    scoreIncrementOnes: .res 1
    scoreIncrementTens: .res 1
    beerCount:                   .res 1      ; first half is cigs, 2nd beer bits: 0123 | 4567
    cigCount:                   .res 1

    ;;; player variables ;;; will clean up any player variable that is above this line when done with this shit

    playerState:                .res 1
    playerFacingDirection:      .res 1
    playerAnimationOffset:      .res 1
    playerAnimationTimer:       .res 1

    ; this is based off 8.8 fixed point arithmetic and 2's complement bullshit.
    playerPxLo:                 .res 1
    playerPxHi:                 .res 1
    playerPyLo:                 .res 1
    playerPyHi:                 .res 1

    playerVxHi:                 .res 1
    playerVxLo:                 .res 1
    playerVyHi:                 .res 1
    playerVyLo:                 .res 1

    ; this might be not needed at all in anyway
    playerAxHi:                 .res 1
    playerAxLo:                 .res 1
    playerAyHi:                 .res 1
    playerAyLo:                 .res 1

    PLAYER_V_CAP            = 7
    PLAYER_ACCELERATION_HI  = $00
    PLAYER_ACCELERATION_LO  = $80

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       SPRITE OAM ADDRESSES
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    SPRITE_OAM_START                = $0200     ; start of OAM address space 0200 - 02ff
    UI_OAM_START                    = $0200     ; UI will be the first sprite slots i guess. something something sprite 0 hits something something
    PLAYER_OAM_START                = $0220     ; randomly chose this
    GAME_OBJECT_OAM_START           = $0240     ; i want this to be as big as possible. just depends on how many UI sprites I will need
    GAME_OBJECT_OAM_OFFSET_START    = $40




    TIMER_OAM_START       = $0208
    TIMER_OAM_HOUR_TENS     = $0208
    TIMER_OAM_HOUR_ONES = $020C
    TIMER_OAM_MIN_TENS  = $0210
    TIMER_OAM_MIN_ONES  = $0214    

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
    
    SPRITE_RAM_START        = $40       ; this is used to store 
         ; this is what I'm using now. its where the sprites for game objects starts
    scottDataStartLo: .res 1
    scottDataStartHi: .res 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       Game Object Constants and Addressses
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    GAME_OBJECT_RAM_START = $0300  
    objectMax = 8
    variableCount = 16

    objectNext = GAME_OBJECT_RAM_START + objectMax * 0 ; ok we are going to try this implementation i guess

    objectXPos = GAME_OBJECT_RAM_START + objectMax * 1
    objectXPosFloat = GAME_OBJECT_RAM_START + objectMax * 2

    objectYPos = GAME_OBJECT_RAM_START + objectMax * 3
    objectYPosFloat = GAME_OBJECT_RAM_START + objectMax * 4

    objectVar1 = GAME_OBJECT_RAM_START + objectMax* 5
    objectVar2 = GAME_OBJECT_RAM_START + objectMax * 6
    objectVar3 = GAME_OBJECT_RAM_START + objectMax * 7     

    objectHi = GAME_OBJECT_RAM_START + objectMax * 8
    objectLo = GAME_OBJECT_RAM_START + objectMax * 9

    objectDrawHi = GAME_OBJECT_RAM_START + objectMax * 10
    objectDrawLo = GAME_OBJECT_RAM_START + objectMax * 11
    objectAnimationOffset = GAME_OBJECT_RAM_START + objectMax * 12    ; im thinkin
    objectAnimationTimer = GAME_OBJECT_RAM_START + objectMax * 13

    objectState = GAME_OBJECT_RAM_START + objectMax * 14
    objectAtt = GAME_OBJECT_RAM_START + objectMax * 15       ; I'm not sure I need this anymore... I'll keep for now when I learn more about the attributes.

; These are for keeping track of game objects in ram
    firstFreeSlot:      .res 1
    firstOccupiedSlot:  .res 1
    lastOccupiedSlot:   .res 1


    ; Declaring some static shit for tiles
    ZERO        = $00
    ONE         = $01
    TWO         = $02
    THREE       = $03
    FOUR        = $04
    FIVE        = $05
    SIX         = $06
    SEVEN       = $07
    EIGHT       = $08
    NINE        = $09

.segment "CODE"

    ;; Subroutines
vblankwait: 
    bit PPU_STATUS   
    bpl vblankwait
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
    sta playerPxHi
    iny
    lda (pointerLo), Y
    sta playerPyHi
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

    lda playerPxHi          ; remove last 8 bits - each tile is 8x8 bits so no need to check last 8
    and #%11111000
    sta temp1
    lda playerPyHi
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

initializeGlobalTimer:
    lda #$00
    sta vblankCounter
    sta timerMinuteOffset
    sta timerHourOffset
    rts 

TimeEngine:
    jsr IncrementGlobalTimer
    jsr UpdateTimerOffsets
    rts 


;; I need to rethink my draw function. cause it breaks with a player. and it breaks with this. do I need to change the format a bit? hmmmm i'll tackle this tomorow
DrawTimer:
   ; ldx timerH
    rts 

IncrementGlobalTimer:
    inc vblankCounter
    bne @DoneIncrementingGlobalTimer
    inc vblankCounter + 1
@DoneIncrementingGlobalTimer:
    rts 

; ok so we are going to check against a mask. that should just rotate between 0100 0000 and 1000 0000
; this should let us know every time 0100 0000 frames have passed. each time this happens we update the minute offset
; if the minute offset rolls over we increment the hour offset. EZ
; if hour rolls over i guess we just restart? I could just reinitialize the timer. or go into the night mode which i think is really cool so i want that way
; we also need to flip the mask offset to swap between the two
UpdateTimerOffsets:
    ldx globalTimerOffset
    lda TIMER_MASK,x 
    and vblankCounter
    bne @DoneUpdatingTimerOffsets
    ; first lets get the offset set and stored
    lda $01
    eor globalTimerOffset
    sta globalTimerOffset 
    
    ; then we need to inc the minutes offset and check for rollover
            ; I want to spend time playing around with optimizing this eventually cause i feel like there is a super cool smart way but god bits are bullshit sometimes
    inc timerMinuteOffset
    lda #$04
    cmp timerMinuteOffset
    bne @DoneUpdatingTimerOffsets       ; the minute has not rolled over so we done
    ldy #$00
    sty timerMinuteOffset           

    inc timerHourOffset
    lda #$0C                            ; this 12 
    bne @DoneUpdatingTimerOffsets
    sty timerHourOffset 
@DoneUpdatingTimerOffsets:
    rts 

TIMER_MASK:
    .byte %01000000, %10000000

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
    lda playerPyHi
    cmp distanceTestXpos    ; this needs to be changed to scotts sprite? hmmmm
    bcc @DistanceXBigger ; branch if distanceTestX is bigger
    sec 
    sbc distanceTestXpos
    sta distanceTestValueX
    jmp @CalcYDiff

@DistanceXBigger:
    lda distanceTestXpos
    sec 
    sbc playerPxHi
    sta distanceTestValueX

@CalcYDiff:
    lda playerPyHi
    cmp distanceTestYpos
    bcc @DistanceYBigger ; brance if distanceTestY is bigger
    sec 
    sbc distanceTestYpos
    sta distanceTestValueY
    jmp @Squaring

@DistanceYBigger:
    lda distanceTestYpos
    sec 
    sbc playerPyHi
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Player Logic
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1/15/25
    ; so basically I need to redo like all of this?
    ; I understand why they do the jump feature. cause I think jumping is the best way
    ; use state as the offset, and then jump to the subroutine 
            ; how the fuck do i do movement with like acceleration and shit...

;test
PlayerLogic:

    ldx playerState
    lda PlayerGameLoopSubroutinesLo,x 
    sta pointerLo 
    lda PlayerGameLoopSubroutinesHi,x 
    sta pointerHi 

    jmp (pointerLo)
    ; 1/23/25
        ; ok so after the state based subroutine rts's it will come back here.  
        ; I'm not sure what else needs to be done. let me map out what a state based subroutine might look like with standing and see what is left to do

    
    ; 
    rts 
PlayerGameLoopSubroutinesLo:
    .byte <PlayerStandingLogic, <PlayerWalkingLogic
PlayerGameLoopSubroutinesHi:
    .byte >PlayerStandingLogic, >PlayerWalkingLogic


PlayerStandingLogic:
    ; 1/23/25
        ; so what can happen when im standing? basically its based on the user input?
        ; like
    
    ; 1/24/25
        ; ok i think i got an idea that works for now
        ; we do, based off input from previous frame,
            ; stay in same state
                ; update animation timer
                    ; update animation offset if needed
            ; change state
                ; read from rom new default state info needed
                ; update playerState to new state
                ; update playerAnimationTimer to 0
    ; 2/4/25
        ; ok i am feeling overwhelmed and like i don't know shit so lets lower the scope and get something working so i can play around with it and figure out a better way later
        ; lets focus on moving. which would change the state to walking.
    lda controller1Pressed
    and #$0F
    cmp #$00
    bne @PlayerStandingNowWalking
    ; ok so if i have directional input, i will skip all this and go to the subroutine to switch my state to walking.
        ; this means that i need to set up all the shit in that routine and i need to not care about it here
        ; what I care about now is I am still standing. so i need to update the animation crap.
    inc playerAnimationTimer
    ; this uses the animation frame offset, to get the amount of time we want to be in that frame
        ; compares it to the animation timer. 
            ; if they are not equal, then we should still be in this frame and we are done
            ; if they are equal, then we need to...
                ; lets reset the timer -  i know its a bad way but it makes the code more clear for now
                ; increase the frame offset
                    ; check the new frame offset against the frame count constant
                        ; reset frame offset to 0 if they are equal
    ldx playerAnimationOffset
    lda PlayerStandingAnimationFrameTimes,x
    cmp playerAnimationTimer
    bne @DonePlayerStanding
    ldy #$00
    sty playerAnimationTimer
    inc playerAnimationOffset
    lda #PLAYER_STANDING_ANIMATION_FRAME_COUNT
    cmp playerAnimationOffset
    bne @DonePlayerStanding
    sty playerAnimationOffset
    jmp @DonePlayerStanding

@PlayerStandingNowWalking:
    lda #$01            
    jsr ChangePlayerState

@DonePlayerStanding:
    rts 

PLAYER_STANDING_ANIMATION_FRAME_COUNT   =   2
PLAYER_WALKING_ANIMATION_FRAME_COUNT    =   2
PlayerStandingAnimationFrameTimes:
    .byte $FF, $88 


    ; the state to change to needs to be set in A before you call this
ChangePlayerState:
    sta playerState
    lda #$00
    sta playerAnimationTimer
    sta playerAnimationOffset
    rts 

    ; 2/4/25
        ; this feels wrong already. right now everything i do in walking i do in standing, except i guess not check input?
        ; i'm interested in how much the physics engine will show me im doing stuff wrong. thats bad english but i'm not editing 
        ; let yourself discover instead of theorize
PlayerWalkingLogic:
    ; i need to check if they hit A or not cause i think you should be able to hit a while moving
    rts 


; ok time to make a really mid phy eng
; so right now im only thinking about the player
; you got this
PhysicsEngine:
    ; first lets just check if we need to do anything based on the players state
    lda #$01
    cmp playerState
    beq @PlayerPhysics                    ; i think its bcc. carry is set when a => value. so is 1 >= state. state should be 1 or 0. so is 1 >= 1 is 1>= 0. so the carry would be set. so bcc means 1 >= 2 which would mean we in a non moving state so no physics. 
    rts                                 ; this will probably be changed but it makes sense i think
@PlayerPhysics:
    ; first lets combine pressed and held input cause i don't think we need to care......
    lda controller1Pressed
    ora controller1Held 
    and #$0F                ; ok so now we have held and pressed directional input

    ; first lets just do x 
    sta temp1   ; storing the pressed and held direction input cause idk if i have to or not yet
    ; im thinking. you can not hold both right and left dir inputs at the same time. 
    ; so. lets isolate right and left. compare to... 0 first. 
    ; if beq then no RL input is held. 
    ; then we can compare to $01. if its even then right is held. if not then left is held?
    ; i know there is a trick or wayyyy better way to do this. but I could write this comment about literally every single line of code i've ever written
    and #%00000011
    cmp #$00    ; chasing these roads until dawn
    beq @NoRightLeftInput
    cmp #$01    ; i won't let the light fade
    beq @YesRightInput
    ; this is left input
        ; left is negative
    lda #PLAYER_X_ACCELERATION_NEGATIVE_LO
    clc 
    adc playerVxLo
    sta playerVxLo

    lda #PLAYER_X_ACCELERATION_NEGATIVE_HI
    adc playerVxHi 
    sta playerVxHi
    cmp #PLAYER_Vx_NEGATIVE_CAP
    bne @NoRightLeftInput
    lda #$00
    sta playerVxLo
    ; Idk if i should check for cap here or not. lets ignore the cap for now. cause just like every single design choice. i can see positive's and negatives for implementing now and for doing a catch all check at the end

    jmp @NoRightLeftInput   ; maybe a bad label name lol
@YesRightInput:
    lda #PLAYER_X_ACCELERATION_POSITIVE_LO
    clc 
    adc playerVxLo
    sta playerVxLo 

    lda #PLAYER_X_ACCELERATION_POSITIVE_HI
    adc playerVxHi
    sta playerVxHi
    cmp #PLAYER_Vx_POSITIVE_CAP
    bne @NoRightLeftInput
    lda #$00 
    sta playerVxLo

; i feel like there is better way to do the bit isolating and comparing but its ok
@NoRightLeftInput:
    lda temp1   ; this is getting back the direction presssed and held input
    and #%00001100  ; isolate just up and down
    ; one day i'll know for sure the order of these fucking bits. i think its down up only cause its sel start, left right. so fuck. its easy to change
    cmp #$00
    beq @NoUpDownInput

    cmp #%00000100
    bne @YesUpInput ; ima swap the beq to this bne. idk which makes more sense/is better right now. i'll examine later
    ; this is down
    lda #PLAYER_Y_ACCELERATION_POSITIVE_LO
    clc 
    adc playerVyLo 
    sta playerVyLo

    lda #PLAYER_Y_ACCELERATION_POSITIVE_HI
    adc playerVyHi 
    sta playerVyHi
    cmp #PLAYER_Vy_POSITIVE_CAP
    beq @AtVyPosCap 
    jmp @NoUpDownInput

@AtVyPosCap:
    lda #$00
    sta playerVyLo 
    jmp @NoUpDownInput

@YesUpInput:
    lda #PLAYER_Y_ACCELERATION_NEGATIVE_LO
    clc 
    adc playerVyLo
    sta playerVyLo

    lda #PLAYER_Y_ACCELERATION_NEGATIVE_HI
    adc playerVyHi
    sta playerVyHi 
    cmp #PLAYER_Vy_NEGATIVE_CAP
    beq @AtVyNegCap
    jmp @NoUpDownInput

@AtVyNegCap:
    lda #$00
    sta playerVyLo
    ; so now we have updated the player's velocity based off player input.
    ;



@NoUpDownInput:
    ; this is going to be a janky V cap
    jsr ApplyPlayerFriction

@NoPlayerPhysics:
    rts 

PLAYER_X_ACCELERATION_POSITIVE_HI = $01
PLAYER_X_ACCELERATION_POSITIVE_LO = $C0 ; this should be $00C0 which means 0.75 i think. or i am still way lost
PLAYER_X_ACCELERATION_NEGATIVE_HI = $FE
PLAYER_X_ACCELERATION_NEGATIVE_LO = $50

PLAYER_Y_ACCELERATION_POSITIVE_HI = $01
PLAYER_Y_ACCELERATION_POSITIVE_LO = $C0 ; this should be $00C0 which means 0.75 i think. or i am still way lost
PLAYER_Y_ACCELERATION_NEGATIVE_HI = $FE
PLAYER_Y_ACCELERATION_NEGATIVE_LO = $50

PLAYER_Vx_POSITIVE_CAP = $02
PLAYER_Vy_POSITIVE_CAP = $02
PLAYER_Vx_NEGATIVE_CAP = $FD
PLAYER_Vy_NEGATIVE_CAP = $FD

FRICTION_COEF_POSITIVE_X_HI = $00
FRICTION_COEF_POSITIVE_X_LO = $40
FRICTION_COEF_NEGATIVE_X_HI = $FF
FRICTION_COEF_NEGATIVE_X_LO = $E0

FRICTION_COEF_POSITIVE_Y_HI = $00
FRICTION_COEF_POSITIVE_Y_LO = $40
FRICTION_COEF_NEGATIVE_Y_HI = $FF
FRICTION_COEF_NEGATIVE_Y_LO = $E0
; I am going to assume i've calculated the players speed based off previous frame, and user input. i've done friction and everything else. this is just doing the 8.8 fixed point
; this is based off that one thread on nesdev. which is quite possibly the most useless comment i've ever written cause duh

; basically i am assuming that i will calculated the player Vx and Vy. and then i can just call this. in a dream world, this would even naturally go to 0 and swap to standing state.
    ; which is making wonder if I need previous Vx and Vy but got thats so many bytes
UpdatePlayerPosition:
    lda #$01
    cmp playerState
    bne @WrongState

    lda playerVxLo 
    clc 
    adc playerPxLo
    sta playerPxLo

    lda playerVxHi
    adc playerPxHi
    sta playerPxHi

    lda playerVyLo
    clc 
    adc playerPyLo
    sta playerPyLo 

    lda playerVyHi 
    adc playerPyHi
    sta playerPyHi 

@WrongState:
    rts 

DetermineFacingDirection:
    rts 

; we aren't trying to be smart
; lets get some shit code that works. having friction be seperate means its easier bug fix. adjust values. add functionality for different floor types and all that. but that is not now 

; i think there is a cuter way to do this with offsets. i will look into that when i revisit. not a priority rn but i can see how that pattern really can be used everywhere. and since its basically the onlything the cpu can do i assume i should be using it where i can
ApplyPlayerFriction:
    lda #$01 
    cmp playerState
    bne @IShouldntBeHere
    ; basically we need to know if we are moving + or -. and then add the opposite. idk if we need to round to 0 here? or somehwere else. somewhere else seems right but i don't know why yet so i'll do it here
    lda playerVxHi 
    asl     ; should store the msb in the carry so we can use that 
    bcs @PlayerVxIsNegative
    ; Vx is positive: so use negative friction coef
    lda #FRICTION_COEF_NEGATIVE_X_LO
    clc 
    adc playerVxLo 
    sta playerVxLo 

    lda #FRICTION_COEF_NEGATIVE_X_HI
    adc playerVxHi 
    sta playerVxHi 
    jmp @PlayerFrictionY

@PlayerVxIsNegative:    
    lda #FRICTION_COEF_POSITIVE_X_LO
    clc 
    adc playerVxLo 
    sta playerVxLo

    lda #FRICTION_COEF_POSITIVE_X_HI
    adc playerVxHi
    sta playerVxHi

@PlayerFrictionY:          
    lda playerVyHi
    asl 
    bcs @PlayerVyIsNegative
    ; Vy is positiver: so use negative friction coef
    lda #FRICTION_COEF_NEGATIVE_Y_LO
    clc 
    adc playerVyLo 
    sta playerVyLo 

    lda #FRICTION_COEF_NEGATIVE_Y_HI
    adc playerVyHi
    sta playerVyHi 

    jmp @DonePlayerFriction
@PlayerVyIsNegative:
    lda #FRICTION_COEF_POSITIVE_Y_LO
    clc 
    adc playerVyLo
    sta playerVyLo

    lda #FRICTION_COEF_POSITIVE_Y_HI
    adc playerVyHi
    sta playerVyHi 
     
@DonePlayerFriction:
    jsr CheckPlayerMovingTooSlow
@IShouldntBeHere:
    rts                                        

; because right now i'm applying friction to Vx and Vy without checking if I should... I need set V to 0 if it is between - Friction coef < V < + friction coef
; there's a zero flag that i can use... so just check z flag. is there a branch... bzs? bpl bmi
    ; i guess its branch minus and branch plus

; i might have fucked up the +/- 1. it made sense when i was writing it but 2 seconds later it doens't. it has to do with being exactly the coef and wanting or not wanting that to be included in the range.
; x and y are used as flags to set player state to standing if both values get set to 0 so we aren't moving. also known, as, standing
    ; this might be the worst subroutine i've written in awhile
CheckPlayerMovingTooSlow:
    ldy #$00
    ldx #$00    

    lda playerVxHi
    bpl @CheckPlayerMovingVxPositive

    ; this is checking if Vx is negative. so it should be Hi = $FF and lo > FRICTION_COEF_LO - 1
    cmp #$FF
    bne @VxTooFast
    lda playerVxLo
    cmp #FRICTION_COEF_NEGATIVE_X_LO + 1
    bcs @VxTooFast
    lda #$00
    sta playerVxHi
    sta playerVxLo
    ldy #$01
    jmp @VxTooFast

    ; this is checking if Vx is postitive. so it should be Hi = $00 and lo < FRICTION_COEF + 1
@CheckPlayerMovingVxPositive:
    cmp #$00
    bne @VxTooFast
    lda playerVxLo 
    cmp #FRICTION_COEF_POSITIVE_X_LO + 1
    bcc @VxTooFast
    lda #$00
    sta playerVxLo 
    sta playerVxHi
    ldy #$01

@VxTooFast:
    lda playerVyHi
    bpl @CheckPlayerMovingVyPositive
    cmp #$FF
    bne @VyTooFast
    lda playerVyLo
    cmp #FRICTION_COEF_NEGATIVE_Y_LO + 1
    bcs @VyTooFast
    lda #$00
    sta playerVyLo
    sta playerVyHi 
    ldx #$01
    jmp @VyTooFast

@CheckPlayerMovingVyPositive:
    cmp #$00        ; i think there is a zero flag but that can be fixed later
    bne @VyTooFast
    lda playerVyLo 
    cmp #FRICTION_COEF_POSITIVE_Y_LO + 1
    bcc @VyTooFast
    lda #$00
    sta playerVyHi
    sta playerVyLo
    ldx #$01
     

@VyTooFast:
; this is checking if we set it to 0 to change to standing state.
    ; i know its horrible. we want working build. this will get fixed when i inevitably delete it
    cpy #$01
    bne @FrictionDone
    cpx #$01
    bne @FrictionDone
    lda #$00
    jsr ChangePlayerState

@FrictionDone:

    rts 

    
SetPlayerVelocityToZero:
    lda #$00
    sta playerVxHi
    sta playerVxLo 
    sta playerVyHi
    sta playerVyLo 
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
    
    

TestNPCGameLoop:

    rts 





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
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
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
    sta playerPxHi
    iny 
    lda (pointerLo), Y
    sta playerPyHi
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
    sta playerPyHi
    iny 
    lda (pointerLo), Y
    sta playerPyHi
    rts 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Game Engine
;       
;       CreateGameObject - Assumes PointerLo and Hi have been set to the object data table
;       DeleteGameObject - Assumes target offset is already in X
;   
;       InitializeGameObjectRam - Called on Startup (could maybe be called to clear all game objects)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    sta deleteFlag
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
    lda #$00
    sta deleteFlag
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





; I need to start dating these
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

    ; GAME_OBJECT_RAM_START = $0300
    ; objectMax = $20  
    ; objectNext
    ; firstFreeSlot:      .res 1
    ; firstOccupiedSlot:  .res 1
    ; lastOccupiedSlot:   .res 1

; ok so we are going back to the old method i first read about using the offset stuff whatever god im so fucking hungry i can't think
; 
; 
    
    ldy #$00
    sty gameObjectCounter
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


;comments
    ; This logic should work for adding. I am now working on deleting which may cause some bugs here when the list gets populated, then deleted and then repopulated and then deleted.
    ; im mostly concered with the list getting back to empty and making sure there are no breaks in the linkedlist 
    ; this is purely for getting the memory management down. I'm not caring about any variables or pointers or whatever. we are just going to call this along side deletegameobject
    ; to make sure that shit is added and deleted and then properly iterated through. 
    ; also not sure about trying to add to a full list or adding last possible element


;; ok now its time to think about adding the actual game object data to the other variables. the main thing is I need to know what my offset is which is what the whole linked
        ; list bullshit is about. but after i figure out where I'm putting it and make sure all the pointers are pointing, I can use that slot as offset to just blast through
        ; the table that some pointer is pointing at and move it to ram.
CreateGameObject:

    ; checking if list is full
    lda firstFreeSlot
    cmp #objectMax
    beq @DoneAddingNewElement   ; if firstFreeSlot is pointing to objectMax then the list is full and we can't add more objects to it.

    ; put the object data from rom into ram

;   XPos, XPosFloat, YPos, YPosFloat, Var1, Var2, Var3, ObjectHi, ObjectLo, DrawHi, DrawLo, Ani Offset, Ani Timer, State, Att

    ldx firstFreeSlot           ; this is the offset for putting the data
    ldy #$00                    ; this is the offset of the data table we grabbing the data from

    lda (pointerLo),y   
    sta objectXPos,x 

    lda (pointerLo + 1),y  
    sta objectXPosFloat,x 

    lda (pointerLo + 2),y  
    sta objectYPos,x

    lda (pointerLo + 3),y   
    sta objectYPosFloat,x 

    lda (pointerLo + 4),y  
    sta objectVar1,x 

    lda (pointerLo + 5),y   
    sta objectVar2,x 

    lda (pointerLo + 6),y  
    sta objectVar3,x 

    lda (pointerLo + 7),y  
    sta objectHi,x 

    lda (pointerLo + 8),y 
    sta objectLo,x    

    lda (pointerLo + 9),y
    sta objectDrawHi,x

    lda (pointerLo + 10),y 
    sta objectDrawLo,x 

    lda (pointerLo + 11),y 
    sta objectAnimationOffset,x    

    lda (pointerLo + 12),y 
    sta objectAnimationTimer,x   

    lda (pointerLo + 13),y 
    sta objectState,x    

    lda (pointerLo + 14),y 
    sta objectAtt,x 


    ; I am also going to have an array of current offsets to help the draw function randomize the sprite prio order each frame
    ; gameObjCounter is my offset of offsets
    ; it should be pointing at the next open slot
    ; i should not be at this code if i have max game objects so no error checking baby
    
    txa 
    ldx gameObjectCounter
    sta GAME_OBJECT_OFFSET,x 
    tax 
    inc gameObjectCounter

; This is adding the game object into the linked list

    ; checking if list is empty
    lda firstOccupiedSlot           
    cmp #objectMax
    beq @TheCurrentListIsEmpty           ; not sure if i should branch if equal or if not equal yet. lets see what things have to happen and what order would make more sens

    ldx lastOccupiedSlot                 ; this is setting up offsets for adding nth element
    lda firstFreeSlot
    sta objectNext,x                        ; this is storing the newely added element's slot's next value to the previous end of the list's next value
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
    
    ; First we delete the offset from the GameObject Offset Array
    txa         ; target is now A reg
    pha         ; storing target on stack i don't remember why
    ldy #$00        ; y is our offset to access the data

@StartDeletingOffsetLoop:
    cmp GAME_OBJECT_OFFSET,y 
    beq @TargetOffsetFound
    iny 
    jmp @StartDeletingOffsetLoop    ; no error checking. we will only delete game objects that exist. so thats what it is

@TargetOffsetFound:
    dec gameObjectCounter
    tya     ; y is holding n
    tax     ; x is holding n + 1
    inx 

@ShufflingOffsetsForwardLoop:
    cpy gameObjectCounter                      ; if y is the same as the new decremented gameObjectCounter, we are at the end of the list and don't need to shuffle elements forward
    beq @DoneDeletingGameObjectOffset
    lda GAME_OBJECT_OFFSET,x 
    sta GAME_OBJECT_OFFSET,y 
    iny 
    inx 
    jmp @ShufflingOffsetsForwardLoop  

@DoneDeletingGameObjectOffset:
    pla 
    tax 

    ; Now we delete it from the linked list
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


@DoneDeleting:
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Draw Engine
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawEngine:
    jsr DrawPlayer

    lda #GAME_OBJECT_OAM_OFFSET_START
    sta spriteBufferOffset
    ; lets focus on just getting game objects drawn fuck everything else right now
    lda #$00
@DrawEngineLoopStart:
    cmp gameObjectCounter
    beq @DoneDrawingGameObjects

    ; we need to get the next game object offset
    tax 
    lda GAME_OBJECT_OFFSET,x
    sta currentGameObjectOffset     ; storing the current game object offset
    tay 
    stx stupidTemp
    jsr DrawEngineJmp
    inc stupidTemp
    lda stupidTemp
    jmp @DrawEngineLoopStart

@DoneDrawingGameObjects:
    ; need to write FE till we reach the end to clear garbage
    ldx spriteBufferOffset
    lda #$FE
@ClearingGarbageLoopStart:
    cpx #$00
    beq @DoneClearingGarbage
    sta SPRITE_OAM_START,x 
    sta SPRITE_OAM_START + 1,x 
    sta SPRITE_OAM_START + 2,x 
    sta SPRITE_OAM_START + 3,x 
    inx 
    inx 
    inx 
    inx 
    jmp @ClearingGarbageLoopStart
@DoneClearingGarbage: 
    rts 

DrawEngineJmp:
    lda objectDrawLo,y 
    pha 
    lda objectDrawHi,y  
    pha 
    rts 



; this should work. maybe i messed up some of the pointer shit but the logic is right. it would be little tweaks in the syntax 
; this is law now. ok time to work backwards fixing the hell i created at the beginning of this project.

PLAYER_STANDING_DOWN_ANIMATION_TIME     = $08
DrawPlayer:
    ; so same idea as any general game object
    ; use the state as offset on PlayerMetaSpriteDataLo/Hi to get the PlayerSTATE
    ; use the frame offset on PlayerState to get the meta sprite table of which tiles and attributes to use.
    ; the player sprite OAM addresses are going to be set cause there is no reason not to
    ; and so i just use the player pos as the achor, and place tiles (maybe i should have x and y offsets in the meta sprite table to just make shit easier. so I don't have to have temp shit and clc adc all the time.) i think ima do that
    
    lda playerFacingDirection
    asl 
    tay 
    ldx playerState

    lda PlayerMetaSpriteDataLo,x 
    sta pointerLo
    lda PlayerMetaSpriteDataHi,x 
    sta pointerHi

    lda (pointerLo),y 
    sta pointer2Lo
    iny 
    lda (pointerLo),y 
    sta pointer2Hi

    lda playerAnimationOffset
    asl 
    tay 

    lda (pointer2Lo),y 
    sta pointerLo
    iny 
    lda (pointer2Lo),y 
    sta pointerHi

    ldy #$00
    ldx #$00

@DrawPlayerLoopStart:

    lda (pointerLo),y               ; y offset
    clc 
    ; adc playerYPos
    adc playerPyHi
    sta PLAYER_OAM_START,y 
    iny 
    lda (pointerLo),y           ; tile
    sta PLAYER_OAM_START,y
    iny 
    lda (pointerLo),y           ; attribute
    sta PLAYER_OAM_START,y
    iny 
    lda (pointerLo),y
    clc 
    ; adc playerXPos
    adc playerPxHi
    sta PLAYER_OAM_START,y

    cpx #PLAYER_TILE_COUNT           ; exit loop after copying PLAYER_TILE_COUNT sprites for player
    beq @DoneDrawingPlayer

    iny                             ; increment y by 4 to start accessing next sprite meta data
    inx                             ; inc loop counter

    jmp @DrawPlayerLoopStart

@DoneDrawingPlayer:
    rts 


InitializePlayer:
    lda #$00
    sta playerState
    sta playerPxLo
    sta playerPxHi 
    sta playerPyLo 
    sta playerPyHi 
    sta playerVxLo
    sta playerVxHi
    sta playerVyLo
    sta playerVyHi
    sta playerAnimationOffset
    sta playerAnimationTimer
    sta playerFacingDirection
    rts 



   ;      y  tile  att   x    hi  lo var ?
   ; .byte $A0, $D0, $00, $40, >ToiletLetterGameLoop, <ToiletLetterGameLoop - 1, >DrawTextStatic2, <DrawTextStatic2 - 1, $FF, $00, $00, $00, $00


TestPerson1:
    ;       y       var3    att     x   hi_gameloop     lo_gameloop     hi_Draw     lo_darw     var1    var2    ani_offset      ani_timer   state
    .byte   $80    
; ok god
; we are going to assume:   
            ; that the sprite shuffler will take care of priority
            ; that we have space to write all 6 sprites to OAM
            ; that we have the sprite ram offset set for this sprite to use

; ok ok
; so what do we need to do:
            ; we need to check facing direction to know if horizontal offsets are postive or negative
            ; check state - first probably. if walking standing or something....
                ; lets just use var1 for state and var2 for facing dir? could combine them to save space but not sure if that is what I need to do right now
            ; use anchor position, facing direction, and sprite meta data table to know where each sprite goes in relation to anchor
            ; place sprites in OAM
            ; we also need to know which table to get the meta sprite data from. if i have multiple people, they all need their respective tables unless they look exactly the same.


; what is the flow
            ; one option which i think is the best way, is to do tables of tables again.
            ; so
            ; first we get the state, use that to
DrawPerson:
    ; first we need to know the state which is var1
    ldx currentGameObjectOffset     ; we will use x to store the current game obj offset to access the data

    ldy objectState,x                ; we get the state of the person and use that to get the TableOfPeopleSTATE:
    lda TableOfPeopleStatesHi,y
    sta pointerHi
    lda TableOfPeopleStatesLo,y 
    sta pointerLo 

    ; we beed to multiply each offset by 2 for the rest of the pointers
        ; don't need to do the first cause its split hi and lo

    lda objectVar3,x                   ; we are assuming var3 will hold the person offset. but god i might need 4 variables now
    asl 
    tay 
    lda (pointerLo),y
    sta pointer2Lo
    lda (pointerLo + 1),y
    sta pointer2Hi

    lda objectVar2,x        ; this is getting the table of person X, state Y, facing dir Z
    asl 
    tay 
    lda (pointer2Lo),y
    sta pointerLo
    lda (pointer2Lo + 1),y 
    sta pointerHi

    lda objectAnimationOffset,x ; this is getting the actual meta sprite table address
    asl 
    tay 
    lda (pointerLo),y 
    sta pointer2Lo
    lda (pointerLo + 1),y 
    sta pointer2Hi

    ; brute force it jabes, don't care about efficiency rn

    ; I am doing it dumb and having 4 tables for each facing dir. idc i'll fix it later
    ; so basically we should not give a shit now and just take info. add to anchor and move on
    ; ima read it normally, left to right top down so..
        ; 1 2
        ; 3 4
        ; 5 6

    lda objectXPos,x            ; this might be done by the draw engine idk
    sta currentX
    lda objectYPos,x 
    sta currentY

    ldx spriteBufferOffset      ; offset to write the data
    ldy #$00                    ; offset to access the sprite meta data
@DrawPersonLoopStart:
    lda (pointer2Lo),y          ; y pos
    clc 
    adc currentY
    sta SPRITE_OAM_START,x 

    
    lda (pointer2Lo + 1),y          ; tile
    sta SPRITE_OAM_START + 1,x 
    
    lda (pointer2Lo + 2),y          ; att
    sta SPRITE_OAM_START + 2,x 

    lda (pointer2Lo + 3),y          ; x pos
    clc  
    adc currentX
    sta SPRITE_OAM_START + 3,x 

    iny 
    iny 
    iny     
    iny 
    inx 
    inx 
    inx 
    inx 

    cpy #24         ; 6 sprites, 4 bytes each so at 24 we should be done
    bne @DrawPersonLoopStart

    stx spriteBufferOffset
    rts 

; tableOfPeopleStates (state offset):                       <standing, <walking          
; tableOfPeopleStanding (var3 offset):                      <person1Standing, >person1Standing, <person2Standing, ...
; tableOfPerson1Standing (facing dir):                      <person1StandingUp, >person1StandingUp, <person1StandingLeftRight, >person1StandingLeftRight
; tableOfPerson1StandingUp (animation offset):              <frame1, <frame2, <frame3, ...
; metaSpriteDataOfPerson1StandingFrame1:    ypos, tile, att, xpos

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

    lda #$00
    sta playerState
    sta globalTimerOffset

    lda #$40
    sta spriteBufferOffset

    jsr InitializePlayer
    jsr updateSprites

    jsr loadbackground


    ; setting DMA pointer

        ; ok so lets try and initialize the spriteRam pointer
 ;   lda GAME_OBJECT_RAM_START
   ; sta FirstFreeSlot      ; i think this is really stupid and wrong
   ; lda #$00
    ;sta NextFreeSlot

    ; so lets set the hi byte of the pointer to $03, and then the first and next slot to $00
    ; that should make it so when we add to next, it will start at address $0300, and then will be pointing at $0308, while first still points at $00, tbh idk if we need a first then... im kind of confused on why
    lda #<GAME_OBJECT_RAM_START
    sta gameObjectLo
    lda #>GAME_OBJECT_RAM_START
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

    jsr IncrementGlobalTimer
    jsr Timer2
    jsr ReadController1
    jsr PlayerLogic
    jsr PhysicsEngine
    ;jsr ApplyPlayerFriction
    jsr UpdatePlayerPosition
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

;.segment "RODATA"

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


RoomBasedEventsLo:
    .byte <DoNothing - 1, <LivingRoomTestFunction - 1, <DoNothing - 1, <BathroomBasedEvents - 1, <DoNothing - 1, <DoNothing - 1, <DoNothing - 1, <DoNothing - 1
RoomBasedEventsHi:
    .byte >DoNothing, >LivingRoomTestFunction, >DoNothing, >BathroomBasedEvents, >DoNothing, >DoNothing, >DoNothing, >DoNothing

RoomLoadingLo:
    .byte   <JamesRoomLoad - 1
    .byte   <LivingRoomLoad - 1, <ScottRoomLoad - 1, <BathRoomLoad - 1, <BalconyLoad - 1
RoomLoadingHi:
    .byte >JamesRoomLoad, >LivingRoomLoad, >ScottRoomLoad, >BathRoomLoad, >BalconyLoad

DefaultObjectStartLo:
    .byte <ScottStartingData
DefaultObjectStartHi:
    .byte >ScottStartingData


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Player Animation Tables
;
;           PlayerState:                                    Facing Direction:
;                           0   -   Standing                                    0   -   Down    
;                           1   -   Walking                                     1   -   Left
;                           2   -                                               2   -   Up
;                           3                                                   3   -   Right
;                           4    
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PLAYER_TILE_COUNT = 6
PlayerMetaSpriteDataLo:
    .byte <PlayerStanding, <PlayerWalking
PlayerMetaSpriteDataHi:
    .byte >PlayerStanding, >PlayerWalking


PlayerStanding:
    .byte <PlayerStandingDown, >PlayerStandingDown, <PlayerStandingLeft, >PlayerStandingLeft, <PlayerStandingUp, >PlayerStandingUp, <PlayerStandingRight, <PlayerStandingRight
PlayerWalking:
    .byte <PlayerWalkingDown, >PlayerWalkingDown, <PlayerWalkingLeft, >PlayerWalkingLeft, <PlayerWalkingUp, >PlayerWalkingUp, <PlayerWalkingRight, >PlayerWalkingRight

PlayerStandingDown:
    .byte <PlayerStandingDownFrame1, >PlayerStandingDownFrame1, <PlayerStandingDownFrame2, >PlayerStandingDownFrame2
PlayerStandingLeft:
    .byte <PlayerStandingLeftFrame1, >PlayerStandingLeftFrame1, <PlayerStandingLeftFrame2, >PlayerStandingLeftFrame2
PlayerStandingUp:
    .byte <PlayerStandingUpFrame1, >PlayerStandingUpFrame1, <PlayerStandingUpFrame2, >PlayerStandingUpFrame2
PlayerStandingRight:
    .byte <PlayerStandingRightFrame1, >PlayerStandingRightFrame1, <PlayerStandingRightFrame2, >PlayerStandingRightFrame2

PlayerWalkingDown:
    .byte <PlayerWalkingDownFrame1, >PlayerWalkingDownFrame1, <PlayerWalkingDownFrame2, >PlayerWalkingDownFrame2
PlayerWalkingLeft:
    .byte <PlayerWalkingLeftFrame1, >PlayerWalkingLeftFrame1, <PlayerWalkingLeftFrame2, >PlayerWalkingLeftFrame2
PlayerWalkingUp:
    .byte <PlayerWalkingUpFrame1, >PlayerWalkingUpFrame1, <PlayerWalkingUpFrame2, >PlayerWalkingUpFrame2
PlayerWalkingRight:
    .byte <PlayerWalkingRightFrame1, >PlayerWalkingRightFrame1, <PlayerWalkingRightFrame2, >PlayerWalkingRightFrame2


; TopLeft Tile Location:            frame 1     frame 2
;                           Down:      88           8A
;                           Left:      8C           8E
;                           Up:        B8           BA
; left to right top to bottom:                  y offset, Tile, Attribute, x offset
PlayerStandingDownFrame1:
    .byte $00, $88, $00, $00,   $00, $89, $00, $08,     $08, $98, $00, $00,     $08, $99, $00, $08,     $10, $A8, $00, $00,     $10, $A9, $00, $08
PlayerStandingDownFrame2:
    .byte $00, $00, $8A, $00,   $00, $8B, $00, $08,     $08, $9A, $00, $00,     $08, $9B, $00, $08,     $10, $AA, $00, $00,     $10, $AB, $00, $08

PlayerStandingLeftFrame1:
    .byte $00, $8C, $00, $00,   $00, $8D, $00, $08,     $08, $9C, $00, $00,     $08, $9D, $00, $08,     $10, $AC, $00, $00,     $10, $AD, $00, $08
PlayerStandingLeftFrame2:
    .byte $00, $8E, $00, $00,   $00, $8F, $00, $08,     $08, $9E, $00, $00,     $08, $9F, $00, $08,     $10, $AE, $00, $00,     $10, $AF, $00, $08


PlayerStandingUpFrame1:
    .byte $00, $B8, $00, $00,   $00, $B9, $00, $08,     $08, $C8, $00, $00,     $08, $C9, $00, $08,     $10, $D8, $00, $00,     $10, $D9, $00, $08
PlayerStandingUpFrame2:
    .byte $00, $BA, $00, $00,   $00, $BB, $00, $08,     $08, $CA, $00, $00,     $08, $CB, $00, $08,     $10, $DA, $00, $00,     $10, $DB, $00, $08


PlayerStandingRightFrame1:
    .byte $00, $8C, $40, $00,   $00, $8D, $40, $08,     $08, $9C, $40, $00,     $08, $9D, $40, $08,     $10, $AC, $40, $00,     $10, $AD, $40, $08
PlayerStandingRightFrame2:
    .byte $00, $8E, $40, $00,   $00, $8F, $40, $08,     $08, $9E, $40, $00,     $08, $9F, $40, $08,     $10, $AE, $40, $00,     $10, $AF, $40, $08


PlayerWalkingDownFrame1:
    .byte $00, $88, $00, $00,   $00, $89, $00, $08,     $08, $CD, $00, $00,     $08, $DD, $00, $08,     $10, $CE, $00, $00,     $10, $DE, $00, $08
PlayerWalkingDownFrame2:
    .byte $00, $88, $00, $00,   $00, $89, $00, $08,     $08, $ED, $00, $00,     $08, $FD, $00, $08,     $10, $EE, $00, $00,     $10, $EF, $00, $08

PlayerWalkingLeftFrame1:
    .byte $00, $00, $00, $00,   $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00
PlayerWalkingLeftFrame2:
    .byte $00, $00, $00, $00,   $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00

PlayerWalkingUpFrame1:
    .byte $00, $00, $00, $00,   $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00
PlayerWalkingUpFrame2:
    .byte $00, $00, $00, $00,   $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00

PlayerWalkingRightFrame1:
    .byte $00, $40, $00, $40,   $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00
PlayerWalkingRightFrame2:
    .byte $00, $40, $00, $40,   $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00,     $00, $00, $00, $00

    
   


PlayerStandingRightLeftMetaData1:
    .byte $A0, $11, $00, $20            ; frame 1 of animation
    .byte $FF
PlayerStandingRightLeftMetaData2:
    .byte $A0, $21, $00, $20            ; frame 2
    .byte $FF

PlayerStandingUpDownMetaData:
    .word PlayerStandingUpDownMetaData1, PlayerStandingUpDownMetaData1

PlayerStandingUpDownMetaData1:
    .byte $A0, $10, $00, $20
    .byte $FF
PlayerStandingUpDownMetaData2:
    .byte $A0, $20, $00, $20
    .byte $FF

TextTableLo:
    .byte <SampleText, <SampleText2
TextTableHi:
    .byte >SampleText, >SampleText2

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

SampleText2:
    .byte       $00, $D0, $00, $00
    .byte       $08, $D1, $00, $00
    .byte       $10, $D2, $00, $00
    .byte       $FF


;; the numbers as words should be the tile location for that number. the code of that is somehwere else maybe at the top.
; this should make it easier when I eneiviatbly change the chr file
TIMER_X_ANCHOR  = $10
TIMER_Y_ANCHOR  = $10
COLON = $0A ; this is a placeholder i don't have a colon sprite.
TimerHourMetaSpriteTable:
    .byte   $00, ZERO,  $00, $00,       $00, ONE,     $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ZERO,  $00, $00,       $00, TWO,     $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ZERO,  $00, $00,       $00, THREE,   $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ZERO,  $00, $00,       $00, FOUR,    $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ZERO,  $00, $00,       $00, FIVE,    $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ZERO,  $00, $00,       $00, SIX,     $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ZERO,  $00, $00,       $00, SEVEN,   $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ZERO,  $00, $00,       $00, EIGHT,   $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ZERO,  $00, $00,       $00, NINE,    $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ONE,   $00, $00,       $00, ZERO,    $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ONE,   $00, $00,       $00, ONE,     $00, $08,         $00, COLON, $00, $10,       $FF
    .byte   $00, ONE,   $00, $00,       $00, TWO,     $00, $08,         $00, COLON, $00, $10,       $FF


TimerMinuteMetaSpriteTable:
    .byte   $00, ZERO,  $00, $18,       $00, $00, ZERO, $00, $20,       $FF
    .byte   $00, ONE,   $00, $18,       $00, $00, FIVE, $00, $20,       $FF
    .byte   $00, THREE, $00, $18,       $00, $00, ZERO, $00, $20,       $FF
    .byte   $00, FOUR,  $00, $18,       $00, $00, FIVE, $00, $20,       $FF




; Facing Direction ( 0: Down    1: Left     2: Up   3: Right )

TableOfPeopleStatesLo:
    .byte <TableOfPeopleStanding 
TableOfPeopleStatesHi:
    .byte >TableOfPeopleStanding

TableOfPeopleStanding:
    .byte <Person1Standing, >Person1Standing
 
Person1Standing:
    .byte  <Person1StandingDown, >Person1StandingDown, <Person1StandingLeft, >Person1StandingLeft, <Person1StandingUp, >Person1StandingUp, <Person1StandingRight, >Person1StandingRight
Person1StandingDown:
    .byte <Person1StandingDownFrame1, >Person1StandingDownFrame1, <Person1StandingDownFrame2, >Person1StandingDownFrame2
Person1StandingLeft:
    .byte <Person1StandingLeftFrame1, >Person1StandingLeftFrame1, <Person1StandingLeftFrame2, >Person1StandingLeftFrame2
Person1StandingUp:
    .byte <Person1StandingUpFrame1, >Person1StandingUpFrame1, <Person1StandingUpFrame2, >Person1StandingUpFrame2
Person1StandingRight:
    .byte <Person1StandingRightFrame1, >Person1StandingRightFrame1, <Person1StandingRightFrame2, >Person1StandingRightFrame2

Person1StandingDownFrame1:
    .byte $00, $00, $00, $00
Person1StandingDownFrame2:
    .byte $00, $00, $00, $00

Person1StandingLeftFrame1:
    .byte $00, $00, $00, $00
Person1StandingLeftFrame2:
    .byte $00, $00, $00, $00

Person1StandingUpFrame1:
    .byte $00, $00, $00, $00
Person1StandingUpFrame2:
    .byte $00, $00, $00, $00

Person1StandingRightFrame1:
    .byte $00, $00, $00, $00
Person1StandingRightFrame2:
    .byte $00, $00, $00, $00





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