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
    gameObjectLo:   .res 1  ; pointer variables for storing the game objects in ram
    gameObjectHi:   .res 1
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
    
    scottDataStartLo: .res 1
    scottDataStartHi: .res 1
    ;; Game Engine shit
    spriteRamStart = $0300
    objectMax = $20
  ;  object_y_pos = spriteramstart + object_max*0
   ; object_tile = spriteramstart +object_max*1
   ; object_att = spriteramstart + object_max*2
   ; object_x_pos = spriteramstart + object_max*3
   ; object_script_lo = spriteramstart + object_max*4
   ; object_script_hi = spriteramstart + object_max *5
   ; object_var_1 = spriteramstart + object_max * 6
   ; object_var_2 = spriteramstart + object_max * 7
    
    objectNext = spriteRamStart + objectMax * 0 ; ok we are going to try this implementation i guess

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

    lda bathroomFlag
    cmp #$00
    bne @ToiletInteractDone  ; text is still there. i don't want to reset the text timer cause idc
    jsr ToiletInteractSetSprites

@ToiletInteractDone:
    rts 

    

ToiletInteractSetSprites:
    lda #$3C
    sta bathroomFlag

    ldy #$00

    lda #$10                                ; G
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$D0
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$88
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$10                                ; O
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$D8
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$90
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$10                                ; P        
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$D9
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$A0
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$10                                ; I
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$D2
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$A8
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$10                                ; S
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$DC
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$B0
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$10                                ; S
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$DC
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$B8
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$18                                ; G
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$D0
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$90
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$18                            ; I
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$D2
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$98                            
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$18                            ; R
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$DB
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$A0
    sta bathroomToiletSpriteStart, y 
    iny 

    lda #$18                            ; L
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$D5
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$00
    sta bathroomToiletSpriteStart, y 
    iny 
    lda #$A8
    sta bathroomToiletSpriteStart, y  
    
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

 CreateGameObject2:
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
    jsr loadpalettes

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



   ; ldy #$00
   ; lda DefaultObjectStartLo, y
   ; sta pointerLo
   ; lda DefaultObjectStartHi, y
   ; sta pointerHi
   ; jsr CreateGameObject

    
   ; ldy #$00
   ; lda DefaultObjectStartLo, y
   ; sta pointerLo
  ;  lda DefaultObjectStartHi, y
  ;  sta pointerHi
   ; jsr CreateGameObject



    ldx #$02

    ;lda controller1
   ; and #%00001000      ; checking if up is pressed
   ; beq upNotPressed
   ; jsr moveUp
upNotPressed:
 ;   lda controller1
  ;  and #%00000100      ; checking if down is pressed
  ;  beq downNotPressed
   ; jsr moveDown
downNotPressed:
   ; lda controller1
   ; and #%00000010
   ; beq leftNotPressed
   ; jsr moveLeft
leftNotPressed:
   ; lda controller1
   ; and #%00000001
   ; beq rightNotPressed
   ; jsr moveRight
rightNotPressed:
    ;lda playerXpos
   ; sta $0203
   ; lda playerYpos
   ; sta $0200

    jsr DoAction

    jsr ControllerLogic
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