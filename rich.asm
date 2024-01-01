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
    controller1:    .res 1  ; controller 1 byte to store what buttons are pressed each frame
   ; playerXpos:     .res 1  
   ; playerYpos:     .res 1
    nameTable:      .res 1  ; which nametable to load
    roomIndex:      .res 1
    spriteCount:    .res 1
    frameCounter60: .res 1
    gameTime:       .res 2
    fifteenSeconds:      .res 1
    score:          .res 1
    
    temp1:          .res 1
    temp2:          .res 1

    flag1:        .res 1    ; bits: X - X - X - X - X - X - NMI Flag - Lag Frame Flag
    
    controller1PreviousInput:   .res 1
    controller1Pressed:         .res 1
    controller1Held:            .res 1

    playerState:                .res 1 ; Drinking - Smoking - Peeing - Walking - (5-8) Beers in Inv
    playerAnimationCounter:             .res 1  
    ;; Constants

    distanceTestValueX:       .res 1
    distanceTestValueY:       .res 1
    distanceTestResult:       .res 2
    scoreIncrementOnes: .res 1
    scoreIncrementTens: .res 1

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
    cpx #$30
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
    iny                         ; y = 5
    iny                         ; y = 6
    dex                         
    cpx #$00
    BNE checkLoadingLoop
    rts                         ; loop ended no loading zones found

loadingZoneFound:
    iny                     ; y = 3
    lda (pointerLo), y      ; storing roomIndex that loading zone loads into
    sta roomIndex
    iny                        ; Setting starting pos of player based of what loading zone triggered
    lda (pointerLo), Y      ; y = 4
    sta playerXpos
    iny
    lda (pointerLo), Y      ; y = 5
    sta playerYpos

    jsr loadbackground
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
;   hoping its based off roomID?
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
    rts 
DoAction:
    lda roomIndex
    tax 
    lda RoomBasedEventsHi,x
    pha 
    lda RoomBasedEventsLo,x
    pha  
    rts 

DoNothing:
    rts 



PlayerLogic:
    lda playerState         ; Drinking - Walking - Peeing - Smoking - x - X - Facing Direction ( 0: Down    1: Left     2: Up   3: Right )
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
    lda playerTile                   ; flipping between sprites 0 and 1 for now
    eor #%00000001
    sta playerTile

@ControllerChecking:                    ; check input
    lda controller1Pressed
    asl     ; start?
    asl     ; select?
    asl     ; a?
    asl     ; b? i need to code more often cause i should remember the fucking order of user inputs 
    asl ; up
    bcs @StandingUpPressed      ; right now i only read one input. also need to incorperate the actual player movement into this instead of having it be in this old function that sucks balls
    asl ; down
    bcs @StandingDownPressed
    asl ; left
    bcs @StandingLeftPressed
    asl ; right
    bcs @StandingRightPressed
    jmp @DoneStanding

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
    jsr WalkingLogicStart
                                  ; standing animation sprite update
@DoneStanding:        
    rts 

StandingStateStart:
    lda #$00  
    sta playerAnimationCounter
    lda #$00                        ; first sprite of standing animation (obvi temp idk what ima do for animation but make it bad at first tand then better lets goooo)
    sta playerTile

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

    ;; first animation.
    lda playerAnimationCounter
    clc 
    adc #$01
    sta playerAnimationCounter
    ;; this is where animation shit would go? maybe just have like
    ; jsr WalkingAnimation?
    ;; but I want to at least inc the counter cause i have it so might as well think about it.


    ;; ok now just apply the movement?
    lda controller1PreviousInput
    ror ; right?
    bcc @NotMovingRight
    ;; ok im yolo trying to use the stack to save this address. yolo you know
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

    jsr loadpalettes

    jsr loadSprites
    jsr updateSprites

    jsr loadbackground


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

RoomBasedEventsLo:
    .byte <DoNothing - 1, <LivingRoomTestFunction - 1, <DoNothing - 1, <DoNothing - 1, <DoNothing - 1, <DoNothing - 1, <DoNothing - 1, <DoNothing - 1
RoomBasedEventsHi:
    .byte >DoNothing, >LivingRoomTestFunction, >DoNothing, >DoNothing, >DoNothing, >DoNothing, >DoNothing, >DoNothing



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
    .byte $28, $B0, $01, $C0, $44   
    .byte $28, $B8, $01, $C0, $44

LivingRoomLoadZone:
    .byte $0A
    .byte $C8, $40, $00, $38, $B6 ; james room
    .byte $C8, $48, $00, $38, $B6

    .byte $C8, $68, $02, $38, $34 ; scotts room
    .byte $C8, $60, $02, $38, $34 

    .byte $B0, $28, $03, $A3, $68 ; bathroom
    .byte $B8, $28, $03, $A3, $68

    .byte $30, $B8, $04, $C0, $84 ; balcony
    .byte $30, $B0, $04, $C0, $84

    .byte $78, $28, $04, $C0, $50 ; balcony lower
    .byte $70, $28, $04, $C0, $50

ScottRoomLoadZone:
    .byte $02
    .byte $28, $30, $01, $C0, $63   ; living room
    .byte $28, $38, $01, $C0, $63

BathroomLoadZone:
    .byte $02
    .byte $A0, $78, $01, $B3, $30
    .byte $A8, $78, $01, $B3, $30
BalconyLoadZone:
    .byte $02
    .byte $D0, $80, $01, $38, $B4
    .byte $D0, $88, $01, $38, $B4
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

JamesRoomStartPos:
    .byte $80,$80
LivingRoomStartPos:
    .byte $40,$40
ScottRoomStartPos:
    .byte $80,$80
BathroomStartPos:
    .byte $80,$80
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
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24
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
    .byte $80, $00, $00, $80 ; YCoord, tile number, attr, XCoord
    .byte $10, $00, $00, $10        ; 1's digit of timer sprite
    .byte $10, $00, $00, $08        ; 10s digit of timer sprite
    .byte $20, $00, $00, $10        ; 1s digit of timer2
    .byte $20, $00, $00, $08        ; 10s digit of timer 2

    .byte $20, $00, $00, $80        ; score 1s
    .byte $20, $00, $00, $78        ; score 10s
    .byte $20, $00, $00, $70        ; score 100s
    .byte $20, $09, $00, $68        ; score 1000s
    .byte $20, $00, $00, $60        ; score 10000s

    .byte $80, $01, $00, $80        ; sprite pos for distance testing
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
    .byte %00000010, %00000010, %00000010, %01000000
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