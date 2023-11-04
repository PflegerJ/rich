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
    blANK:          .res 8
    controller1:    .res 1  ; controller 1 byte to store what buttons are pressed each frame
   ; playerXpos:     .res 1  
   ; playerYpos:     .res 1
    nameTable:      .res 1  ; which nametable to load
    roomIndex:      .res 1
    spriteCount:    .res 1
    temp1:          .res 1
    temp2:          .res 1
    ;; Constants

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
    playerYpos            = $0204       ; sprite 1 y pos


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
    cpx #$08
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

readcontroller1:
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016
    LDX #$08
readcontroller1loop:
    LDA $4016
    LSR A           ; Logical shift right - all bits in A are shifted to the right, bit7 is 0 and whatever is in bit0 goes to Carry flag
    ROL controller1    ; Rotate left - opposite of LSR
    ;; used as a smart way to read controller inputs, as when each button is read, the button data is in bit0, and doing LSR puts the button 
    ;; in the Carry. Then ROL shifts the previous button data over and puts the carry back into bit0
    DEX 
    BNE readcontroller1loop
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
    
forever:
    jmp forever



;;;;;; vblank loop - called every frame ;;;;;
VBLANK:

    jsr updateSprites

    jsr readcontroller1

    lda controller1
    and #%00001000      ; checking if up is pressed
    beq upNotPressed
    jsr moveUp
upNotPressed:
    lda controller1
    and #%00000100      ; checking if down is pressed
    beq downNotPressed
    jsr moveDown
downNotPressed:
    lda controller1
    and #%00000010
    beq leftNotPressed
    jsr moveLeft
leftNotPressed:
    lda controller1
    and #%00000001
    beq rightNotPressed
    jsr moveRight
rightNotPressed:
    ;lda playerXpos
   ; sta $0203
   ; lda playerYpos
   ; sta $0200

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   LoadZone information
;   
;   byte 1:     count
;   byte 2-4:   x pos, y pos, next roomIndex, starting x pos, starting y pos
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

    .byte $30, $B8, $04, $80, $80 ; balcony
    .byte $30, $B0, $04, $80, $80

    .byte $78, $28, $06, $80, $80 ; outside 1
    .byte $70, $28, $06, $80, $80

ScottRoomLoadZone:
    .byte $02
    .byte $28, $30, $01, $C0, $63   ; living room
    .byte $28, $38, $01, $C0, $63

BathroomLoadZone:
    .byte $02
    .byte $A0, $78, $01, $B3, $30
    .byte $A8, $78, $01, $B3, $30
BalconyLoadZone:
    .byte $00

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