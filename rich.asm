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
    playerXpos:     .res 1
    playerYpos:     .res 1
    nameTable:      .res 1  ; which nametable to load
    roomIndex:      .res 1
    temp1:           .res 1
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
    
   ; WALL_TILE   = $85
  ;  RIGHT_WALL  = $E5
   ; TOP_WALL    = $20
   ; BOTTOM_WALL = $E0
   ; LEFT_WALL   = $04


.segment "CODE"

    ;; Subroutines
vblankwait: 
    bit PPU_STATUS   
    bpl vblankwait
    rts

loadpalettes:
    LDA PPU_STATUS
    LDA #$3f
    STA PPU_ADDRESS
    LDA #$00
    STA PPU_ADDRESS
    LDX #$00
loadpalettesloop:
    LDA palette,X   ; load data from adddress (palette + X)
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
    lda RoomTableLo, x
    sta pointerLo
    lda RoomTableHi, X
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
:
    LDA attributes,X
    STA PPU_DATA   ; write to PPU
    INX 
    CPX #$40    ; copying 8 bytes of data
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

moveUp:
    dec playerYpos
    lda playerYpos
    clc
    adc #$01
    tay
    ldx playerXpos
    jsr check_background_collision
    beq @done
    inc playerYpos
@done:
    rts

moveDown:
    inc playerYpos
    ldy playerYpos
    ldx playerXpos
    jsr check_background_collision
    beq @done
    DEC playerYpos
@done:
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
    
@noCollision:
    jsr checkLoadingZone
    rts

moveLeft:
    dec playerXpos
    ldy playerYpos
    ldx playerXpos
    jsr check_background_collision
    beq @done
    inc playerXpos
@done:
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
    tay         ; store value in Y

    TXA
    lsr
    lsr
    lsr
    and #%0111
    Tax

    lda hitTable1, y
    and bitMask, x      ; beq means not collide         bne means collide

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checks if the player has enterered a loadingzone
;; uses the roomIndex to get the LoadZone
;; loadzone table structure: 
;;                  1st byte is the count
;;                  2nd byte is x pos
;;                  3rd byte is y pos
checkLoadingZone:
    ldx roomIndex           ; uses roomIndex for set pointer to the array of
    lda LoadZoneLo, X       ;   loading zones for the room we are in
    sta pointerLo
    lda LoadZoneHi, X
    sta pointerHi

    lda playerXpos
    and #%11111000
    sta temp1
    lda playerYpos
    and #%11111000
    sta temp2
    ldy #$00                ; y is used as our offset to go through the loop
    lda (pointerLo), y
    tax                     ; first value in loadingzone table is the counter
    iny                     ; store the counter in x
checkLoadingLoop:


    lda (pointerLo), y
    cmp temp1          ; compare x pos of player with x pos of loading zone
    bne @bottomOfLoopNoYCheck
    iny                     ; increment offset
    lda (pointerLo), y      
    cmp temp2          ; compare y pos of player with y pos of loading zone
    beq loadingZoneFound    ; both x and y pos must be equal so player is in loading zone
    bne @bottomOfLoop
@bottomOfLoopNoYCheck:
    iny
@bottomOfLoop:
    iny                     ; increment y if for next loop iteration if there are multiple possible loading zones in this map
    iny
    dex                     
    cpx #$00
    BNE checkLoadingLoop
    rts                     ; loop ended no loading zones found

loadingZoneFound:
    iny                     ; inc y
    lda (pointerLo), y
    sta roomIndex
    jsr loadbackground
    rts

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


loadsprites:
    lda #$80            ; setting starting player position
    sta playerXpos
    sta playerYpos

    LDX #$00
loadspritesloop:
    LDA sprites,X
    STA $0200,X
    INX 
    CPX #$04
    BNE loadspritesloop 

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

    lda #$00
    sta PPU_SPR_ADDR
    lda #$02
    sta SPR_DMA

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
    lda playerXpos
    sta $0203
    lda playerYpos
    sta $0200

    RTI

RoomTableLo:
    .byte <background1, <background2
RoomTableHi:
    .byte >background1, >background2

LoadZoneLo:
    .byte <Room1LoadZones, <Room2LoadZones
LoadZoneHi:
    .byte >Room1LoadZones, >Room2LoadZones


Room1LoadZones:
    .byte $04,  $20, $40, $01,   $20, $48, $01,  $20, $50, $01,  $20, $58, $01
Room2LoadZones:
    .byte $01, $80, $80, $00

palette:
    .byte $22, $29, $1a, $0F, $22, $36, $17, $0F, $22, $30, $21, $0F, $0f, $0f, $0f, $0F  ; background palette data
    .byte $22, $16, $27, $18, $22, $1A, $30, $27, $22, $16, $30, $27, $0f, $0F, $0f, $0f  ; sprite palette data 

attributes:  ;8 x 8 = 64 bytes
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %11111111, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

background1:
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; row1
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 3
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 4
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 5
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 6
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 7
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 7
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 8
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 9
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 10
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 11
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 12
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 13
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 14
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 15
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 16
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 17
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 18
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 19
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 20
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 21
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 22
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 23
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 26
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 27
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; row 30
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

background2:
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; row1
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 3
    .byte $24,$24,$24,$24,$24,$24,$00,$00,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 4
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 5
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 6
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 7
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 7
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 8
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 9
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 10
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 11
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 12
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 13
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 14
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 15
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 16
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 17
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 18
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 19
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 20
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 21
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 22
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 23
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 26
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 27
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; row 30
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

sprites:
    .byte $80, $00, $00, $80 ; YCoord, tile number, attr, XCoord


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Hit table for default background collisions
;;      - each bit represent the tile in that location
;;      - 0: collision off      1: collision on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
hitTable1:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00001111, %11111111, %11111111, %11110000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00010000, %00000000, %00000000, %00010000
    .byte %00010000, %00000000, %00000000, %00010000
    .byte %00010000, %00000000, %00000000, %00010000
    .byte %00010000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001111, %11111111, %11111111, %11110000

    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

hitTable2:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00001111, %11111111, %11111111, %11110000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000011, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000

    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001000, %00000000, %00000000, %00010000
    .byte %00001111, %11111111, %11111111, %11110000

    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000


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