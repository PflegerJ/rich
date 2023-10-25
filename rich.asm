.segment "HEADER"
	
    ; These values tell the emulator what type of cartirdge we are using with what extra features
    ;   the cartridge has. lets start with 0 features and see if we need to learn about them for this game

    ; 16 byte header
	.byte	"NES", $1A	; iNES header identifier
	.byte	2		    ; 2x 16KB PRG code - lists how much program rom you have
	.byte   1		    ; 1x  8KB CHR data - lists how much chr rom you have
	.byte   $01         ; mapper 0
    .byte   $00	        ; vertical mirroring off
    .byte   $00         ; iNES_SRAM
    .byte   $00         ; iNES Mapper?
    .byte   $00         ; iNES Mapper?
    .byte   $00, $00, $00, $00, $00  ; padding

.segment "ZEROPAGE"
    ;; Variables
    ;; Constants
    WALL_TILE   = $85
    RIGHT_WALL  = $E5
    TOP_WALL    = $20
    BOTTOM_WALL = $E0
    LEFT_WALL   = $04

.segment "STARTUP"
.segment "CODE"

    ;; Subroutines
vblankwait:
    bit $2002   
    bpl vblankwait
    rts

;; load palette loads the palette data into the PPU palette RAM
;; addresses are hard coded for now
loadpalette:
    LDA $2002   ; clears PPU status reg
    LDA #$3f    ; upper bit of palette address
    STA $2006   ; store in ppu address reg
    LDA #$00    ; lower bit of palette address
    STA $2006

    LDX #$00    ; make sure X is $00
loadpaletteloop:
    LDA palettedata,X
    STA $2007
    INX 
    CPX #$20
    BNE loadpaletteloop
    rts

loadnametable:
    LDA $2002   ; read PPU to reset high/low latch
    LDA #$20
    STA $2006   ; write high byte of $2000 address
    LDA #$00
    STA $2006   ; write low byte
    LDX #$00
:
    LDA nametabledata,X
    STA $2007           ; write to PPU
    INX 
    CPX #$FF            ; compare X to $80, decimal 128, as copying 128 bytes
    BNE :-
    rts

loadattribute:
    LDA $2002
    LDA #$23    ; high byte of $23C0
    STA $2006
    LDA #$c0    ; low byte
    STA $2006
    LDX #$00
:
    LDA attributedata,X
    STA $2007   ; write to PPU
    INX 
    CPX #$08    ; copying 8 bytes of data
    BNE :-
    rts

RESET:
    sei			; disable IRQs
	cld			; disable decimal mode
	ldx	#$40
	stx	$4017		; dsiable APU frame IRQ
	ldx	#$ff		; Set up stack
	txs			;  +
	inx			; now X = 0
	stx	$2000		; disable NMI
	stx	$2001		; disable rendering
	stx	$4010		; disable DMC IRQs

	;; first wait for vblank to make sure PPU is ready
    jsr vblankwait

clear_memory:
	lda	#$00
	sta	$0000, x
	sta	$0100, x
	sta	$0300, x
	sta	$0400, x
	sta	$0500, x
	sta	$0600, x
	sta	$0700, x
	lda	#$fe
	sta	$0200, x	; move all sprites off screen
	inx
	bne	clear_memory

    ; Other things you can do between vblank waits are set up audio
    ; or set up other mapper registers.


	;; second wait for vblank, PPU is ready after this 
    jsr vblankwait

    jsr loadpalette

    jsr loadnametable

    jsr loadattribute

    CLI 
    LDA #%10010000  ; enable NMI, sprites from pattern table 0, background from pattern table 1
    STA $2000

    LDA #%10001110  ; background and sprites enable, no clipping on left
    STA $2001
    ; load initial sprite info

forever:
    jmp forever

;;;;;; vblank loop - called every frame ;;;;;
VBLANK:
    rti 

palettedata:
    .byte $22, $29, $1a, $0F, $22, $36, $17, $0F, $22, $30, $21, $0F, $0f, $0f, $0f, $0F  ; background palette data
    .byte $22, $16, $27, $18, $22, $1A, $30, $27, $22, $16, $30, $27, $0f, $0F, $0f, $0f  ; sprite palette data 

attributedata:
    .byte %11111111, %00100111, %00100000, %00000000, %00000000, %00000000, %00000000, %00110000

nametabledata:
    .byte $24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; row1
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   ; row 2
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$00,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; row1
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$24,$24,$24,$24


.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "mario.chr"