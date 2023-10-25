.segment "HEADER"
	
    ; These values tell the emulator what type of cartirdge we are using with what extra features
    ;   the cartridge has. lets start with 0 features and see if we need to learn about them for this game

    ; 16 byte header
	.byte	"NES", $1A	; iNES header identifier
	.byte	2		    ; 2x 16KB PRG code - lists how much program rom you have
	.byte   1		    ; 1x  8KB CHR data - lists how much chr rom you have
	.byte   $01,        ; mapper 0
    .byte   $00	        ; vertical mirroring off
    .byte   $00         ; iNES_SRAM
    .byte   $00         ; iNES Mapper?
    .byte   $00         ; iNES Mapper?
    .byte   $00, $00, $00, $00, $00  ; padding

.segment "ZEROPAGE"
    ;; Variables
    ;; Constants
.segment "STARTUP"
.segment "CODE"

    ;; Subroutines
vblankwait:
    bit $2002   
    bpl vblankwait
    rts

reset:
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
    jsr vblankwait:

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

    ; load initial sprite info

forever:
    jmp forever

;;;;;; vblank loop - called every frame ;;;;;
VBLANK:
    

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0
.segment "CHARS"