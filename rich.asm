.segment "HEADER"
	
	.byte	"NES", $1A	; iNES header identifier
	.byte	2		    ; 2x 16KB PRG code - lists how much program rom you have
	.byte   1		    ; 1x  8KB CHR data - lists how much chr rom you have
	.byte   $01, $00	; mapper 0, vertical mirroring
    ; NEED TO FIGURE OUT WHAT THESE DO below this line
    .byte   $00
    .byte   $00
    .byte   $00
    .byte   $00, $00, $00, $00, $00

.segment "ZEROPAGE"
.segment "STARTUP"
.segment "CODE"

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
vblankwait1:
	bit	$2002
	bpl	vblankwait1

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

	;; second wait for vblank, PPU is ready after this
vblankwait2:
	bit	$2002
	bpl	vblankwait2
    
.segment "VECTORS"
.segment "CHARS"