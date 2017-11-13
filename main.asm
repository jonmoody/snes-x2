.INCLUDE "Header.inc"
.INCLUDE "Snes_Init.asm"
.INCLUDE "LoadGraphics.asm"

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

.equ spriteXLocation $1000
.equ spriteYLocation $1001

.enum $1200
  Joy1Raw     dw      ; Holder of RAW joypad data from register (from last frame)
  Joy1Press   dw      ; Contains only pressed buttons (not held down)
  Joy1Held    dw      ; Contains only buttons that are Held
.ende

; $4218
.equ Button_A		$80
.equ Button_X		$40
.equ Button_L		$20
.equ Button_R		$10
; $4219
.equ Button_B		$80
.equ Button_Y		$40
.equ Button_Select	$20
.equ Button_Start	$10
.equ Button_Up		$08
.equ Button_Down	$04
.equ Button_Left	$02
.equ Button_Right	$01

Start:
  InitSNES

  rep #$10
  sep #$20

  lda #($80-16)
  sta spriteXLocation
  lda #(224/2 - 16)
  sta spriteYLocation

  lda #%00001001
  sta $2105

  ; Blue Background
  stz $2121
  lda #$40
  sta $2122
  sta $2122

  LoadPalette SpritePalette, 128, 16
  LoadBlockToVRAM Sprite, $0000, $0800

  LoadPalette SpritePalette, 0, 16

  jsr SpriteInit

  ; Put sprite in the center of the screen
  lda spriteXLocation
  sta $0000
  lda spriteYLocation
  sta $0001

  stz $0002
  lda #%01110000
  sta $0003

  lda #%01010100
  sta $0200

  ; This modifies the background tiles
  ; ldx #$0400
  ; stx $2116
  ; lda #$00
  ; sta $2118

  jsr SetupVideo

  stz $4016

  lda #$81
  sta $4200       ; Enable NMI

EternalTorment:
  jmp EternalTorment

SpriteInit:
	php

	rep	#$30	;16bit mem/A, 16 bit X/Y

	ldx #$0000
  lda #$0001
_setoffscr:
  sta $0000,X
  inx
  inx
  inx
  inx
  cpx #$0200
  bne _setoffscr

	ldx #$0000
	lda #$5555
_clr:
	sta $0200, X		;initialize all sprites to be off the screen
	inx
	inx
	cpx #$0020
	bne _clr

	plp

	rts

SetupVideo:
  php

  rep #$10
  sep #$20

  stz $2102
  stz $2103

  lda #$04
  sta $2107

  stz $210B

  jsr TransferSpriteData

	lda #%10100000
  sta $2101

  lda #%00010001    ; Enable BG1
  sta $212C

  lda #$FF
  sta $210E
  sta $210E

  lda #$0F
  sta $2100         ; Turn on screen, full Brightness

  plp

  rts

;============================================================================

Gameloop:
  rep #$30        ; A/mem=16 bits, X/Y=16 bits (to push all 16 bits)

  ; Push registers
  phb
	pha
	phx
	phy
	phd

  sep #$20        ; A/mem=8 bit

  jsr Joypad

  jsr ProcessControllerInput

  jsr MoveSprite

  jsr TransferSpriteData

  lda $4210       ; Clear NMI flag
  rep #$30        ; A/Mem=16 bits, X/Y=16 bits

  ; Pull registers
  pld
	ply
	plx
	pla
	plb

  sep #$20

  rti

ProcessControllerInput:

ReadUp:
  lda $4219
  and #Button_Up
  beq ReadDown

  dec spriteYLocation

ReadDown:
  lda $4219
  and #Button_Down
  beq ReadLeft

  inc spriteYLocation

ReadLeft:
  lda $4219
  and #Button_Left
  beq ReadRight

  dec spriteXLocation

  lda #%01110000
  sta $0003

ReadRight:
  lda $4219
  and #Button_Right
  beq ProcessControllerInputEnd

  inc spriteXLocation

  lda #%00110000
  sta $0003

ProcessControllerInputEnd:
  rts

MoveSprite:
  lda spriteXLocation
  sta $0000
  lda spriteYLocation
  sta $0001

  rts

TransferSpriteData:
  stz $2102		; set OAM address to 0
  stz $2103

  ldy #$0400
  sty $4300		; CPU -> PPU, auto increment, write 1 reg, $2104 (OAM Write)
  stz $4302
  stz $4303		; source offset
  ldy #$0220
  sty $4305		; number of bytes to transfer
  lda #$7E
  sta $4304		; bank address = $7E  (work RAM)
  lda #$01
  sta $420B		;start DMA transfer

  rts

Joypad:
  lda $4212           ; auto-read joypad status
  and #$01            ;
  bne Joypad          ; read is done when 0

  rep #$30            ; A/X/Y - 16 bit

  ; Player 1
  ldx Joy1Raw         ; load log of last frame's RAW read of $4218
                      ; the log will be 0 the first time read of course..
  lda $4218           ; Read current frame's RAW joypad data
  sta Joy1Raw         ; save it for next frame.. (last frame log is still in X)
  txa                 ; transfer last frame input from X -> A (it's still in X)
  eor Joy1Raw         ; Xor last frame input with current frame input
                      ; shows the changes in input
                      ; buttons just pressed or just released become set.
                      ; Held or unactive buttons are 0
  and Joy1Raw         ; AND changes to current frame's input.
                      ; this ends up leaving you with the only the buttons that
                      ; are pressed.. It's MAGIC!
  sta Joy1Press       ; Store just pressed buttons
  txa                 ; Transfer last frame input from X -> A again
  and Joy1Raw	        ; Find buttons that are still pressed (held)
  sta Joy1Held        ; by storing only buttons that are pressed both frames

  ; Joypads standard (ie not a mouse or superscope..) and connected?
  sep #$20
  ldx #$0000      ; we'll clear recorded input if pad is invalid

  lda $4016       ; Pad 1 - now we read this (after we stored a 0 to it earlier)
  bne _done     ; $4016 returns 0 if not connected, 1 if connected
  stx Joy1Raw     ; otherwise clear all recorded input.. it's not valid..
  stx Joy1Press
  stx Joy1Held

_done:
  rts

.ends

;============================================================================
; Character Data
;============================================================================
.BANK 1 SLOT 0
.ORG 0
.SECTION "CharacterData"
Sprite:
    .INCBIN "graphics/biker.pic"

SpritePalette:
    .INCBIN "graphics/biker.clr"

.ENDS
