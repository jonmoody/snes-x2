.INCLUDE "Header.inc"
.INCLUDE "Snes_Init.asm"
.INCLUDE "LoadGraphics.asm"

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

Start:
  InitSNES

  rep #$10
  sep #$20

  lda #%00001001
  sta $2105

  ; Blue Background
  stz $2121
  lda #$40
  sta $2122
  sta $2122

  LoadPalette SpritePalette, 128, 16
  LoadBlockToVRAM Sprite, $0000, $0800

  jsr SpriteInit

  ; Put sprite in the center of the screen
  lda #($80-16)
  sta $0000
  lda #(224/2 - 16)
  sta $0001

  stz $0002
  lda #%01110000
  sta $0003

  lda #%01010100
  sta $0200

  jsr SetupVideo

  lda #$80
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

  jsr TransferSpriteData

	lda #%10100000
  sta $2101

  lda #%00010000    ; Enable BG1
  sta $212C

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
