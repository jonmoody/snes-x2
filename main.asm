.include "Header.inc"
.include "Snes_Init.asm"

.include "LoadGraphics.asm"

.bank 0 slot 0
.org 0
.section "MainCode"

Start:
  InitSNES

  LoadPalette SpritePalette, 128, 16
  LoadBlockToVRAM Sprite, $0000, $0800

  jsr SpriteInit

  lda #(256/2 - 16)
  sta $0000           ; Sprite X-Coordinate

  lda #(224/2 - 16)   ; Sprite Y-Coordinate
  sta $0001

  lda #%01010100  ; clear X-MSB
  sta $0200


  lda #$80
  sta $2115

  ldx #$0400
  stx $2116
  lda #$02
  sta $2118

  ldx #$0510
  stx $2116
  lda #$03
  sta $2118

  ldx #$0620
  stx $2116
  lda #$01
  sta $2118

  jsr SetupVideo

  ; Loop forever.
EternalTorment:
  jmp EternalTorment

VBlank:
  rti

SetupVideo:
  rep #$10
  sep #$20            ; 8bit A, 16bit X/Y

  ; DMA sprite data
  stz $2102
  stz $2103           ; Set OAM address to 0

  ldy #$0400          ; Writes #$00 to $4300, #$04 to $4301
  sty $4300           ; CPU -> PPU, auto inc, $2104 (OAM write)
  stz $4302
  stz $4303
  lda #$7E
  sta $4304           ; CPU address 7E:0000 - Work RAM
  ldy #$0220
  sty $4305           ; #$220 bytes to transfer
  lda #$01
  sta $420B

  lda #%10100000      ; 32x32 and 64x64 size sprites (we are using a 32x32)
  sta $2101

  lda #%00010000      ; Enable Sprites
  sta $212C

  lda #$0F
  sta $2100           ; Turn on screen, full brightness

  rts

SpriteInit:
  php

  rep #$30        ; 16bit A/X/Y

  ldx #$0000
  lda #$01
_offscreen:
  sta $0000, X
  inx
  inx
  inx
  inx
  cpx #$0200
  bne _offscreen

  lda #$5555
_xmsb:
  sta $0000, X
  inx
  inx
  cpx #$0220
  bne _xmsb

  plp
  rts


.ends

;============================================================================
; Character Data
;============================================================================
.bank 1 slot 0
.org 0
.section "CharacterData"

  .include "tiles.inc"

SpritePalette:
  .incbin "graphics/biker.clr"

Sprite:
  .incbin "graphics/biker.pic"

.ends
