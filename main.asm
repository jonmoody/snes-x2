.include "Header.inc"
.include "Snes_Init.asm"

.include "LoadGraphics.asm"

.bank 0 slot 0
.org 0
.section "MainCode"

Start:
  InitSNES

  LoadPalette BG_Palette, 0, 16

  LoadBlockToVRAM Tiles, $0000, $0080	; 4 tiles, 4bpp, = 128 bytes

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
  lda #$01
  sta $2105           ; Set Video mode 0, 8x8 tiles, 4 color BG1/BG2/BG3/BG4

  lda #$04            ; Set BG1's Tile Map offset to $0400 (Word address)
  sta $2107           ; And the Tile Map size to 32x32

  stz $210B           ; Set BG1's Character VRAM offset to $0000 (word address)

  lda #$01            ; Enable BG1
  sta $212C

  lda #$FF
  sta $210E
  sta $210E

  lda #$0F
  sta $2100           ; Turn on screen, full Brightness

  rts

.ends

;============================================================================
; Character Data
;============================================================================
.bank 1 slot 0
.org 0
.section "CharacterData"

  .include "tiles.inc"

.ends
