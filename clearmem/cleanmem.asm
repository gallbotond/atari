  processor 6502

  seg code
    org $F000       ; define the code origin at $f000

Start: 
  sei               ; disable interrupts
  cld               ; disable the BCD decimal math mode
  ldx #$FF          ; loads the X register with #$FF
  txs               ; transfer X registers to S(tack) register
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clear the Zero Page region ($00 to $ff)
; Meaning the entire TIA register space and also RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  lda #0            ; A = 0
  ldx #$7F          ; X = #$ff 
  sta $FF

Memloop:
  dex               ; x--
  sta $80,X         ; store 0 at address $0 + X
  bne Memloop       ; loop until X == 0 (z-flag set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File ROM size to exactly 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $FFFC
  .word Start       ; reset vector at $fffc (where program starts)
  .word Start       ; interrupt vector at $fffe (unused in vcs)