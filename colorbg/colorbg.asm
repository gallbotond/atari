  PROCESSOR 6502

  INCLUDE "vcs.h"
  INCLUDE "macro.h"

  SEG code
  org $f000         ; defines the origin of the ROM at $f000

start: 
CLEAN_START          ; macro to safely clear the memory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the background luminosity color to yellow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDA #$1e          ; load color into A ($1e is NTSC for yellow)
  STA COLUBK        ; store A to BackgroundColor Address $09

  JMP start         ; repeat from start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill ROM size to exactly 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ORG $fffc         ; defines the origin to $fffc
  .word start       ; reset vector at $fffc (where program starts)
  .word start       ; interrupt vector at $fffe (unused in the vcs) 