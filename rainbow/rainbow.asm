  PROCESSOR 6502

  INCLUDE "vcs.h"
  INCLUDE "macro.h"

  SEG code
  ORG $f000

start:
CLEAN_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
nextframe:
  LDA #2          ; same as binary value %00000010
  STA VBLANK      ; turn on the VBLANK
  sta VSYNC       ; turn on VSYNK

;; Generate three lines of VSYNK
  STA WSYNC       ; first scanline
  STA WSYNC       ; second scanline
  STA WSYNC       ; third scanline

  LDA #0
  STA VSYNC       ; turn off VSYNK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDX #37         ; x = 37 (to count 37 scanlines)
loopVBlank: 
  STA WSYNC       ; hit WSYNC and wait for the next scanline
  DEX             ; decrement x
  BNE loopVBlank  ; loop while x != 0

  LDA #0
  STA VBLANK      ; turn off VBLANK 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDX #192        ; counter for 192 scanlines
loopVisible:
  STX COLUBK      ; set the background color
  STA WSYNC       ; wait for next scanline
  DEX             ; x--
  bne loopVisible ; loop while x != 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDA #2          ; hit and turn on VBLANK again
  STA VBLANK

  LDX #30
loopOverscan:
  STA WSYNC       ; put scanline
  DEX             ; decrement
  BNE loopOverscan; loop while x != 0

  JMP nextframe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete the ROM size to 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $FFFC
  .word start
  .word start
