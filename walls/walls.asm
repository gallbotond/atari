  PROCESSOR 6502

  INCLUDE "vcs.h"
  INCLUDE "macro.h"

  SEG
  ORG $f000

Reset:
CLEAN_START

  LDX #$80                ; blue bg color
  STX COLUBK

  LDA #$1c                ; yellow playfield color
  STA COLUPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
  LDA #2          
  STA VBLANK              ; turn on the VBLANK
  STA VSYNC               ; turn on VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate three lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  REPEAT 3
    STA WSYNC             ; three scanlines for VSYNC 
  REPEND

  LDA #0
  STA VSYNC               ; turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  REPEAT 37
    STA WSYNC             ; hit WSYNC and wait for the next scanline
  REPEND

  LDA #0
  STA VBLANK              ; turn off VBLANK 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the CTRLPF register to allow playfield reflection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDX #%00000001  ; CTRPLF register (D0 means reflect the PF)
  STX CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; Skip 7 scanlines with no PF set
  LDX #0
  STX PF0
  STX PF1
  STX PF2
  REPEAT 7
    STA WSYNC
  REPEND

  ; Set the PF0 to 1110 (LSB first) and PF1-PF2 as 1111 1111
  LDX #%11100000
  STX PF0
  LDX #%11111111
  STX PF1
  STX PF2
  REPEAT 7
    STA WSYNC
  REPEND

  ; Set the next 164 lines only with PF0 third bit enabled
  LDX #%01100000
  STX PF0
  LDX #0
  STX PF1
  LDX #%10000000
  STX PF2
  REPEAT 164
    STA WSYNC
  REPEND

  ; Set the PF0 to 1110 (LSB first) and PF1-PF2 as 1111 1111
  LDX #%11100000
  STX PF0
  LDX #%11111111
  STX PF1
  STX PF2
  REPEAT 7
    STA WSYNC
  REPEND

  ; Skip 7 vertical lines with no PF set
  LDX #0
  STX PF0
  STX PF1
  STX PF2
  REPEAT 7
    STA WSYNC
  REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK overscan lines to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDA #2
  STA VBLANK
  REPEAT 30
    STA WSYNC
  REPEND
  LDA #0
  STA VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  JMP StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete the ROM size to 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ORG $FFFC
  .word Reset
  .word Reset