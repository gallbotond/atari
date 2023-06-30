  PROCESSOR 6502

  INCLUDE "vcs.h"
  INCLUDE "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start an unitialized segment at $80 for var declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  SEG.U Variables
  ORG $80
P0Height ds 1             ; defines 1 byte for player 0 height
P1Height ds 1             ; defines 1 byte for player 1 height

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start ROM code segment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  SEG
  ORG $f000

Reset:
  CLEAN_START

  LDX #$80                ; blue bg color
  STX COLUBK

  LDA #%1111              ; yellow playfield color
  STA COLUPF

  LDA #10
  STA P0Height
  STA P1Height

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the TIA registers for the colors of P0 and P1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDA #$48
  STA COLUP0            ; player 0 color light red

  LDA #$c6              ; player 1 color light green
  STA COLUP1

  LDY #%00000010  ; CTRLPF D1 set to 1 means (score)
  STY CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
  LDA #2          
  STA VBLANK              ; turn on the VBLANK
  STA VSYNC               ; turn on VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate 3 lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  REPEAT 3
    STA WSYNC             ; three scanlines for VSYNC 
  REPEND

  LDA #0
  STA VSYNC               ; turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  REPEAT 23
    STA WSYNC             ; hit WSYNC and wait for the next scanline
  REPEND

  LDA #0
  STA VBLANK              ; turn off VBLANK 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VisibleScanlines:
  ; Draw 10 empty scanlines at the top of the frame
  REPEAT 10
    STA WSYNC
  REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for scoreboard numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDY #0
ScoreboardLoop:
  LDA NumberBitMap,Y
  STA PF1
  STA WSYNC
  INY
  CPY P0Height
  BNE ScoreboardLoop

  LDA #0
  STA PF1 ; disable playfield

  ; Draw 50 more scanlines between the scoreboard and the player
  REPEAT 50
    STA WSYNC
  REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for the player 0 graphics
;; Pulls data from an array of bytes defined at PlayerBitmap.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDY #0
Player0Loop:
  LDA PlayerBitmap,Y
  STA GRP0
  STA WSYNC
  INY
  CPY P1Height
  BNE Player0Loop

  LDA #0
  STA GRP0 ; disable player 0 graphics

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for the player 1 graphics
;; Pulls data from an array of bytes defined at PlayerBitmap.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDY #0
Player1Loop:
  LDA PlayerBitmap,Y
  STA GRP1
  STA WSYNC
  INY
  CPY #10
  BNE Player1Loop

  LDA #0
  STA GRP1 ; disable player 1 graphics

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the remaining 102 scanlines to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  REPEAT 102
    STA WSYNC
  REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK overscan lines to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  REPEAT 35
    STA WSYNC
  REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  JMP StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defines an array of bytes to draw the scoreboard number.
;; We add these bytes in the last ROM addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ORG $ffe8
PlayerBitmap:
  .byte #%01111110 ;  ######
  .byte #%11111111 ; ########
  .byte #%10011001 ; #  ##  #
  .byte #%11111111 ; ########
  .byte #%11111111 ; ########
  .byte #%11111111 ; ########
  .byte #%10111101 ; # #### #
  .byte #%11000011 ; ##    ##
  .byte #%11111111 ; ########
  .byte #%01111110 ;  ######

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defines an array of bytes to draw the scoreboard number.
;; We add these bytes in the last ROM addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ORG $fff2
NumberBitMap:
  .byte #%00001110
  .byte #%00001110
  .byte #%00000010
  .byte #%00000010
  .byte #%00001110
  .byte #%00001110
  .byte #%00001000
  .byte #%00001000
  .byte #%00001110
  .byte #%00001110
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete the ROM size to 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ORG $FFFC
  .word Reset
  .word Reset