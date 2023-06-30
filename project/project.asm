    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos         byte         ; player0 x-position
JetYPos         byte         ; player0 y-position
BomberXPos      byte         ; player1 x-position
BomberYPos      byte         ; player1 y-position
Score           BYTE        ; 2 digit score as BCD
Timer           BYTE        ; 2 digit timer stored as BCD
OnesDigitOffset WORD        ; lookup table offset fo the score 1`s digit
TensDigitOffset WORD        ; lookup table offset fo the score 10`s digit
Temp    BYTE    ; store temporary score values here
JetSpritePtr    word         ; pointer to player0 sprite lookup table
JetColorPtr     word         ; pointer to player0 color lookup table
BomberSpritePtr word         ; pointer to player1 sprite lookup table
BomberColorPtr  word         ; pointer to player1 color lookup table
JetAnimOffset   byte         ; player0 frame offset for sprite animation
Random          byte         ; used to generate random bomber x-position
ScoreSprite     BYTE        ; store the sprite bit pattern for the score
TimerSprite     BYTE        ; store the sprite bit pattern for the timer
TerrainColor    BYTE        ; the color of the terrain
RiverColor      BYTE        ; river color
MissileXPos     BYTE        ; missisle x pos
MissileYPos     BYTE        ; missisle y pos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9               ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9            ; player1 sprite height (# rows in lookup table)
DIGITS_HEIGHT = 5           ; digit sprite height

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START              ; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #68
    sta JetXPos              ; JetXPos = 68
    lda #10
    sta JetYPos              ; JetYPos = 10
    lda #62
    sta BomberXPos           ; BomberXPos = 62
    lda #83
    sta BomberYPos           ; BomberYPos = 83
    lda #%11010100
    sta Random               ; Random = $D4
    LDA #0
    STA Score
    STA Timer

    LDA #$84
    STA RiverColor
    LDA #$c2
    STA TerrainColor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a macro to check if we should display missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        LDA #%00000000
        CPX MissileYPos     ; compare X current scanline with missile Y pos
        BNE .SkipMissileDraw    ; if x != missile y pos, then skip draw
.DrawMissile
        LDA #%00000010      ; else: enable missile 0 display
        INC MissileYPos     ; missile forward
.SkipMissileDraw
        STA ENAM0       ; store correct value in the TIA missile register
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table adresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>JetSprite
    sta JetSpritePtr+1       ; hi-byte pointer for jet sprite lookup table

    lda #<JetColor
    sta JetColorPtr          ; lo-byte pointer for jet color lookup table
    lda #>JetColor
    sta JetColorPtr+1        ; hi-byte pointer for jet color lookup table

    lda #<BomberSprite
    sta BomberSpritePtr      ; lo-byte pointer for bomber sprite lookup table
    lda #>BomberSprite
    sta BomberSpritePtr+1    ; hi-byte pointer for bomber sprite lookup table

    lda #<BomberColor
    sta BomberColorPtr       ; lo-byte pointer for bomber color lookup table
    lda #>BomberColor
    sta BomberColorPtr+1     ; hi-byte pointer for bomber color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; turn on VBLANK
    sta VSYNC                ; turn on VSYNC
    REPEAT 3
        sta WSYNC            ; display 3 recommended lines of VSYNC
    REPEND
    lda #0
    sta VSYNC                ; turn off VSYNC
    REPEAT 33
        sta WSYNC            ; display the 37 recommended lines of VBLANK
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos        ; set player0 horizontal position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos        ; set player1 horizontal position

    LDA MissileXPos
    LDY #2
    JSR SetObjectXPos       ; set missile horizontal position

    JSR CalculateDigitOffset    ; calculate the digit offset

    JSR GenerateJetSound        ; configure and enable jet engine audio

    sta WSYNC
    sta HMOVE                ; apply the horizontal offsets previously set

    LDA #0
    sta VBLANK               ; turn off VBLANK
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #0      ; clear TIA registers before each new frame 
    STA PF0
    STA PF1
    STA PF2
    STA GRP0
    STA GRP1        ; reset registers
    STA CTRLPF
    STA COLUBK

    LDA #$1e
    STA COLUPF      ; set the scoreboard color yellow
    
    LDX #DIGITS_HEIGHT      ; start X counter with 5 (height of digits)

.ScoreDigitLoop:
    LDY TensDigitOffset     ; get the tens digit offset fot the Score
    LDA Digits,Y        ; load the bit pattern from lookup table
    AND #$f0        ; mask/remove the graphics for the ones digit
    STA ScoreSprite ; save the score tens digit pattern in a variable

    LDY OnesDigitOffset     ; load the ones digit offset for the Score
    LDA Digits,Y        ; load the bit pattern from the lookup table
    AND #$0f        ; mask/remove the tens digits

    ORA ScoreSprite     ; join the ones and tens
    STA ScoreSprite     ; write it to memory

    STA WSYNC       ; wait for the end of the scanline
    STA PF1     ; update the playfield to display the Score sprite

    LDY TensDigitOffset+1       ; get the left digit offset for the Timer
    LDA Digits,Y        ; load digit pattern from the lookup table
    AND #$f0        ; mask/remove the graphics for the ones digits
    STA TimerSprite     ; save the timer tens digit pattern in a variable

    LDY OnesDigitOffset+1       ; get the ones digit offset for the timer
    LDA Digits,Y           ; load digit pattern from the lookup table
    AND #$0f        ; mask/remove the graphics for the tens digits
    ORA TimerSprite     ; join ones and tens
    STA TimerSprite     ; store the joined timer byte

    JSR Sleep12Cycles       ; sleep 12 cycles

    STA PF1         ; update playfield for Timer display

    LDY ScoreSprite     ; preload for the next scanline
    STA WSYNC       ; wait for next scanline

    STY PF1     ; update playfield for the score display
    INC TensDigitOffset
    INC TensDigitOffset+1
    INC OnesDigitOffset
    INC OnesDigitOffset+1       ; increment all digits for the next line of data

    JSR Sleep12Cycles       ; waste some cycles

    DEX         ; X--

    STA PF1     ; upadate the playfield to display the timer
    BNE .ScoreDigitLoop     ; if dex != 0, then branch to ScoreDigitLoop

    STA WSYNC       ; one more scanline just in case

    LDA #0
    STA PF0
    STA PF1
    STA PF2
    STA WSYNC
    STA WSYNC
    STA WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 192 visible scanlines of our game (96 lines because 2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda RiverColor
    sta COLUBK               ; set background/river color to blue
    lda TerrainColor
    sta COLUPF               ; set playfield/grass color to green
    lda #%00000001
    sta CTRLPF               ; enable playfield reflection
    lda #$F0
    sta PF0                  ; setting PF0 bit pattern
    lda #$FC
    sta PF1                  ; setting PF1 bit pattern
    lda #0
    sta PF2                  ; setting PF2 bit pattern

    ldx #85                  ; X counts the number of remaining scanlines
.GameLineLoop:
    DRAW_MISSILE        ; macro to check if we should draw the missile

.AreWeInsideJetSprite:
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set before subtraction
    sbc JetYPos              ; subtract sprite Y-coordinate
    cmp #JET_HEIGHT           ; are we inside the sprite height bounds?
    bcc .DrawSpriteP0        ; if result < SpriteHeight, call the draw routine
    lda #0                   ; else, set lookup index to zero
.DrawSpriteP0:
    clc                      ; clear carry flag before addition
    adc JetAnimOffset        ; jump to correct sprite frame address in memory
    tay                      ; load Y so we can work with the pointer
    lda (JetSpritePtr),Y     ; load player0 bitmap data from lookup table
    sta WSYNC                ; wait for scanline
    sta GRP0                 ; set graphics for player0
    lda (JetColorPtr),Y      ; load player color from lookup table
    sta COLUP0               ; set color of player 0

.AreWeInsideBomberSprite:
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set before subtraction
    sbc BomberYPos           ; subtract sprite Y-coordinate
    cmp #BOMBER_HEIGHT        ; are we inside the sprite height bounds?
    bcc .DrawSpriteP1        ; if result < SpriteHeight, call the draw routine
    lda #0                   ; else, set lookup index to zero
.DrawSpriteP1:
    tay                      ; load Y so we can work with the pointer

    lda #%00000101
    sta NUSIZ1               ; stretch player 1 sprite

    lda (BomberSpritePtr),Y  ; load player1 bitmap data from lookup table
    sta WSYNC                ; wait for scanline
    sta GRP1                 ; set graphics for player1
    lda (BomberColorPtr),Y   ; load player color from lookup table
    sta COLUP1               ; set color of player 1

    dex                      ; X--
    bne .GameLineLoop        ; repeat next main game scanline until finished

    lda #0
    sta JetAnimOffset        ; reset jet animation frame to zero each frame

    sta WSYNC                ; wait for a scanline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; turn on VBLANK again
    REPEAT 30
        sta WSYNC            ; display 30 recommended lines of VBlank Overscan
    REPEND
    lda #0
    sta VBLANK               ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000           ; player0 joystick up
    bit SWCHA       ; compare with register for joysticks
    bne CheckP0Down          ; if bit pattern doesnt match, bypass Up block
    LDA JetYPos
    CMP #77                  ; compare jet y pos with upper bounds
    BPL CheckP0Down     ; if plus skip to 
    inc JetYPos     ; else increase jey y pos
    lda #0
    sta JetAnimOffset        ; reset sprite animation to first frame

CheckP0Down:
    lda #%00100000           ; player0 joystick down
    bit SWCHA
    bne CheckP0Left          ; if bit pattern doesnt match, bypass Down block
    LDA JetYPos
    CMP #1      ; if JetYPos is less than 1
    BMI CheckP0Left     ; skip to next check    
    dec JetYPos     ; else decrement
    lda #0
    sta JetAnimOffset        ; reset sprite animation to first frame

CheckP0Left:
    lda #%01000000           ; player0 joystick left
    bit SWCHA
    bne CheckP0Right         ; if bit pattern doesnt match, bypass Left block
    LDA JetXPos         ; ocmpare the jet x pos with the 
    CMP #31         ; left bounds
    BMI CheckP0Right      ; if minus, skip to 
    dec JetXPos         ; else decrement x pos
    lda #JET_HEIGHT           ; 9 compare in constant mode
    sta JetAnimOffset        ; set animation offset to the second frame

CheckP0Right:
    lda #%10000000           ; player0 joystick right
    bit SWCHA
    bne CheckButtonPressed        ; if bit pattern doesnt match, bypass Right block
    LDA JetXPos     ; compare the jet x position with the 
    CMP #103        ; right boundary
    BPL CheckButtonPressed  ; if plus skip to 
    inc JetXPos         ; else increment
    lda #JET_HEIGHT           ; 9 compare in constant mode
    sta JetAnimOffset        ; set animation offset to the second frame

CheckButtonPressed:
    LDA #%10000000      ; if button is pressed
    BIT INPT4
    BNE EndInputCheck
.ButtonPressed
    LDA JetXPos
    CLC
    ADC #5
    STA MissileXPos     ; set the missile x position to the player
    LDA JetYPos
    CLC
    ADC #6
    STA MissileYPos     ; set the missile y position to the player 

EndInputCheck:               ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0                   ; compare bomber y-position with 0
    bmi .ResetBomberPosition ; if it is < 0, then reset y-position to the top
    dec BomberYPos           ; else, decrement enemy y-position for next frame
    jmp EndPositionUpdate
.ResetBomberPosition
    jsr GetRandomBomberPos   ; call subroutine for random bomber position

.SetScoreValues:
    sed                      ; set BCD mode for score and timer values
    lda Timer
    clc
    adc #1
    sta Timer                ; add 1 to the Timer (BCD does not like INC)
    cld                      ; disable BCD after updating Score and Timer

EndPositionUpdate:           ; fallback for the position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    LDA #%10000000      ; CXPPMM bit 7 detects P0 P1 collision
    BIT CXPPMM      ; check CXPPMM bit 7 with the above pattern
    BNE .CollisionP0P1      ; if collision between P0 and P1 happened
    JSR SetTerrainRiverColor    ; else, set PF color to green-blue
    JMP CheckCollisionM0P1   ; else skip to next check
.CollisionP0P1
    JSR GameOver        ; call GameOver subroutine 

CheckCollisionM0P1
     LDA #%10000000     ; CXM0P bit 7 detects missile and P0 collision
     BIT CXM0P      ; check CXM0P bit 7 with the above pattern
     BNE .M0P1Collided      ; collision missile 0 and player 1 happened
     JMP EndCollisionCheck
.M0P1Collided
    SED         ; set decimal mode 
    LDA Score
    CLC     ; clear carry
    ADC #1      ; add 1
    STA Score       ; save score
    CLD     ; disable decimal mode
    LDA #0
    STA MissileYPos

EndCollisionCheck:      ; fallback
    STA CXCLR       ; clear all collison flags before the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; INC Timer       ; increase the timer
    jmp StartFrame           ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate jet sound based on JetYPos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateJetSound SUBROUTINE
    LDA #3
    STA AUDV0       ; volume

    ; divide the y pos by 8
    LDA JetYPos     ; loads jet y pos int accumulator
    LSR 
    LSR 
    LSR     ; divide 3 times by 2 = divide by 8
    STA Temp        ; store in temp
    LDA #31     
    SEC         
    SBC Temp        ; 31 - (Y/8)
    STA AUDF0       ; set the new audio frequency register

    LDA #8
    STA AUDC0       ; tone

    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the color of terrain and river wit green and blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetTerrainRiverColor SUBROUTINE
    LDA #$c2
    STA TerrainColor        ; terrain to green
    LDA #$84
    STA RiverColor      ; river to blue
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC                ; start a fresh new scanline
    sec                      ; make sure carry-flag is set before subtracion
.Div15Loop
    sbc #15                  ; subtract 15 from accumulator
    bcs .Div15Loop           ; loop until carry-flag is clear
    eor #7                   ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                      ; four shift lefts to get only the top 4 bits
    sta HMP0,Y               ; store the fine offset to the correct HMxx
    sta RESP0,Y              ; fix object position in 15-step increment
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver SUBROUTINE
    LDA #$30
    STA TerrainColor        ; terrain color to red
    STA RiverColor      ; river color to red
    LDA #0
    STA Score
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback Shift Register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random number for the X-position of the bomber.
;; Divide the random value by 4 to limit the size of the result to match river.
;; Add 30 to compensate for the left green playfield
;; The routine also sets the Y-position of the bomber to the top of the screen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random               ; performs a series of shifts and bit operations

    lsr
    lsr                      ; divide the value by 4 with 2 right shifts
    sta BomberXPos           ; save it to the variable BomberXPos
    lda #30
    adc BomberXPos           ; adds 30 + BomberXPos to compensate for left PF
    sta BomberXPos           ; and sets the new value to the bomber x-position

    lda #96
    sta BomberYPos           ; set the y-position to the top of the screen

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scoreboard is stored using BCD, so the display shows hex numbers.
;; This converts the high and low nibbles of the variable Score and Timer
;; into the offsets of digits lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes in the lookup table.
;;
;; For the low nibble we need to multiply by 5
;;   - we can use left shifts to perform multiplication by 2
;;   - for any number N, the value of N*5 = (N*2*2)+N
;;
;; For the upper nibble, since its already times 16, we need to divide it
;; and then multiply by 5:
;;   - we can use right shifts to perform division by 2
;;   - for any number N, the value of (N/16)*5 is equal to (N/4)+(N/16)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset SUBROUTINE
    LDX #1      ; X register is the loop counter
.PrepareScoreLoop       ; this will loop twice, first X=1, and then X=0
    LDA Score,X     ; load S with Timer (X=1) or Score (X=0)
    AND #$0f        ; mask out the tens digit mask
    STA Temp        ; save the value of A into Temp
    ASL
    ASL             ; two shift left multiplies by 4
    ADC Temp        ; add the value saved in Temp, now it's multiplied by 5
    STA OnesDigitOffset,X ; save A in OnesDigitOffset+1, or OnesDigitOffset

    LDA Score,X     ; load A with Timer(X=1) or Score(X=0)
    AND #$f0        ; remove the ones digit by masking 4 bits 11110000
    LSR
    LSR         ; N / 4
    STA Temp        ; save the value of A into Temp
    LSR
    LSR         ; N / 16
    ADC Temp        ; add the vale saved in Temp (N/16 + N/4)
    STA TensDigitOffset,X       ; store A in TensDigitOffset+1, or TensDigitOffset

    DEX         ; X--
    BPL .PrepareScoreLoop        ; while X >= 0, loop to pass a second time

    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to sleep 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles SUBROUTINE
    RTS     ; JSR and RTS is 12 cycles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

JetSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                ; move to position $FFFC
    word Reset               ; write 2 bytes with the program reset address
    word Reset               ; write 2 bytes with the interruption vector
