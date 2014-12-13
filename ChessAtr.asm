;
; A small chess project for Atari 800
; done on MADS compiler (http://mads.atari8.info)
;
; User can move pieces in cursor mode (press "C" key).
; This program replays a list of chess games by just drawing board.
; Chess logic is ok.
; TODO: Comp.chess player AI does not exist at all.
;
;...................... Memory 1 ................................
LOAD_ADDR       equ $1000   ; where to load and start module
MovesGen_ADDR   equ $5000   ; where to generate moves (~2Kb)
MovesHst_ADDR   equ $6000   ; where is history of moves (~2Kb)

ChessGames_ADDR equ $A000   ; where to load pgn of chess games
                
    org  LOAD_ADDR
                                                        
VCOUNT  equ $d40b ; keeps track of what scan line is currently being drawn
DLISTL  equ $d402 ; holds the address of the display list


IRQEN   equ $d20e ; is used to enable or disable IRQ interrupts (I/O)
NMIEN   equ $d40e ; is used to enable or disable Non-Maskable Interrupts (we use display list)
DMACTL  equ $d400 ; controls Direct Memory Access
KEYBCH  equ $02FC ; to catch keypress

; Program
    
main  
    
    sei                 ; prevent interrupts
; init
    lda #0              ; to disable
    ;sta IRQEN          ; keyboard also
    sta NMIEN           ; display list, also reset,vertical blank
    sta DMACTL          ; will be direct memory drawings
    cmp:rne VCOUNT      ; Loop till drawing is done
       
    mwa #dlist DLISTL   ; set the new DLIST
    mva #$22 DMACTL
    
    cli                 ; clear, enable interrupts
    
    jmp StartProg
    
; Global datas, variables

StartingBoard
    .by "RNBQKBNR"
    .by "PPPPPPPP"
    .by "        "
    .by "        "
    .by "        "
    .by "        "
    .by "pppppppp"
    .by "rnbqkbnr"

Board .ds 64
ToMove  .by 0   ; 0-white, 1-black
EPsq    .by 0   ; en-passant square 16..23 / 40..47
Castle  .by 0,0,0,0 ; castling flags (KQkq), 0-can do castling, 1-can not
Piece2promote .by 0 ; piece to promote next move, by default is queen (Q,R,B,N)
MVhistLastAddr .wo 0    ; global address of last move
MoveNumber .by 1

PClist
    .by " KQRBNPkqrbnp", 0  ; pieces
    
ChrDispAddr .wo 0  ; global address for printing characters

Sq_drag .by 0   ; drag-square (to move from) $ff=none

Cr_sq   .by 0   ; cursor square 0..63
Cr_V    .by 0   ; Vertical 0..7 (1-8)
Cr_H    .by 0   ; Horizontal 0..7 (A-H)

;----------------------------------------
;  The main program starts here
;----------------------------------------

StartProg    
 
    lda #1          ; display about title
    ShowTitle
    ClearScreen
    
    lda #2          ; display title 2 (Anand-Carlsen)
    ShowTitle
     
usrSelLoop

    SelectChessGame     ; User choice
    
    lda GameEnter
    jeq ExitProgLoop    ; if nothing selected then exit
    
    NewGame         ; Chess Board and replay

keyboardLoop

    mva #$ff KEYBCH
loopKbKeypress
    lda KEYBCH
    cmp #$ff
    jeq loopKbKeypress

    KeybControlMode ; board keyboard control
    
    lda KEYBCH
    cmp #$1c        ; is Esc key in buffer?
    jne keyboardLoop    ; otherwise loop
    jmp usrSelLoop
ExitProgLoop
    ClearScreen
    
    lda #3          ; display title 3 (Game Over)
    ShowTitle
    
    jmp *           ; just no exit, stay there

Str_blank       .by "                 ",0
Str_check       .by "Check+           ",0
Str_mate10      .by "Checkmate# 1-0   ",0
Str_mate01      .by "Checkmate# 0-1   ",0
Str_stalemate   .by "Stalemate 1/2-1/2",0

Str_cursorOn    .by "CURSOR MODE      ", 0
Str_undo        .by "Undo             ", 0

Str_UserSele    .by "Select a game    ", 0

pre1Key  .by 0       ; contains key pressed previously
;-------------------------------------------------------
; PROCEDURE  KeybControlMode - this mode allows control moves by keyboard
;-------------------------------------------------------
.PROC KeybControlMode

    lda KEYBCH
    ;dispCode
    
; verify keyboard keys
    cmp #$26     ;key "/" repeats prev.key
    jne kbC0

    lda pre1Key  ; use prev.key
kbC0
    sta pre1Key  ; save prev.key to repeat 
    
    cmp #$86     ;Left
    jne kbC1

    cpb Cr_sq #$ff
    jeq kbSpace
    
; cursor moves left   
    lda Cr_H
    jeq kbSkpLf
    
    ClrCursor
    dec Cr_sq   ; cursor horiz-1
    updCursor
    RdrwCursor
kbSkpLf
    jmp kbOk

kbC1    
    cmp #$87     ;Right
    jne kbC2
    
    cpb Cr_sq #$ff
    jeq kbSpace
    
; cursor moves right   
    cpb Cr_H #7
    jeq kbSkpRt
    
    ClrCursor
    inc Cr_sq   ; cursor horiz+1
    updCursor
    RdrwCursor
kbSkpRt
    jmp kbOk

kbC2        
    cmp #$8e     ;Up
    jne kbC3
    
    cpb Cr_sq #$ff
    jeq kbSpace
    
; cursor moves up
    cpb Cr_V #7
    jeq kbSkpUp
    
    ClrCursor
    adb Cr_sq #8    ; cursor vert+1
    updCursor
    RdrwCursor
kbSkpUp
    jmp kbOk

kbC3        

    cmp #$8f     ;Down
    jne kbC4

    cpb Cr_sq #$ff
    jeq kbSpace
    
; cursor moves down   
    lda Cr_V
    jeq kbSkpDn
    
    ClrCursor
    sbb Cr_sq #8    ; cursor vert-1
    updCursor
    RdrwCursor
kbSkpDn
    jmp kbOk

kbC4
kbSpace
    cmp #$21     ;Space key
    jne kbC5
    jmp kbClickedEnter
kbC5        
    cmp #$0c     ;Enter key
    jne kbC6
kbClickedEnter
    ClickedOnSquare
    jmp kbOk
kbC6        
    cmp #$23     ;n
    jne kbC7
    jmp kbClicked_N
kbC7
    cmp #$63     ;N
    jne kbC8
kbClicked_N
    ClearScreen
    NewGame     ; start a new game
    CursorON    ; activate cursor mode
    jmp kbOk
kbC8
    cmp #$0b     ;u
    jne kbC9
    jmp kbClicked_U
kbC9
    cmp #$4b     ;U
    jne kbCA
kbClicked_U
    UnDoPressed ; try undo move
    jmp kbOk
kbCA
    cmp #$12     ;c
    jne kbCB
    jmp kbClicked_C
kbCB
    cmp #$52     ;C
    jne kbCC
kbClicked_C
    CursorON
    jmp kbOk
kbCC

    cmp #$1c     ;Esc key
kbOk         
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  updCursor - this updates cursor pos.X,Y
;-------------------------------------------------------
.PROC updCursor
    lda Cr_sq   ; Updates H,V
    pha
    and #7
    sta Cr_H    ; A-H for to-square [0..7]
    pla
    alr #$ff
    alr #$ff
    alr #$ff
    sta Cr_V    ; 1-8 for to-square [0..7]
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  CursorON - this activates cursor
;-------------------------------------------------------
.PROC CursorON
    
    cpb Cr_sq #$ff      ; activates cursor, if inactive
    jeq swcrsFF
    jne swcrsOk
swcrsFF
    mwa #Str_cursorOn $C2
    dispMessage         ; display cursor on text

    mva #12 Cr_sq       ; square e2
    updCursor
    RdrwCursor          ; redraw square
swcrsOk
    rts
.ENDP


;-------------------------------------------------------
; PROCEDURE  PrintSq   - redraws square
; Regs: x - position x on screen 0-7 (from left to right)
;       y - position y on screen 0-7 (from bottom to top)
;-------------------------------------------------------
.PROC PrintSq

    .var pH  .byte ; pos horiz.
    .var pC  .byte ; calc.square
    
    stx pH
    sty pC
    
    aso pC
    aso pC
    aso pC
    adb pC pH

    tax
    lda Board,x     ; Board[ square ]
    ldx pH
    PutPiece
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  RdrwCursor - redraws cursor square
;-------------------------------------------------------
.PROC RdrwCursor
    ldx Cr_H
    ldy Cr_V
    PrintSq
    rts
.ENDP
;-------------------------------------------------------
; PROCEDURE  ClrCursor - clears cursor square for redraw
;-------------------------------------------------------
.PROC ClrCursor
    lda Cr_sq
    pha
    lda #$ff    ; this clears cursor before redrawing
    sta Cr_sq   ; no cursor
    RdrwCursor
    pla
    sta Cr_sq   ; restore cursor
    rts
.ENDP
;-------------------------------------------------------
; PROCEDURE  PrintBoard   - display chess board
;-------------------------------------------------------
.PROC PrintBoard

    .var loop_I  .byte ; to loop 64 squares
    .var pV  .byte ; pos vertical
    .var pH  .byte ; pos horiz.

    mva #0 loop_I   ; for 0..63
    mva #0 pV       ; Y=0
    mva #0 pH       ; H=0

loop64sq
    ldx loop_I
    lda Board,x     ; Board[ loop_I ]
    ldx pH
    ldy pV
    PutPiece
    
    inc pH          ; H++
    cpb #8 pH
    jne skipNextH
    
    mva #0 pH       ; if(H>7) H=0,Y++
    inc pV

skipNextH

    adb loop_I #1
    cmp #64         ; all 64 squares?
    jne loop64sq
    
    rts
    
.ENDP


;-------------------------------------------------------
; PROCEDURE  PutPiece
; Regs: x - position x on screen 0-7 (from left to right)
;       y - position y on screen 0-7 (from bottom to top)
;       a - piece from " KQRBNPkqrbnp" that is
;            0-blank square
;            1-wK,2-wQ,3-wR,4-wB,5-wN,6-wP
;            7-bK,8-bQ,9-bR,10-bB,11-bN,12-bP
;-------------------------------------------------------
;
.PROC PutPiece

; parameters
    .var posX .byte ; position X
    .var posY .byte ; position Y
    .var piecN .byte ; piece by number

; variables
    .var posYi .byte ; "inverted" position Y (7-posY)
    .var sc_adrs .word ; current address of pixel
    .var pc_I    .byte ; piece+I counter in data
    .var loop_16 .byte ; to loop 16 lines
    .var loop_I  .byte ; to loop 4bytes per line
     
    .var p16X .byte ; to calc. x position
    .var p16Y .byte ; to calc. y position
    .var pc_6 .byte ; piece 0-5 (king-pawn)
    .var pc_cl .byte ; colour of piece 0=B/1=W
    .var sqEmp .byte ; empty square flag 1=is
    .var bgCl .byte ; colour of background squares 0/ff
    
    stx posX
    sty posY
    sta piecN
    
    mva #7 posYi
    sbb posYi posY          ; posYi = 7-posY
    
    ldx #0
loopPCofList
    cpb piecN PClist,x
    jeq thisPC
    inx
    jmp loopPCofList
thisPC
    stx piecN   ; set pieceN = piece of list
 
    mva #0 sqEmp
    lda piecN
    jne notBlank
    inc sqEmp    ; square is empty
    jmp skp01dec
notBlank        
    dec piecN     ; without empty square (-1)
skp01dec
    mva piecN pc_6 
    
    lda piecN
    cmp #6
    jcs pcIsBlack
   
    mva #0 pc_cl    ; white piece
    jmp pcIsOk
    
pcIsBlack
    sbb pc_6 #6     ; -6
    mva #1 pc_cl    ; black piece
pcIsOk

    mva posX bgCl
    inc bgCl
    adb bgCl posYi
    lda #1
    and bgCl        ; (X+Y) mod 2
    sta bgCl
    lda bgCl
    jeq stayDarkBg
    mva #$ff bgCl   ; light background
stayDarkBg

    mwa #ScrAddr sc_adrs ; screen 0,0 address
    
    lda posYi
    cmp #4
    jcc noSplitted      ; H=0..3

    mwa #ScrAdd2 sc_adrs ; splitted screen address ; H=4..7
    sbb posYi #4
    
noSplitted

    mva #0 p16X     ; p16X=0
loopXpos
    lda p16X
    cmp posX        ; p16X==posX?
    jeq thisXpt
    inc p16X        ; p16X++
    adw sc_adrs #4    ; skip next 16 pixels
    jmp loopXpos
thisXpt

    mva #0 p16Y     ; p16Y=0
loopYpos
    lda p16Y
    cmp posYi        ; p16Y==posYi?
    jeq thisYline
    inc p16Y        ; p16Y++
    adw sc_adrs #800    ; skip next 16 lines #640  (+4 blank)
    jmp loopYpos
thisYline

    mva #0 pc_I     ; pc_I=0    
    mva #0 loop_16  ; loop_16=0
    

loop16Lines
    mva #0 loop_I   ; loop_I=0
loopLine
    mwa sc_adrs $C0 ; $C0,$C1 = screen address
    
    mwa #fig24bit $C2 ; $C2,$C3 = address of piece datas
    ldx pc_6        ; obtain address of piece data

loopAddr
    cpx #0
    jeq addrGood
    dex             ; x-- till pc_6
    adw $C2 #256    ; 256 bytes per piece
    jmp loopAddr
addrGood

    ldx pc_cl
    cpx #0
    jeq addrPcol
    adw $C2 #64    ; other colour piece
addrPcol

    lda bgCl
    jeq addrBgCol
    adw $C2 #128    ; other background for piece
addrBgCol
  
    ldy pc_I
    sbw $C2 #8      ; skip first 2 lines 
    lda ($C2), y     ; read datas from address in A
    
    
    ldx loop_16
    cpx #2          ; first 2 lines
    jcc EmpSq
    cpx #18         ; last 2 lines
    jcs EmpSq    
    
    ldx #1
    cpx sqEmp       ; is square empty?
    jne notEmpSq
EmpSq
    lda bgCl        ; A= colour of backround   
notEmpSq

    ; modify byte if it is cursor (make it red)
    
    tay             ; y = A
    cpb Cr_sq #$ff  ; should cursor be displayed
    jeq notCSq
    cpb Cr_H posX
    jne notCSq
    cpb Cr_V posY
    jne notCSq
        
    ldx loop_16
    cpx #2          ; first two lines are cursor
    jcc thisCSq
    cpx #18         ; last two lines are cursor
    jcs thisCSq
    
    tya
    ldy bgCl
    ldx loop_I
    cpx #0
    jne thNot1stb
                    ; left vertical of cursor
    cpy #0          ; which method?
    jeq thC1ora
    eor #128        ; make first point red on light square background
    jmp drw4px
thC1ora    
    ora #64         ; make first point red on black background
    jmp drw4px

thNot1stb
    cpx #3
    jne thNot3stb
                    ; right vertical of cursor
    cpy #0          ; which method?
    jeq thC2ora
    eor #2          ; make last point red on light square background
    jmp drw4px
thC2ora    
    ora #1          ; make last point red on black background
    jmp drw4px    
thNot3stb 
    jmp drw4px
thisCSq
    lda #85         ; A = colour of cursor (4 red pixels)
    jmp drw4px
notCSq
    tya             ; A is as it should be, restore
drw4px

    ldy #0
    sta ($C0), y    ; put 4 pixels on screen
    adw sc_adrs #1   ; screen address++
    
    inc pc_I        ; pc_I++
    
    inc loop_I      ; loop_I++
    lda loop_I
    cmp #4
    jne loopLine    ; if loop_I<4
    
    adw sc_adrs #36    ; screen address+=line-4bytes
        
    inc loop_16     ; loop_16++
    lda loop_16
    cmp #20         ; 2+16+2 lines
    jne loop16Lines ; if loop_16<16
    
    rts   
 
.ENDP
    
;-------------------------------------------------------
; PROCEDURE  Preset   - set position for printing
; Regs: a - screen part 0,1,2
;       x - x position (*2 bytes per char)
;       y - y position (*12 lines per char)
;-------------------------------------------------------

.PROC Preset

; variables
    .var dspM .byte ; screen part: 0,1,2
    .var dspX .byte ; position X
    .var dspY .byte ; position Y
    
    stx dspX
    sty dspY
    sta dspM

    cmp #0
    jne notScr0
    mwa #ScrAddr ChrDispAddr    ;vert 5-8
    jmp posChXY
notScr0
    cmp #1
    jne notScr1
    mwa #ScrAdd2 ChrDispAddr    ;vert 1-4
    jmp posChXY
notScr1
    mwa #ScrAdd3 ChrDispAddr    ;under board
posChXY
    ldx dspY
lpYpos
    cpx #0        ; Y==0?
    jeq thYline
    dex          ; Y--
    adw ChrDispAddr #480    ; skip next 12 lines
    jmp lpYpos
thYline
    ldx dspX
lpXpos
    cpx #0        ; X==0?
    jeq thCline
    dex          ; X--
    adw ChrDispAddr #2    ; skip next 8 pixels
    jmp lpXpos
thCline    
    rts

.ENDP

;-------------------------------------------------------
; PROCEDURE  PrintStr   - display string (after Preset)
; Parameters: put string in Str2print buffer, end with 0
;-------------------------------------------------------

Str2print .ds 21    ; max.20 chars in line

.PROC PrintStr

; variables
    .var chI .byte ; pointer to char[]
    .var chC .byte   ; char to display on screen
    .var dspFnt .word   ;address in font table
    .var fntI .byte ; font pointer
    .var b2fl .byte ; 2 bytes per line
        
    mva #0 chI          ; chI = 0
prnLoopChars    
    ldx chI
    lda Str2print, x    ; Ch = Str2print[chI]
    jeq PrnStrOver      ; chr(0)? Exit if so.
    sta chC
    sbb chC #32          ; start from chr(32)
    mwa #font_8x12 dspFnt    ;font table
loopFnt    
    lda chC
    jeq fontFound
    dec chC
    adw dspFnt #24
    jmp loopFnt
fontFound
    mwa ChrDispAddr $C0 ; $C0,$C1 = screen address to print
    mva #0 b2fl
    mwa dspFnt $C2      ; $C2,$C3 = font address
    mva #0 fntI
loop12rw
    ldy fntI
    lda ($C2), y     ; read data from font table

    ldy #0
    sta ($C0), y     ; store data to screen
    
    adb b2fl #1
    cmp #2
    jeq lineCr
    adw $C0 #1      ; next byte of this char
    jmp noLnCr
lineCr
    adw $C0 #39     ; next row
    mva #0 b2fl
noLnCr
    adb fntI #1     ; next data from font table
    cmp #24         ; 12 rows?
    jne loop12rw
    
    adw ChrDispAddr #2  ; next char to be
    inc chI         ; chI++
    
    jmp prnLoopChars
PrnStrOver   
    rts
   
.ENDP
  
;-------------------------------------------------------
; PROCEDURE  PutLogo   - places Atari logo on screen
;-------------------------------------------------------

.PROC PutLogo

; variables
    .var lgB .byte ; logo lines printed
    .var pb2 .byte ; 2*4 bytes per line
            
    mwa #LogoAddr $C0 ; $C0,$C1 = screen address to print logo
    mwa #atari_logo $C2      ; $C2,$C3 = datas address
    
    mva #0 lgB 
loop4p
    mva #0 pb2 
loopLgRw
    ldy #0
    lda ($C2), y     ; read data from table
    ldy #0
    sta ($C0), y     ; store data to screen
    
    adw $C2 #1      ; next byte of data
    
    adb pb2 #1      ; pb2++
    cmp #8          ; 32px per line?
    jeq lineLg
    adw $C0 #1      ; next pixel
    jmp loopLgRw
lineLg
    adw $C0 #33     ; next row
    adb lgB #1      ; lgB++
    cmp #32         ; 8x4 rows ?
    jeq logoOvr
    jmp loop4p
logoOvr   
    rts
   
.ENDP

;-------------------------------------------------------
; PROCEDURE  PutToMove   - places "side to move" sign on screen
;-------------------------------------------------------

.PROC PutToMove
    
    mva #0  Str2print+1

    ldx ToMove
    cpx #0
    jne notWh2mv
    lda #128        ; white to move
    jmp dspWh2mv
notWh2mv
    lda #32         ; clear
dspWh2mv    
    sta Str2print      ; white to move sign
    lda #1
    ldy #6
    ldx #16
    Preset
    PrintStr 

    ldx ToMove
    cpx #1
    jne notBl2mv
    lda #129        ; black to move
    jmp dspBl2mv
notBl2mv
    lda #32         ; clear
dspBl2mv
    sta Str2print      ;  black to move sign
    lda #0
    ldy #0
    ldx #16
    Preset
    PrintStr
    rts

.ENDP

;...................... Memory 2 ................................
; Addresses for screen memory
; splitted screen (3 parts between 4 squares)
ScrAddr equ $7000 
ScrAdd2 equ ScrAddr + $1000
ScrAdd3 equ ScrAddr + $2000

LogoAddr equ ScrAdd2 + $20 + $280   ; + (4 x 8 board size) + ( 16 lines ) 

dlist
          
    :2 dta $70
    dta $4E,a(ScrAddr)      ; 4 colours 160 x 192 pixels (WxH), buffer of 8138 bytes of memory
    :79 dta $E              ; can read 4Kb only
    dta $4E, 0 , h(ScrAdd2) ; then read next 4Kb
    :79 dta $E
    dta $4E, 0 , h(ScrAdd3) ; then remaining Kbs 
    :40 dta $E
    dta $41,a(dlist)
; various long datas


fig24bit 
    ;icl "fig24bit.asm"    ; include datas of pieces
    ins "fig24bit.bin"    ; just binary version
    
font_8x12  
    ;icl "font_8x12.asm"    ; include font datas of letters
    ins "font_8x12.bin"   

atari_logo  
    ;icl "atari_logo.asm"    ; include logo datas
    ins "atari_logo.bin"  

;----------------------------------------
;
; TEXT DISPLAY
;
;----------------------------------------

;Screens (max 255) of titles
Titles
    
; Total display of text = 16 rows of 20 chars
;  7 rows per blocks0,1 and 2 rows in block2
Title0_blank
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
; little bit over next line
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
; little bit over next line
    .by "                    "
    .by "                    "
; ---- display of chars

Title1
    .by "                    "
    .by "                    "
    .by "   Chessforeva      "
    .by "      ATARI         "
    .by "  8-bit assembler   "
    .by "     project        "
    .by "                    "
    
    .by "                    "
    .by "    year 2014       "
    .by "                    "
    .by "  Replays PGN of    "
    .by "  chess games,      "
    .by "  easy to compile   "
    .by "                    "
    
    .by "                    "
    .by "                    "

Title2
    .by "                    "
    .by "                    "
    .by "   World Chess      "
    .by "   Championship     "
    .by "      2014          "
    .by "                    "
    .by "                    "

    .by "   Magnus Carlsen   "
    .by "                    "    
    .by "        vs          "
    .by "                    "
    .by "    Viswanathan     "
    .by "       Anand        "
    .by "                    "
    
    .by "    Sochi, RU       "
    .by "   7th-28th Nov     "
    
Title3
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "    GAME OVER       "
    .by "                    "
    
    .by "                    "
    .by "     THANKS!        "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    .by "                    "
    
    .by "                    "
    .by "                    "
;-------------------------------------------------------
; PROCEDURE  ShowTitle
; Regs: a - Nr of title
;-------------------------------------------------------

.PROC ShowTitle

; variables   
    .var mTitle .word   ;address in titles memory
    .var titNr .byte   ; nr loop
    .var titRows .byte  ; 16 rows to loop
    .var tit7 .byte  ; about 7 rows per block
    .var titB .byte  ; block
            
    sta titNr
    mwa #Titles mTitle
loopTit    
    lda titNr
    jeq titleFound
    dec titNr
    adw mTitle #320     ; address += 16x20
    jmp loopTit
    
titleFound
    lda #0
    sta titRows
    sta tit7
    sta titB
    sta Str2print+20    ; ends with 0
loopPrnPgTit
    mwa mTitle $C2      ; $C2,$C3 = data address
    ldy #0          ; prepare 20 char string
loopCpyTitStr
    lda ($C2), y     ; read data from memory
    sta Str2print, y
    iny
    cpy #20
    jne loopCpyTitStr
    
    ldx #0
    ldy tit7
    lda titB
    Preset
    PrintStr        ; print string

    adw mTitle #20     ; address += 20
        
    adb tit7 #1     ; ++
    cmp #7          ; if 7 then new block
    jne titNoNewBlock
    
    mva #0 tit7
    inc titB        ; block++
    
titNoNewBlock
    adb titRows #1     ; rows++
    cmp #16
    jne loopPrnPgTit
    
    mva #$ff KEYBCH
loopTitKeypress    
    ldy:iny KEYBCH
    beq loopTitKeypress

    rts

.ENDP

;-------------------------------------------------------
; PROCEDURE  dispCode - displays code on screen (for debug)
; Regs: a - code
;-------------------------------------------------------

.PROC dispCode

    .var cc  .byte   ; char code to look in font table
    pha
    tay
; second char    
    and #$f
    sta cc
    cmp #9
    jcs dCdOv9a
    adb cc #'0'
    jmp dCdOka
dCdOv9a
    adb cc #'7' ; 55 + code >= 65 that is code of "A"-"F"
dCdOka
    mva cc Str2print+1

; first char
    tya
    alr #$ff
    alr #$ff
    alr #$ff
    alr #$ff
    sta cc
    cmp #9
    jcs dCdOv9b
    adb cc #'0'
    jmp dCdOkb
dCdOv9b
    adb cc #'7' ; 55 + code >= 65 that is code of "A"-"F"
dCdOkb
    mva cc Str2print+0

    ldy #0
    sty Str2print+2     ; ends with 0
    
    lda #2
    ldx #0
    
    Preset
    PrintStr        ; print string
    pla
    rts

.ENDP

;-------------------------------------------------------
; PROCEDURE  dispMessage - displays string in $C2,$C3
;-------------------------------------------------------
.PROC dispMessage
    ldy #0
dspMsLoop           ; copy string to print
    lda ($C2),y
    sta Str2print,y
    jeq dspMsExit
    iny
    jmp dspMsLoop
dspMsExit    
    lda #2
    ldx #0
    ldy #0
    Preset
    PrintStr        ; print string        
    rts
.ENDP

Str_LMove .ds 21    ; contains last move

;-------------------------------------------------------
; PROCEDURE  DispStatus - displays current chess status
; (Check, checkmate, stalemate)
;-------------------------------------------------------
.PROC DispStatus
    
    mwa #Str_LMove $C2   ; print last move notation
    dispMessage
    
    mwa #MovesGen_ADDR $C2
    ldy #0
    lda ($C2),y
    jne dspStCanMove
                            ; can not move, checkmate or stalemate
    lda ToMove
    jne dspStBlackCantMv

    mva Gn_WKsq Gn_sq2      ; white to move
    mva #'K' Gn_P
    isCheckAtSquare         ; is check+?
    lda Gn_Ck
    jeq dspStStalemate
    mwa #Str_mate01 $C2
    dispMessage             ; checkmate 0-1
    jmp dspStExit
dspStBlackCantMv
    mva Gn_BKsq Gn_sq2      ; black to move
    mva #'k' Gn_P
    isCheckAtSquare         ; is check+?
    lda Gn_Ck
    jeq dspStStalemate
    mwa #Str_mate10 $C2
    dispMessage             ; checkmate 1-0
    jmp dspStExit
dspStStalemate
    mwa #Str_stalemate $C2
    dispMessage             ; stalemate
    jmp dspStExit
      
dspStCanMove
                            ; can move, maybe check+
    lda ToMove
    jne dspStBlackCanMv
    mva Gn_WKsq Gn_sq2      ; white to move
    mva #'K' Gn_P
    isCheckAtSquare         ; is check+?
    lda Gn_Ck
    jeq dspStExit
    jne dspStCheck
dspStBlackCanMv
    mva Gn_BKsq Gn_sq2      ; black to move
    mva #'k' Gn_P
    isCheckAtSquare         ; is check+?
    lda Gn_Ck
    jeq dspStExit
dspStCheck    
    mwa #Str_check $C2
    dispMessage             ; check+
dspStExit
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  ClickedOnSquare - this "drags" and moves
;-------------------------------------------------------

.PROC ClickedOnSquare

    .var f_sq .byte     ; from square
    .var t_sq .byte     ; to square
    .var kC .byte       ; to calc.sq.
    .var mvNr .byte     ; move sequence nr.
    .var fNwDrg .byte   ; flag, to identify new drag
    .var Sq_DD .byte    ; to save drag before
    
    mva #$ff Sq_DD 
    cpb Cr_sq #$ff
    jne ClSqActCurs
    
    ; replay move, cursor is off
    
    ReplayMove
    jmp ClSqOk
    
ClSqActCurs
    mwa #MovesGen_ADDR $C2   ; $C2,$C3 = generated moves list
    lda #0
    sta mvNr
    sta fNwDrg
ClSqloopSq
    ldy #0
    lda ($C2), y
    jeq ClSqMaybeNewDrag    ; if =0 then no more moves, verify new drag case
        
    ldy #2
    lda ($C2), y
    dey
    sta f_sq
    sbb f_sq #'1'    
    aso f_sq
    aso f_sq
    aso f_sq            ; f_sq = Vert*8 (3 bit-shift to the left)
    lda ($C2), y
    sta kC
    sbb kC #'a'         ; kC = Horiz
    adb f_sq kC         ; f_sq +=kC    
    
    iny                 ; y+=4, fxmpl. Nb1-c3   pointer from "b" to "3"
    iny
    iny
    iny
    
    lda ($C2), y
    dey
    sta t_sq
    sbb t_sq #'1'    
    aso t_sq
    aso t_sq
    aso t_sq            ; t_sq = Vert*8 (3 bit-shift to the left)
    lda ($C2), y
    sta kC
    sbb kC #'a'         ; kC = Horiz
    adb t_sq kC         ; t_sq +=kC
    
    cpb Sq_drag #$ff
    jne ClSqDragWas
    
    cpb Cr_sq f_sq      ; is cursor square = move "from" square
    jne ClSqThis1ok
    
    ldy #0
ClSqLoop3chars
    lda ($C2), y
    sta str2print,y
    iny
    cpy #3
    jne ClSqLoop3chars   
    
    mva #'-' str2print+3
    lda #' '
ClSqLoop20chars    
    iny
    sta str2print,y
    cpy #19
    jne ClSqLoop20chars

    mva #0 str2print+20
      
    ldx #0
    ldy #0
    lda #2
    Preset
    PrintStr        ; print string "Nb1-" when drag at B1
    
    mva f_sq Sq_drag
    
    jmp ClSqOk
ClSqDragWas
    inc fNwDrg          ; set flag, maybe new drag
    
    cpb Sq_drag f_sq    ; "from" square the drag.sq.
    jne ClSqThis1ok

    cpb Cr_sq t_sq      ; is cursor square = move "to" square
    jne ClSqThis1ok
    
    CreaLastMoveString
    
    mwa #Str_blank $C2
    dispMessage         ; clear text line
        
    mva #1 MkMode   ; display
    lda mvNr
    MakeMove
    GenMoves
    DispStatus
    
    mva #$ff Sq_drag    ; no drag
    
ClSqThis1ok
    adw $C2 #8          ; move address pointer to next move
    inc mvNr            ; move seq.++
    jmp ClSqloopSq      ; loop

ClSqMaybeNewDrag

    lda fNwDrg
    jeq ClSqPreOk
    cpb Sq_drag #$ff    ; if drag was and no move then
    jeq ClSqPreOk
    mva Sq_drag Sq_DD   ; save drag before
    mva #$ff Sq_drag    ; no drag, repeat drag finding
    jmp ClSqActCurs
ClSqPreOk
    mva Sq_DD Sq_drag   ; if nothing, then restore            
ClSqOk        
    rts

.ENDP

;-------------------------------------------------------
; PROCEDURE  UnDoPressed - Try redo move
;-------------------------------------------------------

.PROC UnDoPressed

    lda #0
    sta Str_LMove           ; clear last move
    
    mwa #Str_undo $C2
    dispMessage         ; clear text line, type Undo
    
    cpb Cr_sq #$ff
    jne UnDoActCurs
    
    ReplayUndoMove
    
    jmp UnDoDoneOk

UnDoActCurs
    
    mva #1 MkMode   ; display
    UnMakeMove
    GenMoves
    DispStatus
    
    mva #$ff Sq_drag    ; no drag
UnDoDoneOk    
    rts

.ENDP

;-------------------------------------------------------
; PROCEDURE  ClearScreen - Clears Screen
;-------------------------------------------------------

.PROC ClearScreen

    .var cRows .byte    ; count of rows to loop
    .var scrP .byte     ; screen part to clear 0,1,2

    mva #0 scrP
clsLoopScrParts    
    lda scrP
    cmp #3
    jeq clsScrExit
    
    cmp #0
    jne clsNotScr0
    mwa #ScrAddr $C2      ; $C2,$C3 = screen address
    mva #80 cRows
    jmp clsClr
clsNotScr0
    cmp #1
    jne clsNotScr1
    mwa #ScrAdd2 $C2
    mva #80 cRows
    jmp clsClr
clsNotScr1
    mwa #ScrAdd3 $C2
    mva #41 cRows
clsClr

clsLoopRows
    ldy #0
    lda #0
clsLoop40c              ; clear line, set 40 bytes to 0
    sta ($C2),y
    iny
    cpy #40
    jne clsLoop40c
    adw $C2 #40         ; pointer to next line

    dec cRows
    jne clsLoopRows
    
    inc scrP            ; next screen part
    jmp clsLoopScrParts
clsScrExit         
    rts

.ENDP

;-------------------------------------------------------
; PROCEDURE  NewGame - Starts a new game
;-------------------------------------------------------

.PROC NewGame
    
    PutLogo             ; put ATARI logo at first
           
    ldy #0              ; copy starting board
nwCpy0boardLoop
    lda StartingBoard,y
    sta Board,y
    iny
    cpy #64             ; all 64 squares
    jne nwCpy0boardLoop
    
    mwa #MovesHst_ADDR MVhistLastAddr
    mwa #MovesHst_ADDR $C2
    ldy #0
    
    lda #1
    sta MoveNumber
    
    lda #0
    sta ($C2), y        ; no moves in list
    sta Str_LMove       ; no text
    
    sta ToMove          ; white to move
    sta EPsq            ; no en-passant
    sta Castle+0        ; clear castling flags K,Q,k,q
    sta Castle+1
    sta Castle+2
    sta Castle+3
    
    mva #'Q' Piece2promote  ; default promotion is queen

    lda #$ff
    sta Cr_sq       ; no cursor, wait keypress
    sta Sq_drag     ; no drag
    
    PrintBoard
    PutToMove
    GenMoves
    
    mwa #RP_Item $C2
    dispMessage         ; display game info

    rts

.ENDP


ITEM_LEFT_POS    .by 1      ; const.left position for item text
ITEM_WIDTH_C_POS .by 18     ; const.width.of item, position of right part of cursor, max.=19

;-------------------------------------------------------
; PROCEDURE  SelectChessGame - User selects chess game to replay
;  On return:
;    GameEnter = 1 - user selected, 0-ESC
;    GameSelected contains user choice
;-------------------------------------------------------

GameEnter .by 0             ; "enter the game" flag
GameSelected .by 0          ; which game was selected
Games2sel .by 0             ; count of games total

.PROC SelectChessGame
    .var cSp .byte          ; screen part
    .var cRw .byte          ; row per screen part
    .var adr_items .word    ; address where to get items
    .var bI .byte           ; bytes to calc.
    .var tC .byte           ; tmp.char
        
    ClearScreen
    mwa #ChessGames_ADDR adr_items
        
    lda #0
    sta cSp
    sta cRw
    sta GameEnter
    sta Games2sel
slGmLoop
    mwa adr_items $C2
    
    ldy #0
    lda ($C2),y
    jeq slGmExit        ; if 0 then no items anymore
slGmCpyLoop
    lda ($C2),y
    sta str2print,y     ; copy item text to print
    sta tC
    iny
    lda tC
    jne slGmCpyLoop
slGmCpd
    sty bI
    adw adr_items bI

slGmSkipPGN             ; skip PGN of this chess game
    mwa adr_items $C2
    adw adr_items #1    ; pointer to next byte
    ldy #0
    lda ($C2),y
    jne slGmSkipPGN
     
    ldx ITEM_LEFT_POS   ; item x position
    ldy cRw
    lda cSp
    Preset
    PrintStr            ; print item
    inc Games2sel
    inc cRw
    lda cRw
    cmp #6
    jne slGm0
    inc cSp             ; next screen part when 7th row reached
    mva #0 cRw
slGm0
    
    jmp slGmLoop        ; loop next game
slGmExit    
    
    mwa #Str_UserSele $C2   ; Print "Select..."
    DispMessage

    lda #1
    RedrawCC
    
LOOP_SeleKeyboard

    lda GameEnter       ; is Enter pressed?
    jne slGmOK

    mva #$ff KEYBCH
loopSeleKbKeypress
    lda KEYBCH
    cmp #$ff
    jeq loopSeleKbKeypress

    SelectGameKey
    
    lda KEYBCH
    cmp #$1c        ; is Esc key in buffer?
    jne LOOP_SeleKeyboard   ; otherwise loop

slGmOK    
    ClearScreen
    
    rts

.ENDP

pre2Key  .by 0       ; contains key pressed previously
;-------------------------------------------------------
; PROCEDURE  SelectGameKey - User key control
;-------------------------------------------------------

.PROC SelectGameKey

    lda KEYBCH
    ;dispCode
    
; verify keyboard keys
    cmp #$26     ;key "/" repeats prev.key
    jne SkbC0

    lda pre2Key  ; use prev.key
SkbC0
    sta pre2Key  ; save prev.key to repeat 
    
    cmp #$86     ;Left
    jne SkbC1
    jmp SkbUp
SkbC1    
    cmp #$87     ;Right
    jne SkbC2
    jmp SkbDn
SkbC2        
    cmp #$8e     ;Up
    jne SkbC3
SkbUp    
    pha
    lda GameSelected
    jeq SkipKbUp0       ; is this the first game of list?
    lda #0
    RedrawCC
    dec GameSelected
    lda #1
    RedrawCC
SkipKbUp0
    pla
    jmp SkbOk
SkbC3        

    cmp #$8f     ;Down
    jne SkbC4
SkbDn
    pha
    ldy GameSelected
    iny
    cpy Games2sel       ; is this the last game of list?
    jeq SkipKbDn0
    
    lda #0
    RedrawCC
    inc GameSelected    
    lda #1
    RedrawCC
    pla
    jmp SkbOk
SkipKbDn0
    pla

SkbC4
    cmp #$21     ;Space key
    jne SkbC5
    jmp SkbClickedEnter
SkbC5        
    cmp #$0c     ;Enter key
    jne SkbC6
SkbClickedEnter
    lda #1
    sta GameEnter
    ReplayPrepSet       ; prepare pointer    
    jmp SkbOk
SkbC6

    cmp #$1c     ;Esc key
SkbOk         
    rts
    
.ENDP


;-------------------------------------------------------
; PROCEDURE  RedrawCC - Redraws selection cursor
;  Parameter: A = 0-clear, 1-draw cursor
;-------------------------------------------------------
.PROC RedrawCC
    .var ClrDrw .byte   ; parm.
    .var pS .byte       ; screen part
    .var pY .byte       ; row
    .var pk .byte       ; to loop row
    
    sta ClrDrw
    lda #0
    sta pS
    sta pY
    sta pk
    sta Str2print+1
RdrwCCloop              ; calculate preset position
    cpb pk GameSelected
    jeq RdrwCCexit
    inc pk
    inc pY
    lda pY
    cmp #6
    jne RdrwCC0
    inc pS
    mva #0 pY
RdrwCC0
    jmp RdrwCCloop
RdrwCCexit
    lda ClrDrw
    jne RdrwDrwCC1
    lda #' '
    jne RdrwCC1
RdrwDrwCC1
    lda #130
RdrwCC1
    sta Str2print
    ldx #0
    ldy pY
    lda pS
    Preset
    PrintStr                ; display/clear left part of cursor

    lda ClrDrw
    jne RdrwDrwCC2
    lda #' '
    jne RdrwCC2
RdrwDrwCC2
    lda #131
RdrwCC2
    sta Str2print
    ldx ITEM_WIDTH_C_POS
    ldy pY
    lda pS
    Preset
    PrintStr                ; display/clear right part of cursor
    
    rts
.ENDP

RP_addr .wo 0     ; replay address position to get moves list
RP_Item .by "                    ",0  ; contains game title

RP_Res0 .by "1-0     resigned ",0
RP_Res1 .by "0-1     resigned ",0
RP_Res2 .by "1/2-1/2          ",0


RP_Result .by 0   ; result data (0 for 1-0, 1 for 0-1, 2 for 1/2-1/2)

;-------------------------------------------------------
; PROCEDURE  ReplayMove - Replayus current move in the list
;-------------------------------------------------------
.PROC ReplayMove

    ;Variables
    .var Lf_sq .byte ; square from in the list
    .var Lt_sq .byte ; square to in the list

    .var f_sq .byte ; square from
    .var t_sq .byte ; square to
    .var mNr .byte  ; move number
    .var kC .byte   ; to calc.
   
    mwa RP_addr $C2
    ldy #0
    lda ($C2),y     ; read square from
    jeq RPmvOVERexit    ; the last move
    sta Lf_sq
    sbb Lf_sq #128
    iny
    lda ($C2),y     ; read square to
    sta Lt_sq
    sbb Lt_sq #128
    iny
    adw RP_addr #2
    
    lda ($C2),y     ; read promotion flag
    cmp #$ff        ; if flag byte = 255
    jne RPmvSkipPromo
    iny
    lda ($C2),y     ; read promoted piece
    sta Piece2promote   ; QRBN
    adw RP_addr #2
    
RPmvSkipPromo    
    
    ; find which move in the gemerated possible moves list

    mwa #MovesGen_ADDR $C2   ; $C2,$C3 = generated moves list
    lda #0
    sta mNr
RPmvloopSqCompares
    ldy #0
    lda ($C2), y
    jeq RPmvOVERexit    ; if =0 then no more moves
        
    ldy #2
    lda ($C2), y
    dey
    sta f_sq
    sbb f_sq #'1'    
    aso f_sq
    aso f_sq
    aso f_sq            ; f_sq = Vert*8 (3 bit-shift to the left)
    lda ($C2), y
    sta kC
    sbb kC #'a'         ; kC = Horiz
    adb f_sq kC         ; f_sq +=kC    
    
    iny                 ; y+=4, fxmpl. Nb1-c3   pointer from "b" to "3"
    iny
    iny
    iny
    
    lda ($C2), y
    dey
    sta t_sq
    sbb t_sq #'1'    
    aso t_sq
    aso t_sq
    aso t_sq            ; t_sq = Vert*8 (3 bit-shift to the left)
    lda ($C2), y
    sta kC
    sbb kC #'a'         ; kC = Horiz
    adb t_sq kC         ; t_sq +=kC
    
    cpb Lf_sq f_sq      ; is this move?
    jne RPmvThis1ok
    
    cpb Lt_sq t_sq
    jne RPmvThis1ok
                        ;  found move, make it
    CreaLastMoveString
    
    mva #1 MkMode   ; display
    lda mNr
    MakeMove
    GenMoves
    DispStatus
        
    jmp RPmvOk
    
RPmvThis1ok
    adw $C2 #8          ; move address pointer to next move
    inc mNr            ; move seq.++
    jmp RPmvloopSqCompares      ; loop

RPmvOVERexit

    lda RP_Result
    jne RPmvOVnot0
    mwa #RP_Res0 $C2    ; 1-0
    jmp RPmvOVdsp
RPmvOVnot0
    cmp #1
    jne RPmvOVnot1
    mwa #RP_Res1 $C2    ; 0-1
    jmp RPmvOVdsp
RPmvOVnot1
    mwa #RP_Res2 $C2    ; 1/2
RPmvOVdsp
    dispMessage         ; display game info
    
RPmvOk

    rts
.ENDP
;-------------------------------------------------------
; PROCEDURE  ReplayUndoMove - Position after undo in the list
;-------------------------------------------------------
.PROC ReplayUndoMove

    mwa RP_addr $C2
    sbw $C2 #2
    ldy #1
    lda ($C2),y             ; if 0 then this is the first move
    jeq RPUndoMvOVERexit    ; if nothing to undo

    sbw RP_addr #2          ; addr-=2
    ldy #0
    lda ($C2),y
    cmp #$ff
    jne RPUndoMvDone    ; if no promo flag, then 2bytes before

    sbw RP_addr #2      ; otherwise addr-=2 another 2 bytes before

RPUndoMvDone
    
    mva #1 MkMode   ; display
    UnMakeMove
    GenMoves
    DispStatus

RPUndoMvOVERexit

    rts
.ENDP
;-------------------------------------------------------
; PROCEDURE  ReplayPrepSet - Sets the starting address of the list
;-------------------------------------------------------
.PROC ReplayPrepSet

    .var cS .byte   ; select loop
    .var cX .byte   ; 2-times loop
    .var lastAddr .word ; to copy last item text
    .var lastY .byte

    mwa #ChessGames_ADDR RP_addr
    
    mwa RP_addr lastAddr
    mva #0 lastY
RPprepSkp0Itm         ; skip first item
    mwa RP_addr $C2
    adw RP_addr #1    ; pointer to next byte
    inc lastY
    ldy #0
    lda ($C2),y
    jne RPprepSkp0Itm

    mva #0 cS
RPprepLoop
    cpb cS GameSelected ; if this game
    jeq RPprepOk
    mva #2 cX
RPprepSkp2times         ; skip 2 times
    mwa RP_addr lastAddr
    mva #0 lastY
RPprepSkpItmPGN         ; skip PGN/Item of one chess game
    mwa RP_addr $C2
    adw RP_addr #1    ; pointer to next byte
    inc lastY
    ldy #0
    lda ($C2),y
    jne RPprepSkpItmPGN
    dec cX
    jne RPprepSkp2times
    inc cS
    jmp RPprepLoop
RPprepOk

    mwa lastAddr $C2    ; copy last item text just to print it out (Game, result)
    ldy #0
RPprepCpyItem
    lda ($C2),y
    sta RP_Item, y
    iny
    cpy lastY
    jne RPprepCpyItem
    
    dey                 ; take result data 2 bytes before as part of Item "...1-[0]"
    dey
    lda ($C2),y
    sta RP_Result
    sbb RP_Result #'0'

    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  A to decimal string in numbStr
;-------------------------------------------------------
numbStr .ds 3           ; 3 chars for result "255"
        .by 0           ; ends with 0
nmbdc   .by 100,10,1    ; to calc
        
.PROC a2Str

    .var dC .byte   ; numb. to calc.
    .var dI .byte   ; which 100,10 or 1
    .var dN .byte   ; to compare
    .var Ch .byte   ; resulting char
    
    sta dC
    mva #0 dI
nmbLoop3
    ldy dI
    lda #'0'
    sta Ch          ; set char="0"
    lda nmbdc,y
    sta dN
nmbLoop0
    cpb dC dN
    jcc nmbSkip0
    inc Ch          ; set next char code
    sbb dC dN       ; -100 or 10 or 1 till
    jmp nmbLoop0 
nmbSkip0
    ldy dI
    lda Ch
    sta numbStr,y
    cpy #2
    jeq nmb0ok
    inc dI
    jmp nmbLoop3
nmb0ok

nmb0ok2
    lda numbStr     ; remove leading "0"s 
    cmp #'0'
    jne nmbOk
    
    mva #1 dI
nmbRmv0loop
    ldy dI
    lda numbStr,y
    dey
    sta numbStr,y
    adb dI #1
    cmp #4
    jne nmbRmv0loop
    jmp nmb0ok2    
    
nmbOk    
    rts
.ENDP

str_000  .by "0-0-0  ",0

;-------------------------------------------------------
; PROCEDURE  CreaLastMoveString - generates last move info line
;-------------------------------------------------------
.PROC CreaLastMoveString

    .var pI .byte   ; char in string
    .var tmpY .byte 
    
    mva #0 pI
    lda MoveNumber
    a2Str           ; create decimal string

    ldy #0
clmvLoopN           ; copy to last move string
    lda numbStr,y
    sta Str_LMove, y
    jeq clmv0
    iny
    jmp clmvLoopN
clmv0

    ldx #1
    lda ToMove
    jeq clmvW
    ldx #3          ; 3x"." for black move
clmvW    
    lda #'.'
    sta Str_LMove, y
    iny
    dex
    cpx #0
    jne clmvW

    sty tmpY
    ldy #0          ; if castling then display other notation
    lda ($C2),y
    cmp #'K'
    jne clmvNotCs
    iny 
    lda ($C2),y
    cmp #'e'
    jne clmvNotCs
    iny 
    iny
    iny 
    lda ($C2),y
    cmp #'g'
    jne clmvNot00
    ldx #2                     ; display 0-0
    jmp clmvCstlCpy
clmvNot00
    cmp #'c'
    jne clmvNotCs
    ldx #0                     ; display 0-0-0
clmvCstlCpy
    ldy tmpY
clmvCstlLoop00
    lda str_000,x
    ldy tmpY
    sta Str_LMove, y
    inc tmpY
    inx
    cpx #6
    jne clmvCstlLoop00
    ldy tmpY
    jmp clmvSpacing
clmvNotCs
    ldy tmpY
    mva #0 pI       ; and 8 chars for move notation
clmvLoop8ch
    sty tmpY
    ldy pI    
    lda ($C2),y
    ldy tmpY
    cmp #'P'        ; skip pawn in notation
    jeq clmvSkpP
    
    sta Str_LMove, y
    iny
clmvSkpP
    adb pI #1
    cmp #8          ; ++ till 8
    jne clmvLoop8ch
    
clmvSpacing    
    lda #' '        ; add spaces
clmv20sp
    sta Str_LMove, y
    iny
    cpy #20
    jne clmv20sp
    lda #0          ; end with 0
    sta Str_LMove, y
    
    rts

.ENDP

   icl "ChessLg.asm" 


   org ChessGames_ADDR      ; Load chess games into memory

   ;icl "wch14.asm"          ; just above 2KB
   ins "wch14.bin"    ; just binary version

    
; End of program



      



