;-------------------------------------------------
;
; Chess logic part for ChessAtr.asm
;   Uses global variables: Board, ToMove, EPsq,
;   Castle, Piece2promote, MoveNumber
;
;-------------------------------------------------

; Current square "from"
Gn_sq   .by 0   ; 0..63
Gn_V    .by 0   ; Vertical 0..7 (1-8)
Gn_H    .by 0   ; Horizontal 0..7 (A-H)
Gn_U    .by 0   ; piece on square in uppercase (PNB...K)
Gn_P    .by 0   ; piece on square (PNB...Kpnb...k)


; Square "to move"
Gn_sq2  .by 0   ; 0..63
Gn_V2   .by 0   ; Vertical 0..7 (1-8)
Gn_H2   .by 0   ; Horizontal 0..7 (A-H)
Gn_P2   .by 0   ; piece on square (PNB...Kpnb...k)
Gn_addr .wo 0   ; Address to store new move in list


; Square to verify check+
Gn_sqK  .by 0   ; 0..63
Gn_Ck   .by 0   ; verification result 0-clear,1=check+
Gn_WKsq .by 0   ; temp.white king position
Gn_BKsq .by 0   ; temp.black king position

;-------------------------------------------------------
; PROCEDURE  GenMoves   - generates moves for pieces
;-------------------------------------------------------
;
.PROC GenMoves
    
    mwa #MovesGen_ADDR Gn_addr
    mwa #MovesGen_ADDR $C2
    
    lda #0
    sta Gn_sq
    ldy #0
    sta ($C2), y    ; no moves in list

;get positions of white king and black king
    tax
GnLoop64sq
    lda Board, x
    cmp #'K'
    jne GnNotWK
    stx Gn_WKsq
GnNotWK     
    cmp #'k'
    jne GnNotBK
    stx Gn_BKsq
GnNotBK     
    inx
    cpx #64
    jne GnLoop64sq
;.....

GnLoop64sq2
    ldx Gn_sq
    lda Board, x
    cmp #' '
    jeq GnOk ; skip empty squares
    txa
    sta Gn_sq2  ; square to = square from
    and #7
    sta Gn_H    ; A-H for sq [0..7]
    txa
    alr #$ff
    alr #$ff
    alr #$ff    
    sta Gn_V    ; 1-8 for sq [0..7]
    
    lda Board,x
    sta Gn_U
    sta Gn_P 
    lda ToMove        
    jeq GnMvIfw
    sbb Gn_U #32 ;  char code to uppercase
GnMvIfw
    ldy ToMove
    lda Gn_U
            ;which piece?
    cmp #'P'
    jeq GnPawn
    cmp #'N'
    jeq GnKnight
    cmp #'B'
    jeq GnBishop
    cmp #'R'
    jeq GnRook
    cmp #'Q'
    jeq GnQueen
    cmp #'K'
    jeq GnKing          
    jne GnOk 

GnPawn
    GenPawn
    jmp GnOk
GnKnight
    GenKnight
    jmp GnOk
GnBishop
    GenBishop
    jmp GnOk
GnRook
    GenRook
    jmp GnOk
GnQueen
    GenBishop
    GenRook
    jmp GnOk
GnKing
    GenKing
    
GnOk
    adb Gn_sq #1
    cmp #64
    jne GnLoop64sq2
    
    rts

.ENDP

;-------------------------------------------------------
; Parameters in registers:
;    x - square 0..63
;    y - white(0) or black(1)

;-------------------------------------------------------
; PROCEDURE  GenPawn   - generates pawn moves
;-------------------------------------------------------
.PROC GenPawn
    cpy #0
    jne GnPwBlack
    
; White pawn   
    adb Gn_sq2 #8   ; vert+1
    ldy Gn_sq2
    lda Board, y
    cmp #' '        ; empty?
    jne GnPwWn1
    AddMove2list    ; e2-e3
    
    lda Gn_V
    cmp #1          ; if line 2 then e2-e4
    jne GnPwWn1

    adb Gn_sq2 #8   ; vert+2
    ldy Gn_sq2
    lda Board, y
    cmp #' '        ; empty?
    jne GnPwWn1
    AddMove2list    ; e2-e4
    
GnPwWn1
        ;capturing
    mva Gn_sq Gn_sq2
    adb Gn_sq2 #7   ; vert+1 horiz-1

    lda Gn_H
    cmp #1          ; B-H?
    jcc GnPwWn2
    
    ldy Gn_sq2
    cpy EPsq        ; if en-passant then capture
    jeq GnPwW1e
    lda Board, y
    cmp #'a'        ; enemy?
    jcc GnPwWn2
GnPwW1e  
    AddMove2list    ; e2xd3 capture
    
GnPwWn2  

    adb Gn_sq2 #2   ; vert+1 horiz+1
    
    lda Gn_H
    cmp #7          ; A-G?
    jcs GnPwOk
    
    ldy Gn_sq2
    cpy EPsq        ; if en-passant then capture
    jeq GnPwW2e
    lda Board, y
    cmp #'a'        ; enemy?
    jcc GnPwOk
GnPwW2e
    AddMove2list    ; e2xf3 capture
    jmp GnPwOk
    
GnPwBlack
; Black pawn   
    sbb Gn_sq2 #8   ; vert-1
    ldy Gn_sq2
    lda Board, y
    cmp #' '        ; empty?
    jne GnPwBn1
    AddMove2list    ; e7-e6
    
    lda Gn_V
    cmp #6          ; if line 7 then e7-e5
    jne GnPwBn1

    sbb Gn_sq2 #8   ; vert-2
    ldy Gn_sq2
    lda Board, y
    cmp #' '        ; empty?
    jne GnPwBn1
    AddMove2list    ; e7-e5
    
GnPwBn1
        ;capturing
    mva Gn_sq Gn_sq2
    sbb Gn_sq2 #9   ; vert-1 horiz-1
    
    lda Gn_H
    cmp #1          ; B-H?
    jcc GnPwBn2
    
    ldy Gn_sq2
    cpy EPsq        ; if en-passant then capture
    jeq GnPwB1e
    lda Board, y
        
    cmp #'a'        ; enemy?
    jcs GnPwBn2
    cmp #' '        ; is empty?
    jeq GnPwBn2

GnPwB1e  
    AddMove2list    ; e7xd6 capture
    
GnPwBn2  

    adb Gn_sq2 #2   ; vert-1 horiz+1
    
    lda Gn_H
    cmp #7          ; A-G?
    jcs GnPwOk
    
    ldy Gn_sq2
    cpy EPsq        ; if en-passant then capture
    jeq GnPwB2e
    lda Board, y
    cmp #'a'        ; enemy?
    jcs GnPwOk
    cmp #' '        ; is empty?
    jeq GnPwOk
    
GnPwB2e
    AddMove2list    ; e7xf6 capture
    
GnPwOk  

    rts
.ENDP


;-------------------------------------------------------
; PROCEDURE  GenKnight   - generates knight moves
;-------------------------------------------------------
.PROC GenKnight
    
    lda Gn_H
    cmp #6          ; A-F?
    jcs GnKn1
    lda Gn_V
    cmp #7          ; 1-7?
    jcs GnKn1
    adb Gn_sq2 #10  ; vert+1 horiz+2
    GnAddIfEmptyOrEnemy
GnKn1
    lda Gn_H
    cmp #7          ; A-G?
    jcs GnKn2
    lda Gn_V
    cmp #6          ; 1-6?
    jcs GnKn2
    mva Gn_sq Gn_sq2
    adb Gn_sq2 #17  ; vert+2 horiz+1
    GnAddIfEmptyOrEnemy
GnKn2
    lda Gn_H
    cmp #2          ; C-H?
    jcc GnKn3
    lda Gn_V
    cmp #1          ; 2-8?
    jcc GnKn3
    mva Gn_sq Gn_sq2
    sbb Gn_sq2 #10  ; vert-1 horiz-2
    GnAddIfEmptyOrEnemy
GnKn3
    lda Gn_H
    cmp #1          ; B-H?
    jcc GnKn4
    lda Gn_V
    cmp #2          ; 3-8?
    jcc GnKn4
    mva Gn_sq Gn_sq2
    sbb Gn_sq2 #17  ; vert-2 horiz-1
    GnAddIfEmptyOrEnemy
GnKn4
    lda Gn_H
    cmp #7          ; A-G?
    jcs GnKn5
    lda Gn_V
    cmp #2          ; 3-8?
    jcc GnKn5
    mva Gn_sq Gn_sq2
    sbb Gn_sq2 #15  ; vert-2 horiz+1
    GnAddIfEmptyOrEnemy
GnKn5
    lda Gn_H
    cmp #6          ; A-F?
    jcs GnKn6
    lda Gn_V
    cmp #1          ; 2-8?
    jcc GnKn6
    mva Gn_sq Gn_sq2
    sbb Gn_sq2 #6   ; vert-1 horiz+2
    GnAddIfEmptyOrEnemy
GnKn6
    lda Gn_H
    cmp #2          ; C-H?
    jcc GnKn7
    lda Gn_V
    cmp #7          ; 1-7?
    jcs GnKn7
    mva Gn_sq Gn_sq2
    adb Gn_sq2 #6   ; vert+1 horiz-2
    GnAddIfEmptyOrEnemy
GnKn7
    lda Gn_H
    cmp #1          ; B-H?
    jcc GnKn8
    lda Gn_V
    cmp #6          ; 1-6?
    jcs GnKn8
    mva Gn_sq Gn_sq2
    adb Gn_sq2 #15  ; vert+2 horiz-1
    GnAddIfEmptyOrEnemy
GnKn8

GnKnOk  
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  GenBishop   - generates bishop moves
;-------------------------------------------------------
.PROC GenBishop

GnBsLoop1    
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #7          ; A-G?
    jcs GnBs1
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs GnBs1
    adb Gn_sq2 #9  ; vert+1 horiz+1
    GnAddIfEmptyOrEnemy
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq GnBsLoop1     
GnBs1
    mva Gn_sq Gn_sq2
GnBsLoop2
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #7          ; A-G?
    jcs GnBs2
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc GnBs2
    sbb Gn_sq2 #7  ; vert-1 horiz+1
    GnAddIfEmptyOrEnemy
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq GnBsLoop2    
GnBs2
    mva Gn_sq Gn_sq2
GnBsLoop3
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #1          ; B-H?
    jcc GnBs3
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc GnBs3
    sbb Gn_sq2 #9  ; vert-1 horiz-1
    GnAddIfEmptyOrEnemy
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq GnBsLoop3    
GnBs3
    mva Gn_sq Gn_sq2
GnBsLoop4
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #1          ; B-H?
    jcc GnBs4
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs GnBs4
    adb Gn_sq2 #7  ; vert+1 horiz-1
    GnAddIfEmptyOrEnemy
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq GnBsLoop4    
GnBs4
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  GenRook   - generates rook moves
;-------------------------------------------------------
.PROC GenRook
    mva Gn_sq Gn_sq2
GnRkLoop1    
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #7          ; A-G?
    jcs GnRk1
    inc Gn_sq2      ; horiz+1
    GnAddIfEmptyOrEnemy
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq GnRkLoop1     
GnRk1
    mva Gn_sq Gn_sq2
GnRkLoop2    
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #1          ; B-H?
    jcc GnRk2
    dec Gn_sq2      ; horiz-1
    GnAddIfEmptyOrEnemy
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq GnRkLoop2     
GnRk2
    mva Gn_sq Gn_sq2    
GnRkLoop3    
    GnSqHV2         ; update H,V
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc GnRk3
    sbb Gn_sq2 #8   ; vert-1
    GnAddIfEmptyOrEnemy
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq GnRkLoop3     
GnRk3
    mva Gn_sq Gn_sq2
GnRkLoop4    
    GnSqHV2         ; update H,V
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs GnRk4
    adb Gn_sq2 #8   ; vert+1
    GnAddIfEmptyOrEnemy
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq GnRkLoop4     
GnRk4  
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  GenKing   - generates king moves
;-------------------------------------------------------
.PROC GenKing

    .var loop_I  .byte ; to loop 2 or 3 squares
    
    lda Gn_H
    cmp #7          ; A-G?
    jcs GnKg1
    inc Gn_sq2      ; horiz+1, vert
    GnAddIfEmptyOrEnemy
GnKg1
    mva Gn_sq Gn_sq2
    lda Gn_V
    cmp #7          ; 1-7?
    jcs GnKg2
    adb Gn_sq2 #8   ; horiz,vert+1
    GnAddIfEmptyOrEnemy
GnKg2
    mva Gn_sq Gn_sq2
    lda Gn_H
    cmp #1          ; B-H?
    jcc GnKg3
    dec Gn_sq2      ; horiz-1,vert
    GnAddIfEmptyOrEnemy
GnKg3
    mva Gn_sq Gn_sq2
    lda Gn_V
    cmp #1          ; 2-8?
    jcc GnKg4
    sbb Gn_sq2 #8   ; horiz,vert-1
    GnAddIfEmptyOrEnemy
    
 ;diognals
GnKg4
    mva Gn_sq Gn_sq2
    lda Gn_V
    cmp #7          ; 1-7?
    jcs GnKg5
    lda Gn_H
    cmp #7          ; A-G?
    jcs GnKg5
    adb Gn_sq2 #9   ; horiz+1,vert+1
    GnAddIfEmptyOrEnemy
GnKg5
    mva Gn_sq Gn_sq2
    lda Gn_V
    cmp #1          ; 2-8?
    jcc GnKg6
    lda Gn_H
    cmp #7          ; A-G?
    jcs GnKg6
    sbb Gn_sq2 #7   ; horiz+1,vert-1
    GnAddIfEmptyOrEnemy
GnKg6
    mva Gn_sq Gn_sq2
    lda Gn_V
    cmp #1          ; 2-8?
    jcc GnKg7
    lda Gn_H
    cmp #1          ; B-H?
    jcc GnKg7
    sbb Gn_sq2 #9   ; horiz-1,vert-1
    GnAddIfEmptyOrEnemy
GnKg7
    mva Gn_sq Gn_sq2
    lda Gn_V
    cmp #7          ; 1-7?
    jcs GnKg8
    lda Gn_H
    cmp #1          ; B-H?
    jcc GnKg8
    adb Gn_sq2 #7   ; horiz-1,vert+1
    GnAddIfEmptyOrEnemy
GnKg8

; castlings
    lda ToMove
    jne GnKgBlack
    
    ldy Gn_sq
    cpy #4          ; is king on E1?
    jne GnKgOk

; ---- White KingSide castling
    ldy Gn_sq
    lda Castle+0    ; verify K-castling
    jne GnKgWKok

    mva #2 loop_I   ; verify: are F1,G1 empty?
GnKgWK1
    iny
    lda Board,y
    cmp #' '
    jne GnKgWKok
    sbb loop_I #1   ; loop_I--
    jne GnKgWK1
    
    mva Gn_sq Gn_sq2    ; king square
    mva #3 loop_I   ; verify: are E1,F1,G1 under check?
GnKgWK2
    isCheckAtSquare ; Verify CHECK+
    cpb Gn_Ck #1
    jeq GnKgWKok
    
    sbb loop_I #1   ; loop_I--
    jeq GnKgWK3
    inc Gn_sq2
    jmp GnKgWK2
GnKgWK3  
    AddMove2list    ; 0-0 for white king
GnKgWKok

; ---- White QueenSide castling
    ldy Gn_sq
    lda Castle+1    ; verify Q-castling
    jne GnKgWQok

    mva #3 loop_I   ; verify: are D1,C1,B1 empty?
GnKgWQ1
    dey
    lda Board,y
    cmp #' '
    jne GnKgWQok
    sbb loop_I #1   ; loop_I--
    jne GnKgWQ1
    
    mva Gn_sq Gn_sq2    ; king square
    mva #3 loop_I   ; verify: are E1,D1,C1 under check?
GnKgWQ2
    isCheckAtSquare ; Verify CHECK+
    cpb Gn_Ck #1
    jeq GnKgWQok
    
    sbb loop_I #1   ; loop_I--
    jeq GnKgWQ3
    dec Gn_sq2
    jmp GnKgWQ2
GnKgWQ3  
    AddMove2list    ; 0-0-0 for white king
GnKgWQok
    jmp GnKgOk

GnKgBlack
    ldy Gn_sq
    cpy #60         ; is king on E8?
    jne GnKgOk

; ---- Black KingSide castling
    ldy Gn_sq
    lda Castle+2    ; verify k-castling
    jne GnKgBKok

    mva #2 loop_I   ; verify: are F8,G8 empty?
GnKgBK1
    iny
    lda Board,y
    cmp #' '
    jne GnKgBKok
    sbb loop_I #1   ; loop_I--
    jne GnKgBK1
    
    mva Gn_sq Gn_sq2    ; king square
    mva #3 loop_I   ; verify: are E8,F8,G8 under check?
GnKgBK2
    isCheckAtSquare ; Verify CHECK+
    cpb Gn_Ck #1
    jeq GnKgBKok
    
    sbb loop_I #1   ; loop_I--
    jeq GnKgBK3
    inc Gn_sq2
    jmp GnKgBK2
GnKgBK3  
    AddMove2list    ; 0-0 for black king
GnKgBKok

; ---- Black QueenSide castling
    ldy Gn_sq
    lda Castle+3    ; verify q-castling
    jne GnKgBQok

    mva #3 loop_I   ; verify: are D8,C8,B8 empty?
GnKgBQ1
    dey
    lda Board,y
    cmp #' '
    jne GnKgBQok
    sbb loop_I #1   ; loop_I--
    jne GnKgBQ1
    
    mva Gn_sq Gn_sq2    ; king square
    mva #3 loop_I   ; verify: are E8,D8,C8 under check?
GnKgBQ2
    isCheckAtSquare ; Verify CHECK+
    cpb Gn_Ck #1
    jeq GnKgBQok
    
    sbb loop_I #1   ; loop_I--
    jeq GnKgBQ3
    dec Gn_sq2
    jmp GnKgBQ2
GnKgBQ3  
    AddMove2list    ; 0-0-0 for black king
GnKgBQok

GnKgOk  
    rts
.ENDP

;-------------------------------------------------------
; If empty or enemy then adds move to list
;-------------------------------------------------------
.PROC GnAddIfEmptyOrEnemy

    ldy Gn_sq2
    lda Board, y
    cmp #' '        ; empty?
    jeq GnAdd1      ; if empty then add move to list
    
    ldx ToMove
    cpx #1
    jeq GnAddBlToMove
    cmp #'a'        ; does white captures black piece?
    jcc GnAddEnd
GnAdd1    
    AddMove2list
    jmp GnAddEnd
    
GnAddBlToMove       ; for black piece      
    cmp #'a'        ; does black captures white piece?
    jcc GnAdd1
        
GnAddEnd
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  GnSqHV2 sets Gn_H2, Gn_V2 values
;-------------------------------------------------------
.PROC GnSqHV2
    lda Gn_sq2
    pha
    and #7
    sta Gn_H2    ; A-H for to-square [0..7]
    pla
    alr #$ff
    alr #$ff
    alr #$ff
    sta Gn_V2    ; 1-8 for to-square [0..7]    
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  AddMove2list
;  Adds text "Rd7-b8# " to the list
;-------------------------------------------------------
.PROC AddMove2list

    VerifyBeforeMove    ; place piece at new position and verify our king before moving
    cpb Gn_Ck #1        ; if check+, then the move is illegal
    jeq AddMVok

    mwa Gn_addr $C2      ; $C2,$C3 = data address
    ldy #0

    lda Gn_U
    sta ($C2), y    ; Piece P,N,B,R,Q,K
    iny
    
    lda Gn_H
    clc
    adc #'a'
    sta ($C2), y    ; from H
    iny
    lda Gn_V
    clc
    adc #'1'
    sta ($C2), y    ; from V
    iny

    ldx Gn_sq2
    cpx #0
    jeq AddMVnoEP
    cpx EPsq
    jne AddMVnoEP
    cpb Gn_U #'P'
    jne AddMVnoEP
    jeq AddMVcapt
AddMVnoEP        
    lda Board, x
    tax
    lda #'-'        ; move sign
    cpx #' '
    jeq AddMVskipCapt
AddMVcapt    
    lda #'x'        ; piece captured
AddMVskipCapt
    sta ($C2), y    ; sign "-" or "x"
    iny
    
    lda Gn_sq2
    pha
    and #7
    sta Gn_H2    ; A-H for to-square [0..7]
    clc
    adc #'a'
    sta ($C2), y    ; to H
    iny
    pla
    alr #$ff
    alr #$ff
    alr #$ff
    sta Gn_V2    ; 1-8 for to-square [0..7]    
    clc
    adc #'1'
    sta ($C2), y    ; to V
    iny
    
    lda #' '
    sta ($C2), y    ; adds space
    iny
    sta ($C2), y    ; adds space
    iny
    
    lda #0
    sta ($C2), y    ; ends with 0
    
    adw Gn_addr #8
AddMVok    
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  isCheckAtSquare  - Verifies check on square
;  Parameters: Gn_sq2 contains square to verify
;              Gn_P - king to check
;  On return:  Gn_Ck = 1 if Check+
;-------------------------------------------------------
.PROC isCheckAtSquare
    mva #0 Gn_Ck        ; no check
    mva Gn_sq2 Gn_sqK   
    GnSqHV2         ; update H,V
; Verify pawn check+
    lda Gn_P
    cmp #'K'
    jne isChBlackKing
    
; for White king    
    adb Gn_sq2 #7   ; vert+1, horiz-1
    lda Gn_V2
    cmp #6          ; 1-6?
    jcs isChcont
    
    ldx Gn_H2
    cpx #1          ; B-H?
    jcc isChWK1
    ldy Gn_sq2
    lda Board, y
    cmp #'p'
    jeq isChCheck
isChWK1
    adb Gn_sq2 #2   ; vert+1, horiz+1
    cpx #7          ; A-G?
    jcs isChcont
    ldy Gn_sq2
    lda Board, y
    cmp #'p'
    jeq isChCheck
    jmp isChcont
    
isChBlackKing
; for Black king

    sbb Gn_sq2 #9   ; vert-1, horiz-1
    lda Gn_V2
    cmp #2          ; 3-8?
    jcc isChcont
    
    ldx Gn_H2
    cpx #1          ; B-H?
    jcc isChBK1
    ldy Gn_sq2
    lda Board, y
    cmp #'P'
    jeq isChCheck
isChBK1
    adb Gn_sq2 #2   ; vert-1, horiz+1
    cpx #7          ; A-G?
    jcs isChcont
    ldy Gn_sq2
    lda Board, y
    cmp #'P'
    jeq isChCheck

isChcont
; continue, verify other pieces NBRQK

    isChKnights         ; verify knights that can beat the king
    cpb Gn_Ck #1
    jeq isChReturn
    isChBishops         ; verify bishops or queens that can beat the king
    cpb Gn_Ck #1
    jeq isChReturn
    isChRooks           ; verify rooks or queens that can beat the king
    cpb Gn_Ck #1
    jeq isChReturn    
    isChKings           ; verify kings that can beat the king

    jmp isChReturn
isChCheck
    mva #1 Gn_Ck        ; CHECK+
isChReturn
    mvx Gn_sqK Gn_sq2
    rts   
.ENDP

;-------------------------------------------------------
; PROCEDURE  isChKnights - is king under Knight check?
; On return sets Gn_Ck value
;-------------------------------------------------------
.PROC isChKnights
    mva Gn_sqK Gn_sq2
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #6          ; A-F?
    jcs isChKn1
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs isChKn1
    adb Gn_sq2 #10  ; vert+1 horiz+2
    isCh1Knight
isChKn1
    lda Gn_H2
    cmp #7          ; A-G?
    jcs isChKn2
    lda Gn_V2
    cmp #6          ; 1-6?
    jcs isChKn2
    mva Gn_sqK Gn_sq2
    adb Gn_sq2 #17  ; vert+2 horiz+1
    isCh1Knight
isChKn2
    lda Gn_H2
    cmp #2          ; C-H?
    jcc isChKn3
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc isChKn3
    mva Gn_sqK Gn_sq2
    sbb Gn_sq2 #10  ; vert-1 horiz-2
    isCh1Knight
isChKn3
    lda Gn_H2
    cmp #1          ; B-H?
    jcc isChKn4
    lda Gn_V2
    cmp #2          ; 3-8?
    jcc isChKn4
    mva Gn_sqK Gn_sq2
    sbb Gn_sq2 #17  ; vert-2 horiz-1
    isCh1Knight
isChKn4
    lda Gn_H2
    cmp #7          ; A-G?
    jcs isChKn5
    lda Gn_V2
    cmp #2          ; 3-8?
    jcc isChKn5
    mva Gn_sqK Gn_sq2
    sbb Gn_sq2 #15  ; vert-2 horiz+1
    isCh1Knight
isChKn5
    lda Gn_H2
    cmp #6          ; A-F?
    jcs isChKn6
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc isChKn6
    mva Gn_sqK Gn_sq2
    sbb Gn_sq2 #6   ; vert-1 horiz+2
    isCh1Knight
isChKn6
    lda Gn_H2
    cmp #2          ; C-H?
    jcc isChKn7
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs isChKn7
    mva Gn_sqK Gn_sq2
    adb Gn_sq2 #6   ; vert+1 horiz-2
    isCh1Knight
isChKn7
    lda Gn_H2
    cmp #1          ; B-H?
    jcc isChKn8
    lda Gn_V2
    cmp #6          ; 1-6?
    jcs isChKn8
    mva Gn_sqK Gn_sq2
    adb Gn_sq2 #15  ; vert+2 horiz-1
    isCh1Knight
isChKn8

isChKnOk  
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  isCh1Knight - for particular knight
;-------------------------------------------------------
.PROC isCh1Knight
    ldy Gn_sq2
    lda Board,y
    ldy Gn_P
    cmp #'N'            ; if white knight?
    jeq isCh1KnBK
    cmp #'n'            ; if black knight?
    jeq isCh1KnWK
    jne isCh1KnOk    
isCh1KnBK
    cpy #'k'            ; then if black king?
    jne isCh1KnOk
    jeq isCh1KnCheck
isCh1KnWK
    cpy #'K'            ; then if white king?
    jne isCh1KnOk
isCh1KnCheck
    mva #1 Gn_Ck        ; CHECK+
isCh1KnOk   
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  isChBishops - is king under Bishop or Queen check?
; On return sets Gn_Ck value
;-------------------------------------------------------
.PROC isChBishops

    mva Gn_sqK Gn_sq2
isChBsLoop1    
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #7          ; A-G?
    jcs isChBs1
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs isChBs1
    adb Gn_sq2 #9  ; vert+1 horiz+1
    isCh1Bishop
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq isChBsLoop1     
isChBs1
    mva Gn_sqK Gn_sq2
isChBsLoop2
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #7          ; A-G?
    jcs isChBs2
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc isChBs2
    sbb Gn_sq2 #7  ; vert-1 horiz+1
    isCh1Bishop
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq isChBsLoop2    
isChBs2
    mva Gn_sqK Gn_sq2
isChBsLoop3
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #1          ; B-H?
    jcc isChBs3
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc isChBs3
    sbb Gn_sq2 #9  ; vert-1 horiz-1
    isCh1Bishop
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq isChBsLoop3    
isChBs3
    mva Gn_sqK Gn_sq2
isChBsLoop4
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #1          ; B-H?
    jcc isChBs4
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs isChBs4
    adb Gn_sq2 #7  ; vert+1 horiz-1
    isCh1Bishop
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq isChBsLoop4    
isChBs4
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  isCh1Bishop - for particular bishop or queen
;-------------------------------------------------------
.PROC isCh1Bishop
    ldy Gn_sq2
    lda Board,y
    ldy Gn_P
    cmp #'B'            ; if white bishop?
    jeq isCh1BsBK
    cmp #'Q'            ; if white queen?
    jeq isCh1BsBK
    cmp #'b'            ; if black bishop?
    jeq isCh1BsWK
    cmp #'q'            ; if black queen?
    jeq isCh1BsWK
    jne isCh1BsOk    
isCh1BsBK
    cpy #'k'            ; then if black king?
    jne isCh1BsOk
    jeq isCh1BsCheck
isCh1BsWK
    cpy #'K'            ; then if white king?
    jne isCh1BsOk
isCh1BsCheck
    mva #1 Gn_Ck        ; CHECK+
isCh1BsOk   
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  isChRooks - is king under Rook or Queen check?
; On return sets Gn_Ck value
;-------------------------------------------------------
.PROC isChRooks
    mva Gn_sqK Gn_sq2
isChRkLoop1    
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #7          ; A-G?
    jcs isChRk1
    inc Gn_sq2      ; horiz+1
    isCh1Rook
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq isChRkLoop1     
isChRk1
    mva Gn_sqK Gn_sq2
isChRkLoop2    
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #1          ; B-H?
    jcc isChRk2
    dec Gn_sq2      ; horiz-1
    isCh1Rook
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq isChRkLoop2     
isChRk2
    mva Gn_sqK Gn_sq2   
isChRkLoop3    
    GnSqHV2         ; update H,V
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc isChRk3
    sbb Gn_sq2 #8   ; vert-1
    isCh1Rook
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq isChRkLoop3     
isChRk3
    mva Gn_sqK Gn_sq2
isChRkLoop4    
    GnSqHV2         ; update H,V
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs isChRk4
    adb Gn_sq2 #8   ; vert+1
    isCh1Rook
    ldy Gn_sq2
    lda Board,y
    cmp #' '
    jeq isChRkLoop4     
isChRk4  
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  isCh1Rook - for particular rook or queen
;-------------------------------------------------------
.PROC isCh1Rook
    ldy Gn_sq2
    lda Board,y
    ldy Gn_P
    cmp #'R'            ; if white rook?
    jeq isCh1RkBK
    cmp #'Q'            ; if white queen?
    jeq isCh1RkBK
    cmp #'r'            ; if black rook?
    jeq isCh1RkWK
    cmp #'q'            ; if black queen?
    jeq isCh1RkWK
    jne isCh1RkOk    
isCh1RkBK
    cpy #'k'            ; then if black king?
    jne isCh1RkOk
    jeq isCh1RkCheck
isCh1RkWK
    cpy #'K'            ; then if white king?
    jne isCh1RkOk
isCh1RkCheck
    mva #1 Gn_Ck        ; CHECK+
isCh1RkOk   
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  isChKings - is king under other king check?
; On return sets Gn_Ck value
;-------------------------------------------------------
.PROC isChKings
    mva Gn_sqK Gn_sq2
    GnSqHV2         ; update H,V
    lda Gn_H2
    cmp #7          ; A-G?
    jcs isChKg1
    inc Gn_sq2      ; horiz+1, vert
    isCh1King
isChKg1
    mva Gn_sqK Gn_sq2 
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs isChKg2
    adb Gn_sq2 #8   ; horiz,vert+1
    isCh1King
isChKg2
    mva Gn_sqK Gn_sq2
    lda Gn_H2
    cmp #1          ; B-H?
    jcc isChKg3
    dec Gn_sq2      ; horiz-1,vert
    isCh1King
isChKg3
    mva Gn_sqK Gn_sq2
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc isChKg4
    sbb Gn_sq2 #8   ; horiz,vert-1
    isCh1King
    
 ;diognals
isChKg4
    mva Gn_sqK Gn_sq2
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs isChKg5
    lda Gn_H2
    cmp #7          ; A-G?
    jcs isChKg5
    adb Gn_sq2 #9   ; horiz+1,vert+1
    isCh1King
isChKg5
    mva Gn_sqK Gn_sq2
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc isChKg6
    lda Gn_H2
    cmp #7          ; A-G?
    jcs isChKg6
    sbb Gn_sq2 #7   ; horiz+1,vert-1
    isCh1King
isChKg6
    mva Gn_sqK Gn_sq2
    lda Gn_V2
    cmp #1          ; 2-8?
    jcc isChKg7
    lda Gn_H2
    cmp #1          ; B-H?
    jcc isChKg7
    sbb Gn_sq2 #9   ; horiz-1,vert-1
    isCh1King
isChKg7
    mva Gn_sqK Gn_sq2
    lda Gn_V2
    cmp #7          ; 1-7?
    jcs isChKg8
    lda Gn_H2
    cmp #1          ; B-H?
    jcc isChKg8
    adb Gn_sq2 #7   ; horiz-1,vert+1
    isCh1King
isChKg8 
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  isCh1King - for particular king
;-------------------------------------------------------
.PROC isCh1King
    ldy Gn_sq2
    lda Board,y
    ldy Gn_P
    cmp #'K'            ; if white king?
    jeq isCh1KgBK
    cmp #'k'            ; if black king?
    jeq isCh1KgWK
    jne isCh1KgOk    
isCh1KgBK
    cpy #'k'            ; then if black king?
    jne isCh1KgOk
    jeq isCh1KgCheck
isCh1KgWK
    cpy #'K'            ; then if white king?
    jne isCh1KgOk
isCh1KgCheck
    mva #1 Gn_Ck        ; CHECK+
isCh1KgOk   
    rts
.ENDP

;-------------------------------------------------------
; PROCEDURE  VerifyBeforeMove -
; Places piece to the new position and verifies king attacks.
; Uses Gn_sq, Gn_sq2, Gn_WKsq, Gn_BKsq
; It is faster than make complete piece movements and undo them.
;
; On return sets Gn_Ck value: 0-good move, 1- illegal
;-------------------------------------------------------

.PROC VerifyBeforeMove

    .var ep_sq2 .byte       ; temp. en-pass. captured pawn square
    .var ep_P   .byte       ; temp. en-pass. captured piece (pawn)
    
    mva #0 ep_P
    
    lda Gn_P
    pha             ; save Gn_P
    lda Gn_sq2
    pha             ; save Gn_sq2
    tay
    lda Board, y
    pha             ; save piece staying on "to-square"
    lda Gn_P
    sta Board, y    ; new piece will stay on "to-square"
    ldy Gn_sq
    lda #' '
    sta Board, y    ; "from-square" becomes empty
        
    lda ToMove
    jne vfBlackMove
; White king verification
    cpb Gn_WKsq Gn_sq   ; is white king position "from-square"?
    jeq vfWK1           ; then white king moves somewhere to new position, all data ready

    lda EPsq
    jeq Gn_WKskpEp
    cmp Gn_sq2     ; if en-passant then clear captured pawn
    jne Gn_WKskpEp
    cpb Gn_P #'P'
    jne Gn_WKskpEp
    
    mva #'p' ep_P
    mva Gn_sq2 ep_sq2
    sbb ep_sq2 #8
    ldy ep_sq2
    lda #' '
    sta Board, y        ; pawn square becomes empty
    
Gn_WKskpEp        
    mva Gn_WKsq Gn_sq2  ; white king stays
    mva #'K' Gn_P       ; for white king verify
    
vfWK1
    jmp vfIsCheck
                
vfBlackMove

; Black king verification
    cpb Gn_BKsq Gn_sq   ; is black king position "from-square"?
    jeq vfBK1           ; then black king moves somewhere to new position, all data ready

    lda EPsq
    jeq Gn_BKskpEp
    cmp Gn_sq2     ; if en-passant then clear captured pawn
    jne Gn_BKskpEp
    cpb Gn_P #'p'
    jne Gn_BKskpEp

    mva #'P' ep_P        
    mva Gn_sq2 ep_sq2
    adb ep_sq2 #8
    ldy ep_sq2
    lda #' '
    sta Board, y        ; pawn square becomes empty

Gn_BKskpEp                
    mva Gn_BKsq Gn_sq2  ; black king stays
    mva #'k' Gn_P       ; for black king verify
vfBK1

vfIsCheck

    isCheckAtSquare ; Verify CHECK+

    ; restore datas
    lda ep_P
    jeq Gn_EpRsskp
    
    ; restore pawn, that was captured by en-passing
    ldy ep_sq2
    sta Board, y    
    
Gn_EpRsskp  
    pla
    tax             ; x=piece
    pla
    sta Gn_sq2      ; restore Gn_sq2
    tay             ; y=Gn_sq2
    pla
    sta Gn_P        ; restore Gn_P
    
    txa
    sta Board, y    ; put back piece on "to_square"
    ldy Gn_sq
    lda Gn_P
    sta Board, y    ; put back piece on "from_square"
        
    rts    
.ENDP

;-----------------------------------------------------------------------
; PROCEDURE  MakeMove - Does movement
; Parameters: A = move sequence number

MkMode .by 0            ; 1 - redraw squares, 0 - silent mode         
;-----------------------------------------------------------------------

.PROC MakeMove

; variables
    .var iloop .byte    ; to loop moves
    
    .var f_sq .byte     ; from square
    .var t_sq .byte     ; to square
    .var kC .byte       ; to calc.sq.,diff.
    .var t_V .byte      ; vert. of to square
    .var pP .byte       ; piece promoted    
    .var Ex_sq .byte    ; square of pawn captured by en-passant move
    .var Rf_sq .byte    ; square of rook by castling move
    .var Rt_sq .byte    ; square of rook after castling move
        
    .var pc_f .byte     ; piece "from"
    .var pc_t .byte     ; captured piece on "to"
    .var m_sign .byte   ; move sign "-" or "x"
    .var FL .byte       ; flags to save
    .var sEPsq .byte    ; en-pass.info to save
            
    sta iloop
    
    ;5 bytes in history:
    ; 0-piece on "to square" before movement (' ' is empty square)
    ; 1-from square 0..63
    ; 2-to square 0..63
    ; 3-en-passant square before move
    ; 4-flags by bits possible castlings before move: &1-K, &2-Q, &4-k, &8-q
    ;       this move is promotion: bit &16 is set
    ;       this move is en-passant capture: bit &32 is set
    ;       this move is castling: bit &64 is set
    
    mwa #MovesGen_ADDR $C2   ; $C2,$C3 = generated moves list
    lda #0
    sta Ex_sq
    sta Rt_sq
    sta pP
    sta FL
    
    adb FL Castle+3         ; save first 4 bits
    aso FL
    adb FL Castle+2
    aso FL  
    adb FL Castle+1
    aso FL
    adb FL Castle+0
    mva EPsq sEPsq          ; save en-passant info
    
    lda iloop
    jeq MkMv0
MkMloopSq   
    adw $C2 #8          ; move address pointer to next move
    dec iloop           ; iloop--
    jne MkMloopSq

MkMv0        
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
    lda ($C2), y
    sta m_sign          ;  "-" or "x"    
    iny
    iny
    
    lda ($C2), y
    dey
    sta t_sq
    sbb t_sq #'1'
    mva t_sq t_V        ; t_V = Vert to verify promotion    
    aso t_sq
    aso t_sq
    aso t_sq            ; t_sq = Vert*8 (3 bit-shift to the left)
    lda ($C2), y
    sta kC
    sbb kC #'a'         ; kC = Horiz
    adb t_sq kC         ; t_sq +=kC
    
    ldy f_sq
    lda Board, y
    sta pc_f            ; pc_f = piece on "from" square
    lda #' '
    sta Board, y        ; clear square

    ldy t_sq
    lda Board, y
    sta pc_t            ; pc_t = captured piece on "to" square
    
    lda pc_f
    sta Board, y        ; "to" square = piece
    
    cpb m_sign #'-'
    jeq MkMvSkipEP
    
    lda pc_t
    cmp #' '            ; if "to" square is empty square and this is capturing then en-passant
    jne MkMvSkipEP
    
    mva t_sq Ex_sq
    adb FL #32    
    cpb pc_f #'P'
    jne MkMvEPblack
    sbb Ex_sq #8        ; to-8 for white move
    jmp MkMvEPprep
MkMvEPblack   
    adb Ex_sq #8        ; to+8 for black move
MkMvEPprep
    ldy Ex_sq
    lda #' '
    sta Board, y        ; clear square
    
MkMvSkipEP
    
    lda pc_f
    cmp #'P'            ; is this white pawn?
    jne MkMvNoWPw
    lda t_V
    cmp #7              ; promotion?
    jne MkMvWPnoProm
    mva Piece2promote pP    ; promoted piece on "to" square
    ldy t_sq
    sta Board, y
    adb FL #16
    jmp MkMvEpOver
MkMvWPnoProm    
    mva t_sq kC
    sbb kC f_sq         ; kC = t_sq - f_sq  for white pawn
    cmp #16
    jne MkMvSetNoEP     ; if diff=16 then e2-e4
    mva f_sq EPsq
    adb EPsq #8         ; New en-passant square e3
    jmp MkMvEpOver
MkMvNoWPw
    cmp #'p'            ; is this black pawn?
    jne MkMvSetNoEP
    lda t_V
    ;cmp #0
    jne MkMvBPnoProm    ; promotion?
    mva Piece2promote pP    ; promoted piece on "to" square
    adb pP #32          ; to lowercase, to A
    ldy t_sq
    sta Board, y
    adb FL #16
    jmp MkMvEpOver
MkMvBPnoProm    
    mva f_sq kC
    sbb kC t_sq         ; kC = f_sq - t_sq  for black pawn
    cmp #16
    jne MkMvSetNoEP     ; if diff=16 then e7-e5
    mva t_sq EPsq
    adb EPsq #8         ; New en-passant square e6
    jmp MkMvEpOver
MkMvSetNoEP
    mva #0 EPsq         ; NO en-passant square  
MkMvEpOver

    lda pc_f
    cmp #'K'            ; is this white king?
    jne MkMvNoWK
    lda f_sq            ; possibly white king castling move
    cmp #4
    jne MkMvOKcastle

    lda #1              ; no more castling for white king              
    sta Castle+0        ; K-castling
    sta Castle+1        ; Q-castling
    
    lda t_sq
    cmp #6
    jne MkMvNoWKk
    ; White king 0-0
    ldy #7
    sty Rf_sq
    lda #' '
    sta Board, y        ; empty h1
    ldy #5
    sty Rt_sq
    lda #'R'
    sta Board, y        ; white rook on f1
    adb FL #64
    jmp MkMvOKcastle
MkMvNoWKk
    cmp #2
    jne MkMvOKcastle
    ; White king 0-0-0
    ldy #0
    sty Rf_sq
    lda #' '
    sta Board, y        ; empty a1
    ldy #3
    sty Rt_sq
    lda #'R'
    sta Board, y        ; white rook on d1
    adb FL #64
    jmp MkMvOKcastle
MkMvNoWK
    cmp #'k'            ; is this black king?
    jne MkMvOKcastle

    lda f_sq            ; possibly black king castling move
    cmp #60
    jne MkMvOKcastle
    
    lda #1              ; no more castling for black king              
    sta Castle+2        ; k-castling
    sta Castle+3        ; q-castling    
    
    lda t_sq
    cmp #62
    jne MkMvNoBKk
    ; Black king 0-0
    ldy #63
    sty Rf_sq
    lda #' '
    sta Board, y        ; empty h8
    ldy #61
    sty Rt_sq
    lda #'r'
    sta Board, y        ; black rook on f8
    adb FL #64
    jmp MkMvOKcastle
MkMvNoBKk
    cmp #58
    jne MkMvOKcastle
    ; Black king 0-0-0
    ldy #56
    sty Rf_sq
    lda #' '
    sta Board, y        ; empty a8
    ldy #59
    sty Rt_sq
    lda #'r'
    sta Board, y        ; black rook on d8
    adb FL #64
MkMvOKcastle

    lda #1              ; set castling flags when rooks are moving or captured
    ldx f_sq
    ldy t_sq
    ; for white rooks
    cpx #0              ; Ra1 moving?
    jeq MkMvWRQ
    cpy #0              ; Ra1 captured?
    jeq MkMvWRQ
    jne MkMvWR2
MkMvWRQ
    sta Castle+1        ; Q-castling
    jmp MkMvOKcRooks
MkMvWR2
    cpx #7              ; Rh1 moving?
    jeq MkMvWRK
    cpy #7              ; Rh1 captured?
    jeq MkMvWRK
    jne MkMv4blackRook
MkMvWRK
    sta Castle+0        ; K-castling
    jmp MkMvOKcRooks
MkMv4blackRook
    ; for black rooks
    cpx #56             ; Ra8 moving?
    jeq MkMvBRQ
    cpy #56             ; Ra8 captured?
    jeq MkMvBRQ
    jne MkMvBR2
MkMvBRQ
    sta Castle+3        ; q-castling
    jmp MkMvOKcRooks
MkMvBR2
    cpx #63             ; Rh8 moving?
    jeq MkMvBRK
    cpy #63             ; Rh1 captured?
    jeq MkMvBRK
    jne MkMvOKcRooks
MkMvBRK
    sta Castle+2        ; k-castling
MkMvOKcRooks

    lda ToMove      ; inverts ToMove = 1-ToMove (0-white, 1-black)
    jne MkMvOp
    inc ToMove
    jne MkMvOz
MkMvOp
    inc MoveNumber
    dec ToMove
MkMvOz

    lda MkMode
    jeq MkMvSkipDraw
    
; redraw squares on board
    mva f_sq Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw "from" square
    
    mva t_sq Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw "to" square
    
    lda Ex_sq
    jeq MkMvSkipEPdraw
    sta Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw square of pawn captured by en-passant move
    
MkMvSkipEPdraw
    lda Rt_sq
    jeq MkMvSkipCastledraw
    mva Rf_sq Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw square of rook before castling
    mva Rt_sq Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw square of rook after castling
    
MkMvSkipCastledraw

    PutToMove
MkMvSkipDraw

    adw MVhistLastAddr #5       ; save new move to history
    mwa MVhistLastAddr $C2      ; $C2,$C3 = address to put move
    ldy #0
    lda pc_t
    sta ($C2), y    ; save piece on "to square"
    iny
    lda f_sq
    sta ($C2), y    ; save from square
    iny
    lda t_sq
    sta ($C2), y    ; save to square
    iny
    lda sEPsq
    sta ($C2), y    ; save to en-passant info
    iny
    lda FL
    sta ($C2), y    ; save flags (castling,promotion)
    rts    
.ENDP

;-----------------------------------------------------------------------
; PROCEDURE  UnMakeMove - Does undo-movement of last move
;    uses MkMode flag
;-----------------------------------------------------------------------

.PROC UnMakeMove

; variables
    .var pc_t .byte     ; piece on "to square"
    .var pc_f .byte     ; piece on "from square"
    .var f_sq .byte     ; from square
    .var t_sq .byte     ; to square    
    .var FL .byte       ; flags
    .var fep .byte      ; flag - is this en-passant move
    .var fpr .byte      ; flag - is this promotion
    .var fcs .byte      ; flag - is this castling    
    .var Ex_sq .byte    ; square of pawn captured by en-passant move
    .var Rf_sq .byte    ; square of rook by castling move
    .var Rt_sq .byte    ; square of rook after castling move
    
    lda #0
    sta Ex_sq
    sta Rt_sq
        
    mwa MVhistLastAddr $C2      ; $C2,$C3 = address of last move
    ldy #0
    lda ($C2), y    ; get piece on "to square"
    jeq UnMkOver    ; is the list empty?
    iny
    sta pc_t
    lda ($C2), y    ; get from square
    iny
    sta f_sq
    lda ($C2), y    ; get to square
    iny
    sta t_sq
    lda ($C2), y    ; get en-passant square
    iny
    sta EPsq
    lda ($C2), y    ; get other flags
    sta FL
    
    ldx #1
    sax Castle+0    ; restore K-castling before move
    alr #$ff
    sax Castle+1    ; restore Q-castling before move
    alr #$ff
    sax Castle+2    ; restore k-castling before move
    alr #$ff
    sax Castle+3    ; restore q-castling before move
    alr #$ff
    
    sax fpr         ; promotion flag
    alr #$ff
    sax fep         ; en-passant flag
    alr #$ff
    sax fcs         ; castling flag
    
    sbw MVhistLastAddr #5
    
            
    lda fep
    jeq UnMkNoEP
    
    ; this was en-passant capture
    mva t_sq Ex_sq
    cmp #20
    jcs UnMkBep
    adb Ex_sq #8
    lda #'P'        ; put back white pawn
    jmp UnMkEp2
UnMkBep
    sbb Ex_sq #8
    lda #'p'        ; put back black pawn
UnMkEp2
    ldy Ex_sq
    sta Board,y     ; restore pawn, captured by en-passant move
UnMkNoEP

    lda fpr
    jeq UnMkNoProm
    lda t_sq
    cmp #20
    jcc UnMkBprom
    lda #'P'        ; back to white pawn
    jmp UnMkProm2
UnMkBprom
    lda #'p'        ; back to black pawn
UnMkProm2
    ldy t_sq
    sta Board,y     ; this piece becomes a pawn
    mva #'Q' Piece2promote  ; set default promotion to queen (may be wrong piece)
UnMkNoProm

    lda fcs
    jeq UnMkNoCst
    lda t_sq
    cmp #20
    jcs UnMkBcst
    cmp #6          ; is white king 0-0
    jne UnMkcstWQ
    mva #5 Rt_sq
    mva #7 Rf_sq    ; rook from f1 must return to h1
    jmp UnMkCstOk
UnMkcstWQ           ; white king 0-0-0
    mva #3 Rt_sq
    mva #0 Rf_sq    ; rook from d1 must return to a1
    jmp UnMkCstOk
UnMkBcst
    cmp #62         ; is black king 0-0
    jne UnMkcstBQ
    mva #61 Rt_sq
    mva #63 Rf_sq   ; rook from f8 must return to h8
    jmp UnMkCstOk
UnMkcstBQ           ; white king 0-0-0
    mva #59 Rt_sq
    mva #56 Rf_sq   ; rook from d8 must return to a8
    jmp UnMkCstOk

UnMkCstOk
    ldy Rt_sq
    lda Board,y
    pha
    lda #' '
    sta Board,y     ; clear rook square
    pla
    ldy Rf_sq
    sta Board,y     ; put rook back before castling move was done
    
UnMkNoCst

    ldy t_sq
    lda Board,y
    sta pc_f
    lda pc_t
    sta Board,y     ; piece back on to square
    ldy f_sq
    lda pc_f
    sta Board,y     ; piece back on from square    
    
    lda ToMove      ; inverts ToMove = 1-ToMove (0-white, 1-black)
    jne UnMkMvOp
    dec MoveNumber
    inc ToMove
    jne UnMkMvOz
UnMkMvOp
    dec ToMove
UnMkMvOz

    lda MkMode
    jeq UnMkMvSkipDraw
    
; redraw squares on board

    lda Rt_sq
    jeq UnMkMvSkipCastledraw
    mva Rt_sq Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw square of rook after castling
    mva Rf_sq Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw square of rook before castling
    
UnMkMvSkipCastledraw

    mva t_sq Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw "to" square
    
    mva f_sq Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw "from" square
    
    lda Ex_sq
    jeq UnMkMvSkipEPdraw
    sta Gn_sq2
    GnSqHV2         ; update H,V
    ldx Gn_H2
    ldy Gn_V2
    PrintSq         ; redraw square of pawn captured by en-passant move
    
UnMkMvSkipEPdraw

    PutToMove
UnMkMvSkipDraw

UnMkOver
    rts    
.ENDP
