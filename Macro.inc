LoadImage MACRO name, size, array
    LEA DX, name
    CALL OpenFile
    mov Cx, size * size
    LEA DX, array
    CALL ReadData	
    call CloseFile
ENDM LoadImage

DrawP MACRO Dataarray , x, y ; draws 1 chess piece
    LEA BX , Dataarray ; BL contains index at the current drawn pixel	
    MOV ch,x
    MOV cl,y
    call far ptr DrawPiece
ENDM

ReDrawSq MACRO x, y ; draws 1 chess piece
    MOV ch,x
    MOV cl,y
    call RedrawBoardSq
ENDM

DrawSq MACRO x, y  ; draws 1 square of solid color at specified coordinates 
                   ; x & y are row and column. not in terms of px
    mov bl,x
    mov al,y
    mov dl, CursorColor;68h;3h
    call DrawSquare
ENDM

DrawSq2 MACRO x, y  ; draws 1 square of solid color at specified coordinates 
                   ; x & y are row and column. not in terms of px
    mov bl,x
    mov al,y
    mov dl, 2Ah ;8Ah
    call DrawSquare
ENDM




DeselectPlayer1 MACRO
            ;===============
            ;General deselects piece in hx:hy
            mov ch, hx
            mov cl, hy
            call RedrawBoardSq
            call RedrawPiece
            mov hx,0
            mov hy,0
            ;===============
ENDM


DeselectPlayer2 MACRO params
            ;===============
            ;General deselects piece in hx:hy
            mov ch, hx2
            mov cl, hy2
            call RedrawBoardSq
            call RedrawPiece
            mov hx2,0
            mov hy2,0
            ;===============
ENDM
