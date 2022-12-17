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
    call DrawPiece
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
    mov dl, 3h
    call DrawSquare
ENDM

ClearValidMoves MACRO 
    Local Clear1
    Local Cleared1
    lea si,ValidMoves
    mov al,'$'
    Clear1:
        cmp [si], al
        je Cleared1
        mov ch, [si]
        mov cl, [si+1]
        call RedrawBoardSq
        ;  call RedrawPiece
        mov [si], al
        mov [si+1], al
        add si,2
        jmp Clear1
    Cleared1:
    DrawSq px,py
    mov ch,px
    mov cl,py
    call RedrawPiece
ENDM

ClearValidAttacks MACRO 
    Local Clear2
    Local Cleared2
    lea si, ValidAttacks
    mov al,'$'
    Clear2:
        cmp [si], al
        je Cleared2
        mov ch, [si]
        mov cl, [si+1]
        call RedrawBoardSq
        call RedrawPiece
        mov [si], al
        mov [si+1], al
        add si,2
        jmp Clear2
    Cleared2:
    DrawSq px,py
    mov ch,px
    mov cl,py
    call RedrawPiece
ENDM
