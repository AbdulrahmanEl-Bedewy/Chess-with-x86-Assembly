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


;     mov AH,0
;     mov al,cl

;     mov Si, 20D
;     mul Si
;     mov Si, 198D
;     mul Si
;     add bx,ax

;     mov Al,ch
;     mov ah,0
;     mov cl, 20D
;     mul cl
;     add bx,ax
; ;==============
;     pop cx
;     mov al,cl
;     mov bl,ch

;     mov Cx, 20D
;     mul Cx
;     mov Cx, 320D
;     mul Cx
;     mov Dx, Ax
    
;     mov Ax,Bx
;     mov Cx, 20D
;     mul Cx
;     add Cx, AX
;     add Cx , 60