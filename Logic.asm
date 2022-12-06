EXTRN Init:FAR
EXTRN DrawSquare:FAR
EXTRN DrawPiece:FAR
EXTRN DrawBoard:FAR
EXTRN RedrawBoardSq:FAR
Public chessBoard
include Macro.inc
.286
.Model Small
.Stack 64
.Data


chessBoard  db "B0 B1 B2 B3 B4 B2 B1 B0 "
            db "B5 B5 B5 B5 B5 B5 B5 B5 "
            db "00 00 00 00 00 00 00 00 "
            db "00 00 00 00 00 00 00 00 "
            db "00 00 00 00 00 00 00 00 "
            db "00 00 00 00 00 00 00 00 "
            db "W5 W5 W5 W5 W5 W5 W5 W5 "
            db "W0 W1 W2 W3 W4 W2 W1 W0 "
selPos label byte
px  db 1
py  db 8
sq db 20 dup('$')
.Code


MAIN PROC FAR
    MOV AX , @DATA
    MOV DS , AX
    
    
    call Init
    DrawSq px, py
    MOV ch,px
    MOV cl,py
GameLP:

    mov ah,0
    int 16h
    cmp al,'e'
    jz ending
    cmp al,'d'
    je Right
    cmp al,'a'
    je Left
    cmp al,'w'
    je up
    cmp al,'s'
    je down
    
    Right:
    cmp px,8
    je GameLP
        MOV ch,px
    MOV cl,py
    add px,1
    jmp lp
    Left:
    cmp px,1
    je GameLP
        MOV ch,px
    MOV cl,py
    sub px,1
    jmp lp
    up:
    cmp py,1
    je GameLP
        MOV ch,px
    MOV cl,py
    sub py,1
    jmp lp
    down:
    cmp py,8
    je GameLP
        MOV ch,px
    MOV cl,py
    add py,1
   lp:  
    ;call Init
    call RedrawBoardSq
    DrawSq px, py
    jmp GameLP
    
    ending:
    
    ; Press any key to exit
    MOV AH , 0
    INT 16h
    
    
    ;Change to Text MODE
    MOV AH,0          
    MOV AL,03h
    INT 10h 

    ; return control to operating system
    MOV AH , 4ch
    INT 21H
hlt 
Main ENDP


END MAIN