EXTRN Init:FAR
EXTRN DrawSquare:FAR
EXTRN DrawPiece:FAR
EXTRN DrawPieces:FAR
EXTRN DrawBoard:FAR
EXTRN RedrawBoardSq:FAR
EXTRN RedrawPiece:FAR
Public to_idx
Public chessBoard
include Macro.inc
.286
.Model Small
.Stack 64
.Data


chessBoard  db "B0","B1","B2","B3","B4","B2","B1","B0"
            db "B5","B5","B5","B5","B5","B5","B5","B5"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "W5","W5","W5","W5","W5","W5","W5","W5"
            db "W0","W1","W2","W3","W4","W2","W1","W0"
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
    ;select key
    ;cmp al,'q'


    ;movement keys
    cmp al,'e'
    jz ending_mid
    cmp al,'d'
    je Right
    cmp al,'a'
    je Left
    cmp al,'w'
    je up
    cmp al,'s'
    je down
    jmp GameLP
    Right:
    cmp px,8
    je GameLP
    MOV ch,px
    MOV cl,py
    add px,1
    jmp lp

    ending_mid:
    jmp ending

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
    ;call DrawBoard
    ;call DrawPieces
    DrawSq px, py
    call RedrawPiece
    mov ch,px
    mov cl,py
    call RedrawPiece
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

;Gets start idx of Position (CX) and puts it in DI
to_idx PROC
    push ax
    push cx
    push bx

    dec cl
    dec ch
    Lea di, chessBoard
    mov al,cl
    mov ah,0
    mov bl,16d
    mul bl
    add di, ax

    mov al,ch
    mov ah,0
    mov bl,2d
    mul bl
    add di,ax

    pop bx
    pop cx
    pop ax
    ret
to_idx ENDP

;description
to_px PROC  ; x:y => bh:bl
            ; return in x:y bh:bl
pusha
    mov AH,0
    ;mov BH,0
    mov al,bl

    mov Cl, 20D
    mul Cl
    mov bl, Al
    mov Al,Bh
    mov ah,0
    mov Cl, 20D
    mul Cl
    mov bh,al
popa
ret
to_px ENDP

END MAIN