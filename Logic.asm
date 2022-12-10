EXTRN Init:FAR
EXTRN DrawSquare:FAR
EXTRN DrawPiece:FAR
EXTRN DrawPieces:FAR
EXTRN DrawBoard:FAR
EXTRN RedrawBoardSq:FAR
EXTRN RedrawPiece:FAR
EXTRN DrawPossibleMoves:FAR
EXTRN DrawPossibleAttacks:FAR
;EXTRN Moves_bishop:FAR
Public to_idx
Public chessBoard
Public ValidMoves
Public ValidAttacks
include Macro.inc
.286
.Model small
.Stack 100h
.Data


chessBoard  db "B0","B1","B2","B3","B4","B2","B1","B0"
            db "B5","B5","B5","B5","B5","B5","B5","B5"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "W5","W5","W5","W5","W5","W5","W5","W5"
            db "W0","W1","W2","W3","W4","W2","W1","W0"

ValidMoves db 20 dup('$$'), '$'  ; assuming that the max no. of possible moves for 1 piece is 20 
                            ; idk the correct number
ValidAttacks db 20 dup('$$'), '$' 

higlight_Pos label byte
px  db 1
py  db 8
selPos label byte ; current selected piece position
hx  db 0
hy  db 0
sq db 20 dup('$')
.Code


MAIN PROC FAR
    MOV AX , @DATA
    MOV DS , AX
    
    
    call Init
    DrawSq px, py
    MOV ch,px
    MOV cl,py
    call RedrawPiece
GameLP:

    mov ah,0
    int 16h    
    ;select key
    cmp al,'q'
    jne G_L_move_mid
    


    lea di, ValidMoves
    mov al, '$'
    G_Lp_ClearMoves:
        cmp [di],al
        je done

        mov ch, [di]
        mov cl, [di+1]
        call RedrawBoardSq

        mov [di], al
        mov [di+1], al

        add di,2
        jmp G_Lp_ClearMoves

    done: 

    lea di, ValidAttacks
    mov al, '$'
    G_Lp_ClearAttacks:
        cmp [di],al
        je done2

        mov ch, [di]
        mov cl, [di+1]
        call RedrawBoardSq
        call RedrawPiece

        mov [di], al
        mov [di+1], al

        add di,2
        jmp G_Lp_ClearAttacks

    done2: 

    mov ch,px
    mov cl,py
    call to_idx
    mov bl,'0'
    cmp [di], bl
    jne select_piece


    ; mov dl,'1'
    ; mov ah,2
    ; int 21h
    ; pressed Q on empty square. if a piece is already selected move piece to that square
    ; if hx == 0 || hy == 0 then no piece selected
    cmp hx,0
    je GameLP

    ;=============
    jmp G_L_skip_move_mid
    G_L_move_mid:
    jmp move
    G_L_skip_move_mid:
    ;=============

    ; if there was some piece previously selected move it 
    mov bx,di ; bx is the empty square position in chessboard array 
    mov ch, hx
    mov cl, hy
    call RedrawBoardSq
    call to_idx ; di is the previously selected piece's position in chessboard array
    mov cl, [di]
    mov [bx], cl
    mov ah, '0'
    mov [di], ah
    inc di
    inc bx
    mov cl, [di]
    mov [bx], cl
    mov [di], ah  
    

 ;call ClearValidMoves
    ; lea si, ValidMoves
    ; mov al, '$'
    ; ; does weird stuff when used as a proc
    ; C_V_M_remove_excess1:
    ;     cmp [si],al
    ;     je C_V_M_move1

    ;     mov ch, [si]
    ;     mov cl, [si+1]
    ;     call RedrawBoardSq
    ;     ;call RedrawPiece

    ;     mov [si], al
    ;     mov [si + 1], al
    ;     add si , 2
    ;     jmp C_V_M_remove_excess1
; C_V_M_move1:

    mov ch, px
    mov cl, py
    DrawSq px, py
    call RedrawPiece

    mov hx,0
    mov hy,0
    

   

    jmp GameLP

    ; no piece was previously selected so select current piece
    ; or another piece is selected
    select_piece:
    ; mov dl,'2'
    ; mov ah,2
    ; int 21h
    ; remove prev highlighted piece background
    mov ch, hx
    mov cl, hy
    call RedrawBoardSq
    call RedrawPiece
    
    ;call ClearValidMoves
    ; lea si, ValidMoves
    ; mov al, '$'
    ; does weird stuff when used as a proc
    ; C_V_M_remove_excess:
    ;     cmp [si],al
    ;     je C_V_M_move


    ;     mov ch, [si]
    ;     mov cl, [si+1]
    ;     call RedrawBoardSq
    ;     ;call RedrawPiece


    ;     mov [si], al
    ;     mov [si + 1], al
    ;     add si , 2
    ;     jmp C_V_M_remove_excess
    ; C_V_M_move:

    mov ch, px
    mov cl, py

    mov hx, ch
    mov hy, cl

  
    call GetValidMoves
    call DrawPossibleMoves
    call DrawPossibleAttacks


    jmp GameLP
GameLPmid:
jmp GameLP

move:
    ;movement keys
    MOV ch,px
    MOV cl,py
    
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
    je GameLPmid
    add px,1
    jmp lp

    ending_mid:
    jmp ending

    Left:
    cmp px,1
    je GameLPmid
    sub px,1
    jmp lp
    up:
    cmp py,1
    je GameLPmid
    sub py,1
    jmp lp
    down:
    cmp py,8
    je GameLPmid
    add py,1
   lp:  
    ;call Init
    call RedrawBoardSq
    ;call DrawBoard
    ;call DrawPieces
    call DrawPossibleMoves
    call DrawPossibleAttacks
    DrawSq px, py
    call RedrawPiece

    cmp hx,0
    je skip
    ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
    DrawSq hx, hy
    mov ch, hx
    mov cl, hy
    call RedrawPiece
    skip:
    mov ch,px
    mov cl,py
    call RedrawPiece
    jmp GameLPmid
    
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


;populates the valid moves array with positions x:y (row:col not px) the selected piece can move to
; need position in (CX) x:y => ch:cl
GetValidMoves PROC
    pusha

    ; mov dl,'3'
    ; mov ah,2
    ; int 21h


    call to_idx
    lea si, ValidMoves
    mov bl, '0'
    mov dx,0
    cmp [di+1],bl
    je pr
    inc bl
    cmp [di+1],bl
    je pkt
    inc bl
    cmp [di+1],bl
    je pb
    inc bl
    cmp [di+1],bl
    je pk
    inc bl
    cmp [di+1], bl
    je pq
    inc bl
    cmp [di+1],bl
    je ppw

    pr:         ;possible moves for rook
    ; mov dl,'4'
    ; mov ah,2
    ; int 21h
    ;mov dx,0
    call Moves_rook
    popa
    ret
    pkt:        ;possible moves for knight
    ;call Moves_rook
    popa
    ret
    pb:         ;possible moves for bishop
 ;   mov dx,0
    call Moves_bishop
    popa
    ret
    pk:         ;possible moves for king

    popa
    ret
    pq:         ;possible moves for queen
  ;  mov dx,0
    call Moves_rook
    call Moves_bishop
    popa
    ret
    ppw:        ;possible moves for pawn

    popa
    ret
GetValidMoves ENDP

;DI has idx
Moves_rook PROC ; lea si with valid moves array
                ; lea dx with offset in Validattack
    ;pusha
    ; mov dl,'5'
    ; mov ah,2
    ; int 21h


    ;mov dx,0
    mov bx,di
    add bx,2
    mov al,'0'
    mov ah, ch
   ; lea si,ValidMoves ;=========>> el index bta3y sabt ba7arako kol ma ala2y valid square
    M_R_Check_Row_right:
        inc ah
        cmp ah, 9
        je M_R_Next1
        cmp [bx], al
        jne M_R_Found_piece1
        mov [si], ah
        mov [si+1], cl
        add si,2
        add bx,2
        jmp M_R_Check_Row_right

    M_R_Found_piece1:
        mov al, [di]
        cmp [bx], al
        je M_R_Next1
        lea bx,ValidAttacks
        add bx,dx
        mov [bx], ah
        mov [bx+1], cl
        add dx,2
        ;push bx ;============= 1 push 

    M_R_Next1:
    ;initializations
    mov bx,di
    sub bx, 2
    mov al,'0'
    mov ah, ch
    M_R_Check_Row_left:
        dec ah
        cmp ah, 0
        je M_R_Next2
        cmp [bx], al
        jne M_R_Found_piece2
        mov [si], ah
        mov [si+1], cl
        add si,2
        sub bx,2
        jmp M_R_Check_Row_left

    M_R_Found_piece2:
        mov al, [di]
        cmp [bx], al
        je M_R_Next2 ;move rook end
        ;pop bx ;=========================== 1 pop
        lea bx,ValidAttacks
        add bx,dx
        mov [bx], ah
        mov [bx+1], cl
        add dx,2
     ;   push bx ;======================== 1 push
         



    M_R_Next2:
    ;initializations
    mov bx,di
    sub bx, 16
    mov al,'0'
    mov ah, cl
    M_R_Check_Col_Up:
        dec ah
        cmp ah, 0
        je M_R_Next3
        cmp [bx], al
        jne M_R_Found_piece3
        mov [si], ch
        mov [si+1], ah 
        add si,2
        sub bx,16
        jmp M_R_Check_Col_Up

    M_R_Found_piece3:
        mov al, [di]
        cmp [bx], al
        je M_R_Next3 ;move rook end
        ;pop bx ;=========================== 1 pop
        lea bx,ValidAttacks
        add bx,dx
        mov [bx], ch
        mov [bx+1], ah
        add dx,2
        ;push bx ;======================== 1 push

    M_R_Next3:
    ;initializations
    mov bx,di
    add bx, 16
    mov al,'0'
    mov ah, cl
    M_R_Check_Col_Down:
        inc ah
        cmp ah, 9
        je M_R_Finalize
        cmp [bx], al
        jne M_R_Found_piece4
        mov [si], ch
        mov [si+1], ah 
        add si,2
        add bx,16
        jmp M_R_Check_Col_Down

    M_R_Found_piece4:
        mov al, [di]
        cmp [bx], al
        je M_R_Finalize ;move rook end
        ;pop bx ;=========================== 1 pop
        lea bx,ValidAttacks
        add bx,dx
        mov [bx], ch
        mov [bx+1], ah
        add dx,2
        ;push bx ;======================== 1 push

    M_R_Finalize:
    ; lea bx, ValidMoves
    ; add bx, 40 ; last memory loction in Validmoves array
    ; mov al, '$'
    ; M_R_remove_excess:
    ;     cmp si,bx
    ;     je M_R_end
    ;     cmp [si],al
    ;     je M_R_end
    ;     mov [si], al
    ;     inc si
    ;     jmp M_R_remove_excess
    ; M_R_end:
    ;popa
    ret
Moves_rook ENDP

;DI has idx
Moves_bishop PROC ; load si with valid moves
                  ; lea dx with offset in Validattack
    ;pusha
    ; mov dl,'6'
    ; mov ah,2
    ; int 21h

;    mov dl,0
    mov bx,di
    sub bx, 14
    mov dh,'0'
    mov ax, cx
    ;lea si,ValidMoves ;=========>> el index bta3y sabt ba7arako kol ma ala2y valid square
    M_B_Check_Diag_right_up:
        inc ah
        dec al
        cmp ah, 9
        je M_B_Next1
        cmp al, 0
        je M_B_Next1
        cmp [bx], dh
        jne M_B_Found_piece1
        mov [si], ah
        mov [si+1], al
        add si,2
        sub bx, 14 ; 34an ytl3 row ela square
        jmp M_B_Check_Diag_right_up

    M_B_Found_piece1:
        mov dh, [di]
        cmp [bx], dh
        je M_B_Next1
        lea bx,ValidAttacks
        mov dh,0
        add bx,dx
        mov [bx], ah
        mov [bx+1], al
        add dl,2
       ; push bx ;============= 1 push 

    M_B_Next1:
    ;initializations
    mov bx,di
    sub bx, 18
    mov dh,'0'
    mov ax, cx
    M_B_Check_Diag_left_up:
        dec ah
        dec al
        cmp ah, 0
        je M_B_Next2
        cmp al, 0
        je M_B_Next2
        cmp [bx], dh
        jne M_B_Found_piece2
        mov [si], ah
        mov [si+1], al
        add si,2
        sub bx, 18 ; 34an ytl3 row + square kman
        jmp M_B_Check_Diag_left_up

    M_B_Found_piece2:
        mov dh, [di]
        cmp [bx], dh
        je M_B_Next2 ;move rook end
        ;pop bx ;=========================== 1 pop
        lea bx,ValidAttacks
        mov dh,0
        add bx,dx
        mov [bx], ah
        mov [bx+1], al
        add dl,2
        ;push bx ;======================== 1 push
         



    M_B_Next2:
    ;initializations
    mov bx,di
    ADD bx, 18
    mov dh,'0'
    mov ax, cx
    M_B_Check_Diag_right_down:
        inc ah
        inc al
        cmp ah, 9
        je M_B_Next3
        cmp al, 9
        je M_B_Next3
        cmp [bx], dh
        jne M_B_Found_piece3
        mov [si], ah
        mov [si+1], al 
        add si,2
        add bx, 18 ; ynzl row + square
        jmp M_B_Check_Diag_right_down

    M_B_Found_piece3:
        mov dh, [di]
        cmp [bx], dh
        je M_B_Next3 ;move rook end
        ;pop bx ;=========================== 1 pop
        lea bx,ValidAttacks
        mov dh,0
        add bx,dx
        mov [bx], ah
        mov [bx+1], al
        add dl,2
        ;push bx ;======================== 1 push

    M_B_Next3:
    ;initializations
    mov bx,di
    add bx, 14
    mov dh,'0'
    mov ax, cx
    M_B_Check_Diag_left_down:
        dec ah
        inc al
        cmp ah, 0
        je M_B_Finalize
        cmp al, 9
        je M_B_Finalize
        cmp [bx], dh
        jne M_B_Found_piece4
        mov [si], ah
        mov [si+1], al 
        add si,2
        add bx, 14 ; row ela square
        jmp M_B_Check_Diag_left_down

    M_B_Found_piece4:
        mov dh, [di]
        cmp [bx], dh
        je M_B_Finalize ;move rook end
        ;pop bx ;=========================== 1 pop
        lea bx,ValidAttacks
        mov dh,0
        add bx,dx
        mov [bx], ah
        mov [bx+1], al
        add dl,2
        ;push bx ;======================== 1 push

     M_B_Finalize:
    ; lea bx, ValidMoves
    ; add bx, 40 ; last memory loction in Validmoves array
    ; mov al, '$'
    ; M_B_remove_excess:
    ;     cmp si,bx
    ;     je M_B_end
    ;     cmp [si],al
    ;     je M_B_end
    ;     mov [si], al
    ;     inc si
    ;     jmp M_B_remove_excess
    ; M_B_end:
    ;popa
    ret
Moves_bishop ENDP


;description
ClearValidMoves PROC
    pusha


    popa
    ret
ClearValidMoves ENDP
END MAIN