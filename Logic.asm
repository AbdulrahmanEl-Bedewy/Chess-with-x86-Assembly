EXTRN Init:FAR
EXTRN DrawSquare:FAR
EXTRN DrawPiece:FAR
EXTRN DrawPieces:FAR
EXTRN DrawBoard:FAR
EXTRN RedrawBoardSq:FAR
EXTRN RedrawPiece:FAR
EXTRN DrawPossibleMoves:FAR
EXTRN DrawPossibleAttacks:FAR
EXTRN DrawDeadP:FAR
;EXTRN Moves_bishop:FAR
Public to_idx
Public chessBoard
Public ValidMoves
Public ValidAttacks
Public B_DeadPiece
Public W_DeadPiece
include Macro.inc
.286
.Model small
.Stack 200h
.Data


chessBoard  db "B0","B1","B2","B3","B4","B2","B1","B0"
            db "B5","B5","B5","B5","B5","B5","B5","B5"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "00","00","00","00","00","00","00","00"
            db "W5","W5","W5","W5","W5","W5","W5","W5"
            db "W0","W1","W2","W3","W4","W2","W1","W0"

ValidMoves db 32 dup('$$'), '$'  ; assuming that the max no. of possible moves for 1 piece is 32 
                            ; idk the correct number
ValidAttacks db 8 dup('$$'), '$' 

;To display dead pieces
B_DeadPiece db 16 dup('$$'),'$'
W_DeadPiece db 16 dup('$$'),'$'

higlight_Pos label byte
px  db 1
py  db 8
selPos label byte ; current selected piece position
hx  db 0
hy  db 0
sq db 20 dup('$')

IsKing db ?
.Code


MAIN PROC FAR
    MOV AX , @DATA
    MOV DS , AX
    
    ;Initialize pieces to locations
    ;Draw highlighted square default
    call Init
    DrawSq px, py
    MOV ch,px
    MOV cl,py
    call RedrawPiece
;======================================================================================
;MAIN GAME LOOP

GameLP:
    mov ch,px
    mov cl,py
    mov ah,1
    int 16h   
    jz GameLP

    mov ah,0
    int 16h   
     
    cmp al, 'e'
    je ending

    ;Check if select key pressed
    call HandleInput
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


HandleInput PROC    ; the user input is in ax => al:ascii ah:scan code
                    ;Handles all game logic
                    ;Including:
                    ;   1-Check for input
                    ;   2-Move piece
                    ;   3-Getting available moves
    MOV ch,px
    MOV cl,py
    cmp al,'q'
    je select_mid
    cmp al,'d'
    je Right
    cmp al,'a'
    je Left
    cmp al,'w'
    je up
    cmp al,'s'
    je down
    
    ret
;==================================
;This part is responsible for updating the selector position on key press
    Right:  
    cmp px,8
    je Return
    add px,1
    jmp Draw_Highlighted
    Left:
    cmp px,1
    je Return
    sub px,1
    jmp Draw_Highlighted
    up:
    cmp py,1
    je Return
    sub py,1
    jmp Draw_Highlighted
    down:
    cmp py,8
    je Return
    add py,1



    jmp Draw_Highlighted
    select_mid:jmp select

    Return:
        ret
;==================================
;This part is responsible for drawing selector position update
    Draw_Highlighted:
        call RedrawBoardSq ; redaraw the current as a normal board square (not higlighted) before moving 
        call DrawPossibleMoves ; need to redraw possible moves so that it is not erased if selector gets on a valid move's square
        call DrawPossibleAttacks ; need to redraw possible moves so that it is not erased if selector gets on a valid attack's square
        call RedrawPiece ; redraw piece if any at the old location

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
        DrawSq px, py ;draw higlighting of new square
        call RedrawPiece ; 
        ret
;==================================
;This part is responsible for moving and attacking logic
    select:
        
    ;check if valid move
        mov ch,px
        mov cl,py
        call to_idx
        lea si,ValidMoves
        call List_Contains
        cmp al,0 ; selected pos is not a valid move
        je Check_Valid_Attack
        call Move_Piece

        jmp H_I_ClearValidLists
        
        Check_Valid_Attack:
            lea si, ValidAttacks
            call List_Contains
            cmp al,0 ; selected pos is not a valid attack 
            jne Skip_Check_Empty
            mov al, '0'
            cmp [di], al ; check if position is empty and not valid move or attack
            je H_I_ClearValidLists
            jmp Sel
            Skip_Check_Empty:
            mov al, 'B'
            cmp [di], al
            je kill_Black
            ;kill white
            lea si, W_DeadPiece
            jmp Kill_Piece
            kill_Black:
            lea si, B_DeadPiece
            Kill_Piece:
            mov bl,'$'  ;get last element in array to append at the end
            sub si,2
            GetEnd:add si,2
                cmp [si],bl
                jne GetEnd
            mov bh,[di]
            mov bl,[di+1]
            mov [si],bh
            mov [si+1],bl
            call Move_Piece 
;==================================
;This part is responsible for re-inializing valid attack/move lists and drawing the updates of attacking/moving 
        H_I_ClearValidLists:
            ClearValidMoves
            ClearValidAttacks
            

            ;===============
            ;General deselects piece in hx:hy
            mov ch, hx
            mov cl, hy
            call RedrawBoardSq
            call RedrawPiece
            mov hx,0
            mov hy,0
            ;===============
            call DrawDeadP
            ret

;==================================
;This part is responsible for selecting a new piece and drawing its valid moves & attacks 
        Sel:

            mov ch, hx
            mov cl, hy
            call RedrawBoardSq
            call RedrawPiece

            mov ch, px
            mov cl, py

            mov hx, ch
            mov hy, cl 
            ClearValidMoves
            ClearValidAttacks

            mov ch, px
            mov cl, py
            call GetValidMoves
            call DrawPossibleMoves
            call DrawPossibleAttacks
            ret
    
HandleInput ENDP


;checks if a value in cx is contained in an array. lea si, array
;array's end is denoted by a $
;return al=1 if found al=0 if not found 
List_Contains PROC
    mov al, '$'
    Check_if_Exists:
        cmp [si],al
        je not_found
        cmp ch,[si]
        jne nextCheck
        cmp cl,[si+1]
        jne nextCheck
        jmp found

        nextCheck:
        add si,2
        jmp Check_if_Exists

    found:
    mov al,1
    ret
    not_found:
    mov al,0
    ret
List_Contains ENDP

; move a piece's location in chessboard array
; from hx & hy to px & py
Move_Piece PROC
    mov ch,px
    mov cl,py
    call to_idx
    mov bx,di
    mov ch,hx
    mov cl,hy
    call to_idx
    mov cx,[di]
    mov [bx],cx
    mov ax,3030h
    mov [di],ax
    ret
Move_Piece ENDP

;Clears an array and replace all elements with $
;lea si, array
ClearList PROC far
    mov al,'$'
    Clear:
        cmp [si], al
        je Cleared
        mov ch, [si]
        mov cl, [si+1]
        call RedrawBoardSq
        call RedrawPiece
        mov [si], al
        mov [si+1], al
        add si,2
        jmp Clear
    Cleared:
    ret
ClearList ENDP


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
    je pq
    inc bl
    cmp [di+1], bl
    je pk
    inc bl
    cmp [di+1],bl
    je ppw

    pr:         ;possible moves for rook
    call Moves_rook
    popa
    ret
    pkt:        ;possible moves for knight
    call Moves_knight
    popa
    ret
    pb:         ;possible moves for bishop
    call Moves_bishop
    popa
    ret
    pk:         ;possible moves for king
    mov IsKing,1
    call Moves_rook
    call Moves_bishop
    mov IsKing,0
    popa
    ret
    pq:         ;possible moves for queen
    call Moves_rook
    call Moves_bishop
    popa
    ret
    ppw:        ;possible moves for pawn
    call Moves_pawn
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
        cmp IsKing,1
        je M_R_Next1
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
        cmp IsKing,1
        je M_R_Next2
        jmp M_R_Check_Row_left

    M_R_Found_piece2:
        mov al, [di]
        cmp [bx], al
        je M_R_Next2 ;move rook end
        lea bx,ValidAttacks
        add bx,dx
        mov [bx], ah
        mov [bx+1], cl
        add dx,2
         



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
        cmp IsKing,1
        je M_R_Next3
        jmp M_R_Check_Col_Up

    M_R_Found_piece3:
        mov al, [di]
        cmp [bx], al
        je M_R_Next3 ;move rook end
        lea bx,ValidAttacks
        add bx,dx
        mov [bx], ch
        mov [bx+1], ah
        add dx,2

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
        cmp IsKing,1
        je M_R_Finalize
        jmp M_R_Check_Col_Down

    M_R_Found_piece4:
        mov al, [di]
        cmp [bx], al
        je M_R_Finalize ;move rook end
        lea bx,ValidAttacks
        add bx,dx
        mov [bx], ch
        mov [bx+1], ah
        add dx,2

    M_R_Finalize:
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
        cmp IsKing,1
        je M_B_Next1
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
        cmp IsKing,1
        je M_B_Next2
        jmp M_B_Check_Diag_left_up

    M_B_Found_piece2:
        mov dh, [di]
        cmp [bx], dh
        je M_B_Next2 ;move rook end
        lea bx,ValidAttacks
        mov dh,0
        add bx,dx
        mov [bx], ah
        mov [bx+1], al
        add dl,2
         



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
        cmp IsKing,1
        je M_B_Next3
        jmp M_B_Check_Diag_right_down

    M_B_Found_piece3:
        mov dh, [di]
        cmp [bx], dh
        je M_B_Next3 ;move rook end
        lea bx,ValidAttacks
        mov dh,0
        add bx,dx
        mov [bx], ah
        mov [bx+1], al
        add dl,2

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
        cmp IsKing,1
        je M_B_Finalize
        jmp M_B_Check_Diag_left_down

    M_B_Found_piece4:
        mov dh, [di]
        cmp [bx], dh
        je M_B_Finalize ;move rook end
        lea bx,ValidAttacks
        mov dh,0
        add bx,dx
        mov [bx], ah
        mov [bx+1], al
        add dl,2


     M_B_Finalize:
    ret
Moves_bishop ENDP

;GET PAWN MOVES
Moves_pawn PROC
    pusha
    lea bx, ValidMoves
    mov al, 'W'
    cmp [di], al
    jne BPwn_Moves
    cmp cl, 7
    je W_First_Move
    cmp cl,1 ;=> any subsequent dec cl wouldnt give 0
    jne Has_Moves
    popa 
    ret
    Has_Moves:
    sub di,16
    dec cl
    mov al,'0'
    cmp [di], al
    jne W_Check_Attacks
    mov [bx], ch
    mov [bx + 1], cl 
    jmp W_Check_Attacks

    W_First_Move:
    sub di,16
    dec cl 
    mov al,'0'
    cmp [di], al
    jne W_Check_Attacks
    mov [bx], ch
    mov [bx + 1], cl 
    add bx,2
    sub di,16
    cmp [di], al
    jne W_Before_Check_Attacks
    dec cl
    mov [bx], ch
    mov [bx + 1], cl 
    inc cl
    W_Before_Check_Attacks:
    add di,16

    W_Check_Attacks:
    lea bx,ValidAttacks
    mov al,'B'
    add di,2
    inc ch
    cmp ch,9
    je MP_skip1
    cmp [di], al
    jne MP_skip1
    mov [bx], ch
    mov [bx + 1], cl 
    add bx,2
    MP_skip1:
    mov al,'B'
    sub ch,2
    cmp ch,0
    je MP_end_mid
    sub di, 4
    cmp [di], al
    jne MP_end_mid
    mov [bx], ch
    mov [bx + 1], cl 
    jmp MP_end_mid

    BPwn_Moves:
    cmp cl, 2
    je B_First_Move
    cmp cl,8
    jne Has_Moves_B
    popa 
    ret
    Has_Moves_B:
    add di,16
    inc cl
    mov al,'0'
    cmp [di], al
    jne B_Check_Attacks
    mov [bx], ch
    mov [bx + 1], cl 
    jmp B_Check_Attacks

    MP_end_mid:
    jmp MP_end

    B_First_Move:
    add di,16
    inc cl 
    mov al,'0'
    cmp [di], al
    jne B_Check_Attacks
    mov [bx], ch
    mov [bx + 1], cl 
    add bx,2
    add di,16
    cmp [di], al
    jne B_Before_Check_Attacks
    inc cl
    mov [bx], ch
    mov [bx + 1], cl 
    dec cl
    B_Before_Check_Attacks:
    sub di,16

    B_Check_Attacks:
    lea bx,ValidAttacks
    mov al,'W'
    add di,2
    inc ch
    cmp ch,9
    je MP_skip2
    cmp [di], al
    jne MP_skip2
    mov [bx], ch
    mov [bx + 1], cl 
    add bx,2
    MP_skip2:
    mov al,'W'
    sub ch,2
    cmp ch,0
    je MP_end
    sub di, 4
    cmp [di], al
    jne MP_end
    mov [bx], ch
    mov [bx + 1], cl 

    MP_end:
    popa
    ret
Moves_pawn ENDP

;GET KNIGHT MOVES
Moves_knight PROC
    pusha
    lea bx, ValidMoves
    lea si,ValidAttacks
    mov al,[di]
    mov ch,hx
    mov cl,hy

    ;if up is possible
    cmp cl,3
    jb Check_Down
    cmp ch,8
    je cont1
    inc ch
    sub cl,2
    call to_idx
    cmp [di],al
    je cont1
    mov ah,'0'
    cmp [di],ah
    je addmove1
    mov [si],ch
    mov [si+1],cl
    add si,2
    jmp cont1
    addmove1:
    mov [bx],ch
    mov [bx+1],cl
    add bx,2
    jmp cont1
    cont1:
    mov ch,hx
    mov cl,hy
    cmp ch,1
    je Check_Down
    sub cl,2
    dec ch
    call to_idx
    cmp [di],al
    je Check_Down
    mov ah,'0'
    cmp [di],ah
    je addmove2
    mov [si],ch
    mov [si+1],cl
    add si,2
    jmp Check_Down
    addmove2:
    mov [bx],ch
    mov [bx+1],cl
    add bx,2

    Check_Down:
    mov ch,hx
    mov cl,hy
    cmp cl,6
    ja Check_Right
    cmp ch,8
    je cont2
    inc ch
    add cl,2
    call to_idx
    cmp [di],al
    je cont2
    mov ah,'0'
    cmp [di],ah
    je addmove3
    mov [si],ch
    mov [si+1],cl
    add si,2
    jmp cont2
    addmove3:
    mov [bx],ch
    mov [bx+1],cl
    add bx,2
    jmp cont2
    cont2:
    mov ch,hx
    mov cl,hy
    cmp ch,1
    je Check_Right
    dec ch
    add cl,2
    call to_idx
    cmp [di],al
    je Check_Right
    mov ah,'0'
    cmp [di],ah
    je addmove4
    mov [si],ch
    mov [si+1],cl
    add si,2
    jmp Check_Right
    addmove4:
    mov [bx],ch
    mov [bx+1],cl
    add bx,2

    Check_Right:
    mov ch,hx
    mov cl,hy
    cmp ch,6
    ja Check_Left
    cmp cl,1
    je cont3
    add ch,2
    dec cl
    call to_idx
    cmp [di],al
    je cont3
    mov ah,'0'
    cmp [di],ah
    je addmove5
    mov [si],ch
    mov [si+1],cl
    add si,2
    jmp cont3
    addmove5:
    mov [bx],ch
    mov [bx+1],cl
    add bx,2
    cont3:
    mov ch,hx
    mov cl,hy
    cmp cl,8
    je Check_Left
    add ch,2
    inc cl
    call to_idx
    cmp [di],al
    je Check_Left
    mov ah,'0'
    cmp [di],ah
    je addmove6
    mov [si],ch
    mov [si+1],cl
    add si,2
    jmp Check_Left
    addmove6:
    mov [bx],ch
    mov [bx+1],cl
    add bx,2

    Check_Left:
    mov ch,hx
    mov cl,hy
    cmp ch,3
    jb No_Kt
    cmp cl,1
    je cont4
    sub ch,2
    dec cl
    call to_idx
    cmp [di],al
    je cont4
    mov ah,'0'
    cmp [di],ah
    je addmove7
    mov [si],ch
    mov [si+1],cl
    add si,2
    jmp cont4
    addmove7:
    mov [bx],ch
    mov [bx+1],cl
    add bx,2
    cont4:
    mov ch,hx
    mov cl,hy
    cmp cl,8
    je No_Kt
    sub ch,2
    inc cl
    call to_idx
    cmp [di],al
    je No_Kt
    mov ah,'0'
    cmp [di],ah
    je addmove8
    mov [si],ch
    mov [si+1],cl
    add si,2
    jmp No_Kt
    addmove8:
    mov [bx],ch
    mov [bx+1],cl
    add bx,2
    
    
    No_Kt:
    popa
    ret
Moves_knight ENDP


END MAIN