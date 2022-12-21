EXTRN InitBoard:FAR
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
Public GameScreen

Public chessBoard
Public ValidMoves
Public ValidAttacks
Public ValidMoves2
Public ValidAttacks2
Public B_DeadPiece
Public W_DeadPiece
include Macro.inc
.286
.Model small
.Stack 200h
.Data

Beginning_Board         db "B0","B1","B2","B3","B4","B2","B1","B0"
                        db "B5","B5","B5","B5","B5","B5","B5","B5"
                        db "00","00","00","00","00","00","00","00"
                        db "00","00","00","00","00","00","00","00"
                        db "00","00","00","00","00","00","00","00"
                        db "00","00","00","00","00","00","00","00"
                        db "W5","W5","W5","W5","W5","W5","W5","W5"
                        db "W0","W1","W2","W3","W4","W2","W1","W0"

chessBoard              db "B0","B1","B2","B3","B4","B2","B1","B0"
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

ValidMoves2 db 32 dup('$$'), '$'  ; assuming that the max no. of possible moves for 1 piece is 32 
                            ; idk the correct number
ValidAttacks2 db 8 dup('$$'), '$' 

;To display dead pieces
B_DeadPiece db 16 dup('$$'),'$'
W_DeadPiece db 16 dup('$$'),'$'

higlight_Pos label byte
px  db 1
py  db 8
px2 db 1
py2 db 1
selPos label byte ; current selected piece position
hx  db 0
hy  db 0
hx2 db 0
hy2 db 0
sq db 20 dup('$')
IsKing db ?
W_King_X db 5
W_King_Y db 8
B_King_X db 5
B_King_Y db 1
B_Check db 0
W_Check db 0

;Time variables
prevms db ? 
prevs db ?  
Prevtime dw ?
frame db ?   
timer dw 0 

CoolDownPieces dw 64 dup(0)
.Code


GameScreen PROC FAR
    MOV AX , @DATA
    MOV DS , AX
    
    
    call InitBoard
    call far ptr InitGame
    DrawSq px, py
    MOV ch,px
    MOV cl,py
    call RedrawPiece
    DrawSq2 px2, py2
    MOV ch,px2
    MOV cl,py2
    call RedrawPiece

    mov ah, 2Ch    
    int 21h
    mov al,dh ; move el seconds
    mov ah,0

    mov bl, 100
    mul bl
    mov bl, cl
    mov bh, 0
    add ax , bx
    mov bx, ax
    mov Prevtime, bx
    mov prevs, dh
    ;======================================================================================
    ;MAIN GAME LOOP

    GameLP:
        call far ptr GetFrameTime
        lea bx, CoolDownPieces
        mov dx, 900
        mov si, 0
        mov cx, 64
        UpdateCooldown:
            cmp [word ptr bx], si ;0
            je NotInCoolDown

            cmp [word ptr bx], dx ;300 
            jae DoneCooldown

            add [word ptr bx], ax 
            jmp NotInCoolDown

            DoneCooldown:
            mov [word ptr bx], si ;0

            NotInCoolDown:
            add bx, 2
            loop UpdateCooldown


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
        call far ptr HandleInput
        call far ptr HandleInput2

        jmp GameLP
        
    ending:
    
    ; ; Press any key to exit
    ; MOV AH , 0
    ; INT 16h
    
    
    ; ;Change to Text MODE
    ; MOV AH,0          
    ; MOV AL,03h
    ; INT 10h 

    ; ; return control to operating system
    ; MOV AH , 4ch
    ; INT 21H
    ret
GameScreen ENDP

;description
InitGame PROC Far

;Initialize array to default values at start of game
        mov ax, @data
        mov es, ax
        lea si, Beginning_Board
        lea di,chessBoard
        mov cx, 64
        REP MOVSW       
        lea si,B_DeadPiece
        call ClearList 
        lea si,W_DeadPiece
        call ClearList 
        lea si, ValidMoves
        call ClearList
        lea si, ValidAttacks
        call ClearList
        lea si, ValidMoves2
        call ClearList
        lea si, ValidAttacks2
        call ClearList
        mov px , 1
        mov py , 8
        mov px2, 1
        mov py2, 1
        mov hx , 0
        mov hy , 0
        mov hx2, 0
        mov hy2, 0
        mov W_King_X , 5
        mov W_King_Y , 8
        mov B_King_X , 5
        mov B_King_Y , 1
        mov B_Check , 0
        mov W_Check , 0
; initialize the board and draws all pieces in place
        call InitBoard   
        DrawSq px, py
        MOV ch,px
        MOV cl,py
        call RedrawPiece
        DrawSq2 px2, py2
        MOV ch,px2
        MOV cl,py2
        call RedrawPiece
InitGame ENDP


;white / should handle input for black and white but in single player mode in phase 2,
; just need to uncomment some parts. for now it handles white player's input
HandleInput PROC Far   ; the user input is in ax => al:ascii ah:scan code
                    ;Handles all game logic
                    ;Including:
                    ;   1-Check for input
                    ;   2-Move piece
                    ;   3-Getting available moves
    MOV ch,px
    MOV cl,py
    cmp al,'q'
    je Player1
    cmp al,'d'
    je RightP1
    cmp al,'a'
    je LeftP1
    cmp al,'w'
    je upP1
    cmp al,'s'
    je downP1

    ; cmp ah, 39h
    ; je Player2
    ; cmp ah, 4Dh
    ; je RightP2
    ; cmp ah, 4Bh
    ; je LeftP2
    ; cmp ah, 48h
    ; je upP2
    ; cmp ah, 50h
    ; je downP2
    
    ret
;==================================
;This part is responsible for updating the selector position on key press
    RightP1:  
    cmp px,8
    je Return
    add px,1
    jmp Draw_Highlighted
    LeftP1:
    cmp px,1
    je Return
    sub px,1
    jmp Draw_Highlighted
    upP1:
    cmp py,1
    je Return
    sub py,1
    jmp Draw_Highlighted
    downP1:
    cmp py,8
    je Return
    add py,1

    ; RightP2:  
    ; cmp px2,8
    ; je Return
    ; add px2,1
    ; jmp Draw_Highlighted
    ; LeftP2:
    ; cmp px2,1
    ; je Return
    ; sub px2,1
    ; jmp Draw_Highlighted
    ; upP2:
    ; cmp py2,1
    ; je Return
    ; sub py2,1
    ; jmp Draw_Highlighted
    ; downP2:
    ; cmp py2,8
    ; je Return
    ; add py2,1


    jmp Draw_Highlighted
    select_mid:jmp select

    Return:
        ret

    Player1:
    mov dl, 'W' ; W indicates that it is player one that is selecting
    jmp select_mid

    ; Player2:
    ; mov dl, 'B' ; B indicates that it is player one that is selecting
    ; jmp select_mid
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
            lea si, ValidMoves
            ClearValidMoves
            lea si, ValidAttacks
            ClearValidAttacks
            DeselectPlayer1
            call DrawDeadP
            ret

;==================================
;This part is responsible for selecting a new piece and drawing its valid moves & attacks 
        Sel:
            mov ch, px
            mov cl, py
            call to_idx
            cmp [di], dl
            je Valid_Sel_mid
            ; should Deslect player1 if Q is pressed and Deselect player2 when Space is pressed
            DeselectPlayer1
            lea si, ValidMoves
            ClearValidMoves
            

            ;============
            jmp Valid_Sel_Skip
            Valid_Sel_mid: jmp Valid_Sel
            Valid_Sel_Skip:
            ;============

            lea si, ValidAttacks
            ClearValidAttacks

            ret
            Valid_Sel:
            mov ch, hx
            mov cl, hy
            call RedrawBoardSq
            call RedrawPiece

            mov ch, px
            mov cl, py

            mov hx, ch
            mov hy, cl 
            lea si, ValidMoves
            ClearValidMoves
            lea si, ValidAttacks
            ClearValidAttacks

            mov ch, px
            mov cl, py
            call GetValidMoves
            call DrawPossibleMoves
            call DrawPossibleAttacks
            ret    
HandleInput ENDP


;black
HandleInput2 PROC Far   ; the user input is in ax => al:ascii ah:scan code
                    ;Handles all game logic
                    ;Including:
                    ;   1-Check for input
                    ;   2-Move piece
                    ;   3-Getting available moves
    MOV ch,px2
    MOV cl,py2


    cmp ah, 39h
    je Player2
    cmp ah, 4Dh
    je RightP2
    cmp ah, 4Bh
    je LeftP2
    cmp ah, 48h
    je upP2
    cmp ah, 50h
    je downP2
    
    ret
;==================================
;This part is responsible for updating the selector position on key press
    RightP2:  
    cmp px2,8
    je Return2
    add px2,1
    jmp Draw_Highlighted2
    LeftP2:
    cmp px2,1
    je Return2
    sub px2,1
    jmp Draw_Highlighted2
    upP2:
    cmp py2,1
    je Return2
    sub py2,1
    jmp Draw_Highlighted2
    downP2:
    cmp py2,8
    je Return2
    add py2,1


    jmp Draw_Highlighted2
    select_mid2:jmp select2

    Return2:
        ret

    Player2:
    mov dl, 'B' ; B indicates that it is player one that is selecting
    jmp select2
;==================================
;This part is responsible for drawing selector position update
    Draw_Highlighted2:
        call RedrawBoardSq ; redaraw the current as a normal board square (not higlighted) before moving 
        call DrawPossibleMoves ; need to redraw possible moves so that it is not erased if selector gets on a valid move's square
        call DrawPossibleAttacks ; need to redraw possible moves so that it is not erased if selector gets on a valid attack's square
        call RedrawPiece ; redraw piece if any at the old location

        cmp hx2,0
        je skip2
        ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
        DrawSq2 hx2, hy2
        mov ch, hx2
        mov cl, hy2
        call RedrawPiece
        skip2:
        mov ch,px2
        mov cl,py2
        DrawSq2 px2, py2 ;draw higlighting of new square
        call RedrawPiece ;
        ; mov ch,px2
        ; mov cl,py2
        ; DrawSq px2, py2 ;draw higlighting of new square
        ; call RedrawPiece ; 
        ret
;==================================
;This part is responsible for moving and attacking logic
    select2:
        
    ;check if valid move
        mov ch,px2
        mov cl,py2
        call to_idx
        lea si,ValidMoves2
        call List_Contains
        cmp al,0 ; selected pos is not a valid move
        je Check_Valid_Attack2
        call Move_Piece2

        jmp H_I_ClearValidLists2
        
        Check_Valid_Attack2:
            lea si, ValidAttacks2
            call List_Contains
            cmp al,0 ; selected pos is not a valid attack 
            jne Skip_Check_Empty2
            mov al, '0'
            cmp [di], al ; check if position is empty and not valid move or attack
            je H_I_ClearValidLists2
            jmp Sel2
            Skip_Check_Empty2:
            mov al, 'B'
            cmp [di], al
            je kill_Black2
            ;kill white
            lea si, W_DeadPiece
            jmp Kill_Piece2
            kill_Black2:
            lea si, B_DeadPiece
            Kill_Piece2:
            mov bl,'$'  ;get last element in array to append at the end
            sub si,2
            GetEnd2:add si,2
                cmp [si],bl
                jne GetEnd2
            mov bh,[di]
            mov bl,[di+1]
            mov [si],bh
            mov [si+1],bl
            call Move_Piece2 
;==================================
;This part is responsible for re-inializing valid attack/move lists and drawing the updates of attacking/moving 
        H_I_ClearValidLists2:
             lea si, ValidMoves2
            ClearValidMoves2
            lea si, ValidAttacks2
            ClearValidAttacks2
            DeselectPlayer2
            call DrawDeadP
            ret

;==================================
;This part is responsible for selecting a new piece and drawing its valid moves & attacks 
        Sel2:
            mov ch, px2
            mov cl, py2
            call to_idx
            cmp [di], dl
            je Valid_Sel_mid2
            ; should Deslect player1 if Q is pressed and Deselect player2 when Space is pressed
            DeselectPlayer2
             lea si, ValidMoves2
            ClearValidMoves2

            ;============
            jmp Valid_Sel_Skip2
            Valid_Sel_mid2: jmp Valid_Sel2
            Valid_Sel_Skip2:
            ;============

            
            lea si, ValidAttacks2
            ClearValidAttacks2

            ret
            Valid_Sel2:
            mov ch, hx2
            mov cl, hy2
            call RedrawBoardSq
            call RedrawPiece

            mov ch, px2
            mov cl, py2

            mov hx2, ch
            mov hy2, cl 
             lea si, ValidMoves2
            ClearValidMoves2
            lea si, ValidAttacks2
            ClearValidAttacks2

            mov ch, px2
            mov cl, py2
            call GetValidMoves
            call DrawPossibleMoves
            call DrawPossibleAttacks
            ret    
HandleInput2 ENDP

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

    mov al, hx
    mov ah,0
    mov bl, 2
    mul bl
    mov bh, al
    
    mov al, hy
    mov ah,0
    mov bl, 16
    mul bl

    add al, bh
    mov si, ax
    mov ax, 0
    lea bx, CoolDownPieces
    cmp [word ptr bx + si], ax
    je MP_move
    ret

    MP_move:
     mov al, px
    mov ah,0
    mov bl, 2
    mul bl
    mov bh, al
         
    mov al, py
    mov ah,0
    mov bl, 16
    mul bl     

    add al, bh
    mov si, ax
    mov ax, 0
    lea bx, CoolDownPieces

    mov ax,1
    mov [word ptr bx + si], ax

    mov ch,hx
    mov cl,hy
    call to_idx
    ;need to check if king update the variable king positions
    ;need to check for check/checkmate
    mov al,'4'
    cmp [di+1],al
    jne SkipKingUpdate

    ;if moving the king
    mov ch,px
    mov cl,py

    ; mov al,'B'
    ; cmp [di], al
    ; je Black_King
    mov W_King_X,ch
    mov W_King_Y,cl
    ; jmp SkipKingUpdate

    ; Black_King:
    ; mov B_King_X,ch
    ; mov B_King_Y,cl

    SkipKingUpdate: 
    

    MovePiece2:
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

    call UpdateCheck
    ret
Move_Piece ENDP

Move_Piece2 PROC

    mov al, hx2
    mov ah,0
    mov bl, 2
    mul bl
    mov bh, al

      
    mov al, hy2
    mov ah,0
    mov bl, 16
    mul bl    

    add al, bh
    mov si, ax
    mov ax, 0
    lea bx, CoolDownPieces
    cmp [word ptr bx + si], ax
    je MP_move2
    ret

    MP_move2:

    mov al, px2
    mov ah,0
    mov bl, 2
    mul bl
    mov bh, al
         
    mov al, py2
    mov ah,0
    mov bl, 16
    mul bl     

    add al, bh
    mov si, ax
    mov ax, 0
    lea bx, CoolDownPieces

    mov ax,1
    mov [word ptr bx + si], ax
      

    mov ch,hx2
    mov cl,hy2
    call to_idx
    ;need to check if king update the variable king positions
    ;need to check for check/checkmate
    mov al,'4'
    cmp [di+1],al
    jne SkipKingUpdate2

    ;if moving the king
    mov ch,px2
    mov cl,py2
    mov B_King_X,ch
    mov B_King_Y,cl

    
    SkipKingUpdate2: 
    mov ch,px2
    mov cl,py2
    call to_idx

    mov bx,di
    mov ch,hx2
    mov cl,hy2
    call to_idx
    mov cx,[di]
    mov [bx],cx
    mov ax,3030h
    mov [di],ax

    call UpdateCheck

      
    ret
Move_Piece2 ENDP

;Clears an array and replace all elements with $
;lea si, array
ClearList PROC 
    mov al,'$'
    Clear:
        cmp [si], al
        je Cleared
        mov ch, [si]
        mov cl, [si+1]
        ; call RedrawBoardSq
        ; call RedrawPiece
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
;dl player W or B
GetValidMoves PROC
    pusha

    ; mov dl,'3'
    ; mov ah,2
    ; int 21h

    call to_idx
    mov bl, '0'
    mov dl, 'B'
    cmp [di],dl
    je LoadBlack
    mov dx,0
    lea si, ValidMoves
    ;lea bx, ValidAttacks
    jmp Compare
    LoadBlack:
    ;lea bx, ValidAttacks2
    lea si, ValidMoves2
    mov dx,82

    Compare:
    mov bl, '0'
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

     mov dl, 'B'
    cmp [di],dl
    je LoadBlack2
    lea bx, ValidMoves
    lea si, ValidAttacks
    ;lea bx, ValidAttacks
    jmp Compare2
    LoadBlack2:
    ;lea bx, ValidAttacks2
    lea bx, ValidMoves2
    lea si, ValidAttacks2
    Compare2:

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

    mov dl, 'B'
    cmp [di],dl
    je LoadBlack3
    lea bx, ValidMoves
    lea dx, ValidAttacks
    ;lea bx, ValidAttacks
    jmp Compare3
    LoadBlack3:
    ;lea bx, ValidAttacks2
    lea bx, ValidMoves2
    lea dx, ValidAttacks2
    Compare3:

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
    M_R_Check_Row_leftP1:
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
        jmp M_R_Check_Row_leftP1

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
    M_B_Check_Diag_leftP1_up:
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
        jmp M_B_Check_Diag_leftP1_up

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
    M_B_Check_Diag_leftP1_down:
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
        jmp M_B_Check_Diag_leftP1_down

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
;DI has idx
;lea Bx has valid moves
;lea dx has valid attacks
Moves_pawn PROC
    pusha
    ;lea bx, ValidMoves
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
    mov bx,dx;================
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
    mov bx,dx;==============================
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
; Moves_knight PROC
;     pusha
;     lea bx, ValidMoves
;     lea si,ValidAttacks
;     mov al,[di]
;     mov ch,hx
;     mov cl,hy

;     ;if up is possible
;     cmp cl,3
;     jb Check_Down
;     cmp ch,8
;     je cont1
;     inc ch
;     sub cl,2
;     call to_idx
;     cmp [di],al
;     je cont1
;     mov ah,'0'
;     cmp [di],ah
;     je addmove1
;     mov [si],ch
;     mov [si+1],cl
;     add si,2
;     jmp cont1
;     addmove1:
;     mov [bx],ch
;     mov [bx+1],cl
;     add bx,2
;     jmp cont1
;     cont1:
;     mov ch,hx
;     mov cl,hy
;     cmp ch,1
;     je Check_Down
;     sub cl,2
;     dec ch
;     call to_idx
;     cmp [di],al
;     je Check_Down
;     mov ah,'0'
;     cmp [di],ah
;     je addmove2
;     mov [si],ch
;     mov [si+1],cl
;     add si,2
;     jmp Check_Down
;     addmove2:
;     mov [bx],ch
;     mov [bx+1],cl
;     add bx,2

;     Check_Down:
;     mov ch,hx
;     mov cl,hy
;     cmp cl,6
;     ja Check_Right
;     cmp ch,8
;     je cont2
;     inc ch
;     add cl,2
;     call to_idx
;     cmp [di],al
;     je cont2
;     mov ah,'0'
;     cmp [di],ah
;     je addmove3
;     mov [si],ch
;     mov [si+1],cl
;     add si,2
;     jmp cont2
;     addmove3:
;     mov [bx],ch
;     mov [bx+1],cl
;     add bx,2
;     jmp cont2
;     cont2:
;     mov ch,hx
;     mov cl,hy
;     cmp ch,1
;     je Check_Right
;     dec ch
;     add cl,2
;     call to_idx
;     cmp [di],al
;     je Check_Right
;     mov ah,'0'
;     cmp [di],ah
;     je addmove4
;     mov [si],ch
;     mov [si+1],cl
;     add si,2
;     jmp Check_Right
;     addmove4:
;     mov [bx],ch
;     mov [bx+1],cl
;     add bx,2

;     Check_Right:
;     mov ch,hx
;     mov cl,hy
;     cmp ch,6
;     ja Check_Left
;     cmp cl,1
;     je cont3
;     add ch,2
;     dec cl
;     call to_idx
;     cmp [di],al
;     je cont3
;     mov ah,'0'
;     cmp [di],ah
;     je addmove5
;     mov [si],ch
;     mov [si+1],cl
;     add si,2
;     jmp cont3
;     addmove5:
;     mov [bx],ch
;     mov [bx+1],cl
;     add bx,2
;     cont3:
;     mov ch,hx
;     mov cl,hy
;     cmp cl,8
;     je Check_Left
;     add ch,2
;     inc cl
;     call to_idx
;     cmp [di],al
;     je Check_Left
;     mov ah,'0'
;     cmp [di],ah
;     je addmove6
;     mov [si],ch
;     mov [si+1],cl
;     add si,2
;     jmp Check_Left
;     addmove6:
;     mov [bx],ch
;     mov [bx+1],cl
;     add bx,2

;     Check_Left:
;     mov ch,hx
;     mov cl,hy
;     cmp ch,3
;     jb No_Kt
;     cmp cl,1
;     je cont4
;     sub ch,2
;     dec cl
;     call to_idx
;     cmp [di],al
;     je cont4
;     mov ah,'0'
;     cmp [di],ah
;     je addmove7
;     mov [si],ch
;     mov [si+1],cl
;     add si,2
;     jmp cont4
;     addmove7:
;     mov [bx],ch
;     mov [bx+1],cl
;     add bx,2
;     cont4:
;     mov ch,hx
;     mov cl,hy
;     cmp cl,8
;     je No_Kt
;     sub ch,2
;     inc cl
;     call to_idx
;     cmp [di],al
;     je No_Kt
;     mov ah,'0'
;     cmp [di],ah
;     je addmove8
;     mov [si],ch
;     mov [si+1],cl
;     add si,2
;     jmp No_Kt
;     addmove8:
;     mov [bx],ch
;     mov [bx+1],cl
;     add bx,2
    
    
;     No_Kt:
;     popa
;     ret
; Moves_knight ENDP

;GET KNIGHT MOVES
Moves_knight PROC
    ;gets the piece's element in chess board array in di
    ;row:column ch:cl
    ;gets valid moves in bx
    ;gets valid attacks in si

    mov al,[di]
    mov ah, '0'
    push cx
    ; Check up1 right
    sub di, 12 ; go up row ela 2 squares 
    cmp cl, 1
    je MK_next4_mid
    cmp ch, 7
    jae MK_next1
    dec cl
    add ch, 2
    cmp [di], ah
    jne MK_CheckAttack1
    mov [bx], ch
    mov [bx+1], cl
    add bx, 2
    jmp MK_next1
    MK_CheckAttack1:
    cmp [di], al
    je MK_next1
    mov [si], ch
    mov [si+1], cl
    add si, 2
    ;jmp MK_next1

    ;up1 left
    MK_next1:
    pop cx
    push cx
    sub di, 8

    cmp ch, 2
    jbe MK_next2
    dec cl
    sub ch, 2
    cmp [di], ah
    jne MK_CheckAttack2
    mov [bx], ch
    mov [bx+1], cl
    add bx, 2
    jmp MK_next2
    MK_CheckAttack2:
    cmp [di], al
    je MK_next2
    mov [si], ch
    mov [si+1], cl
    add si, 2
    jmp MK_next2

    MK_next4_mid: 
    sub di, 18
    jmp MK_next4

    ;up2 left
    MK_next2:
    pop cx
    push cx
    sub di, 14

    cmp cl, 2
    je MK_next3
    cmp ch, 1
    je MK_next3
    sub cl, 2
    dec ch
    cmp [di], ah
    jne MK_CheckAttack3
    mov [bx], ch
    mov [bx+1], cl
    add bx, 2
    jmp MK_next3
    MK_CheckAttack3:
    cmp [di], al
    je MK_next3
    mov [si], ch
    mov [si+1], cl
    add si, 2
    ;jmp MK_next2

    ;up2 right
    MK_next3:
    pop cx
    push cx
    add di, 4

    cmp cl, 2
    je MK_next4
    cmp ch, 8
    je MK_next4
    sub cl, 2
    inc ch
    cmp [di], ah
    jne MK_CheckAttack4
    mov [bx], ch
    mov [bx+1], cl
    add bx, 2
    jmp MK_next4
    MK_CheckAttack4:
    cmp [di], al
    je MK_next4
    mov [si], ch
    mov [si+1], cl
    add si, 2
    ;jmp MK_next2

;==================
;down
    ;down1 right
    MK_next4:
    pop cx
    push cx
    add di, 50 
            
    cmp cl, 8
    je MK_End_mid
    cmp ch, 7
    jae MK_next5
    inc cl
    add ch, 2
    cmp [di], ah
    jne MK_CheckAttack5
    mov [bx], ch
    mov [bx+1], cl
    add bx, 2
    jmp MK_next5
    MK_CheckAttack5:
    cmp [di], al
    je MK_next5
    mov [si], ch
    mov [si+1], cl
    add si, 2

    ;down 1 left
    MK_next5:
    pop cx
    push cx
    sub di, 8

    cmp ch, 2
    jbe MK_next6
    inc cl
    sub ch, 2
    cmp [di], ah
    jne MK_CheckAttack6
    mov [bx], ch
    mov [bx+1], cl
    add bx, 2
    jmp MK_next6
    MK_CheckAttack6:
    cmp [di], al
    je MK_next6
    mov [si], ch
    mov [si+1], cl
    add si, 2
    jmp MK_next6

    MK_End_mid: jmp MK_End
    
    ;down2 left
    MK_next6:
    pop cx
    push cx
    add di, 18

    cmp cl, 7
    jae MK_End
    cmp ch, 1
    je MK_next7
    add cl,2
    dec ch
    cmp [di], ah
    jne MK_CheckAttack7
    mov [bx], ch
    mov [bx+1], cl
    add bx, 2
    jmp MK_next7
    MK_CheckAttack7:
    cmp [di], al
    je MK_next7
    mov [si], ch
    mov [si+1], cl
    add si, 2

    ;down2 right    
    MK_next7:
    pop cx
    push cx
    add di, 4


    cmp ch, 8
    je MK_End
    add cl,2
    inc ch
    cmp [di], ah
    jne MK_CheckAttack8
    mov [bx], ch
    mov [bx+1], cl
    add bx, 2
    jmp MK_End
    MK_CheckAttack8:
    cmp [di], al
    je MK_End
    mov [si], ch
    mov [si+1], cl
    add si, 2

    
    MK_End:
    pop cx 
    ret

Moves_knight ENDP

;decides if a king is in check
; takes the king position in Cx row col ==> ch:cl
; returns al=1 if is in check al = 0 otherwise
Is_Check PROC
    pusha
    call to_idx

    push ax
    ;  mov ah,2
    ; mov dl, '1'
    ; int 21h
    pop ax

    ;=======================
    ;Rows & columns
    ;=======================
        mov bx,di
        add bx,2
        mov al, '0'
        mov ah,ch
        IC_Check_Row_right:
            inc ah
            cmp ah, 9
            je IC_Next1
            cmp [bx], al
            jne IC_Found_piece1
            add bx,2
            
            push ax
            ; mov ah,2
            ; mov dl, '7'
            ; int 21h
            pop ax

            jmp IC_Check_Row_right

        IC_Found_piece1:

            push ax
            ; mov ah,2
            ; mov dl, 'Q'
            ; int 21h
            pop ax

            mov al, [di]
            cmp [bx], al
            je IC_Next1
            mov al, '3'
            cmp [bx+1], al
            je Incheck_Mid1 
            mov al, '0'
            cmp [bx+1], al
            je Incheck_Mid1
            
          
        IC_Next1:
        ;  mov ah,2
        ; mov dl, '2'
        ; int 21h  

        mov bx,di
        sub bx,2
        mov al, '0'
        mov ah,ch
        IC_Check_Row_Left:
            dec ah
            cmp ah, 0
            je IC_Next2
            cmp [bx], al
            jne IC_Found_piece2
            sub bx,2
            jmp IC_Check_Row_Left

        IC_Found_piece2:
            mov al, [di]
            cmp [bx], al
            je IC_Next2
            mov al, '3'
            cmp [bx+1], al
            je Incheck_Mid1 
            mov al, '0'
            cmp [bx+1], al
            je Incheck_Mid1


        ;==================
        jmp IC_Next2
        Incheck_Mid1: jmp far ptr InCheck
        ;==================

        IC_Next2:
        ;  mov ah,2
        ; mov dl, '3'
        ; int 21h

        mov bx,di
        sub bx,16
        mov al, '0'
        mov ah,cl
        IC_Check_Col_Up:
            dec ah
            cmp ah, 0
            je IC_Next3
            cmp [bx], al
            jne IC_Found_piece3
            sub bx,16
            jmp IC_Check_Col_Up

        IC_Found_piece3:
            mov al, [di]
            cmp [bx], al
            je IC_Next3
            mov al, '3'
            cmp [bx+1], al
            je Incheck_Mid1 
            mov al, '0'
            cmp [bx+1], al
            je Incheck_Mid1

        IC_Next3:
        ;  mov ah,2
        ; mov dl, '4'
        ; int 21h

        mov bx,di
        add bx,16
        mov al, '0'
        mov ah,cl
        IC_Check_Col_Down:
            inc ah
            cmp ah, 9
            je Bishop
            cmp [bx], al
            jne IC_Found_piece4
            add bx,16
            jmp IC_Check_Col_Down

        IC_Found_piece4:
            mov al, [di]
            cmp [bx], al
            je Bishop
            mov al, '3'
            cmp [bx+1], al
            je Incheck_Mid1 
            mov al, '0'
            cmp [bx+1], al
            je Incheck_Mid1

        ; mov ah,2
        ; mov dl, '5'
        ; int 21h
    Bishop:
    ;=======================
    ;diagonals
    ;=======================

        mov bx,di
        sub bx, 14
        mov dh,'0'
        mov ax, cx
        ;lea si,ValidMoves ;=========>> el index bta3y sabt ba7arako kol ma ala2y valid square
        IC_Check_Diag_right_up:
            inc ah
            dec al
            cmp ah, 9
            je I_C_Next1
            cmp al, 0
            je I_C_Next1
            cmp [bx], dh
            jne I_C_Found_piece1
            sub bx, 14 ; 34an ytl3 row ela square
            jmp IC_Check_Diag_right_up

        I_C_Found_piece1:
            mov dh, [di]
            cmp [bx], dh
            je I_C_Next1
            mov al, '2'
            cmp [bx+1], al
            je Incheck_Mid2 
            mov al, '3'
            cmp [bx+1], al
            je Incheck_Mid2 

        I_C_Next1:
        ;initializations
        mov bx,di
        sub bx, 18
        mov dh,'0'
        mov ax, cx
        I_C_Check_Diag_leftP1_up:
            dec ah
            dec al
            cmp ah, 0
            je I_C_Next2
            cmp al, 0
            je I_C_Next2
            cmp [bx], dh
            jne I_C_Found_piece2
            sub bx, 18 ; 34an ytl3 row + square kman
            jmp I_C_Check_Diag_leftP1_up

        I_C_Found_piece2:
            mov dh, [di]
            cmp [bx], dh
            je I_C_Next2 ;move rook end
            mov al, '2'
            cmp [bx+1], al
            je Incheck_Mid2 
            mov al, '3'
            cmp [bx+1], al
            je Incheck_Mid2 


        ;==================
            jmp I_C_Next2
            Incheck_Mid2: jmp far ptr InCheck
        ;==================

        I_C_Next2:
        ;initializations
        mov bx,di
        ADD bx, 18
        mov dh,'0'
        mov ax, cx
        I_C_Check_Diag_right_down:
            inc ah
            inc al
            cmp ah, 9
            je I_C_Next3
            cmp al, 9
            je I_C_Next3
            cmp [bx], dh
            jne I_C_Found_piece3
            add bx, 18 ; ynzl row + square
            jmp I_C_Check_Diag_right_down

        I_C_Found_piece3:
            mov dh, [di]
            cmp [bx], dh
            je I_C_Next3 ;move rook end
            mov al, '2'
            cmp [bx+1], al
            je Incheck_Mid2 
            mov al, '3'
            cmp [bx+1], al
            je Incheck_Mid2 

        I_C_Next3:
        ;initializations
        mov bx,di
        add bx, 14
        mov dh,'0'
        mov ax, cx
        I_C_Check_Diag_leftP1_down:
            dec ah
            inc al
            cmp ah, 0
            je Knight
            cmp al, 9
            je Knight
            cmp [bx], dh
            jne I_C_Found_piece4
            add bx, 14 ; row ela square
            jmp I_C_Check_Diag_leftP1_down

        I_C_Found_piece4:
            mov dh, [di]
            cmp [bx], dh
            je Knight ;move rook end
            mov al, '2'
            cmp [bx+1], al
            je Incheck_Mid2 
            mov al, '3'
            cmp [bx+1], al
            je Incheck_Mid2 


    Knight:
    ;=======================
    ;Knight Moves
    ;=======================
        mov dx,cx
        mov si,di
        mov al,[Di]
        mov ah, '0'
        ;push cx
        ; Check up1 right
        sub si, 12 ; go up row ela 2 squares 
        cmp cl, 1
        je ICK_next4_mid
        cmp ch, 7
        jae ICK_next1
        dec cl
        add ch, 2
        cmp [si], ah
        jne ICK_CheckAttack1
        jmp ICK_next1

        ICK_CheckAttack1:
        cmp [si], al
        je ICK_next1
        mov al, '1'
        cmp [si+1], al
        je Incheck_Mid3 
        ;jmp ICK_next1

        ;up1 left
        ICK_next1:
        mov cx,dx
        sub si, 8

        cmp ch, 2
        jbe ICK_next2
        dec cl
        sub ch, 2
        cmp [si], ah
        jne ICK_CheckAttack2
        jmp ICK_next2
        
        ICK_CheckAttack2:
        mov al,[Di]
        cmp [si], al
        je ICK_next2
        mov al, '1'
        cmp [si+1], al
        je Incheck_Mid3 

        ICK_next4_mid: 
        sub si, 18
        jmp ICK_next4

        ;up2 left
        ICK_next2:
        mov cx,dx
        sub si, 14

        cmp cl, 2
        je ICK_next3
        cmp ch, 1
        je ICK_next3
        sub cl, 2
        dec ch
        cmp [si], ah
        jne ICK_CheckAttack3
        jmp ICK_next3

        ICK_CheckAttack3:
        mov al,[Di]
        cmp [si], al
        je ICK_next3
        mov al, '1'
        cmp [si+1], al
        je Incheck_Mid3 

        ;up2 right
        ICK_next3:
        mov cx,dx
        add si, 4

        cmp cl, 2
        je ICK_next4
        cmp ch, 8
        je ICK_next4
        sub cl, 2
        inc ch
        cmp [si], ah
        jne ICK_CheckAttack4
        jmp ICK_next4

        ICK_CheckAttack4:
        mov al,[Di]
        cmp [si], al
        je ICK_next4
        mov al, '1'
        cmp [si+1], al
        je Incheck_Mid3 

         ;==================
            jmp ICK_next4
            Incheck_Mid3: jmp far ptr InCheck
        ;==================

        ;==================
        ;down
        ;down1 right
        ICK_next4:
        mov cx,dx
        add si, 50 
                
        cmp cl, 8
        je ICK_next5
        cmp ch, 7
        jae ICK_next5
        inc cl
        add ch, 2
        cmp [si], ah
        jne ICK_CheckAttack5
        jmp ICK_next5

        ICK_CheckAttack5:
        mov al,[Di]
        cmp [si], al
        je ICK_next5
        mov al, '1'
        cmp [si+1], al
        je Incheck_Mid4 

        ;down 1 left
        ICK_next5:
        mov cx,dx
        sub si, 8

        cmp ch, 2
        jbe ICK_next6
        inc cl
        sub ch, 2
        cmp [si], ah
        jne ICK_CheckAttack6
        jmp ICK_next6

        ICK_CheckAttack6:
        mov al,[Di]
        cmp [si], al
        je ICK_next6
        mov al, '1'
        cmp [si+1], al
        je Incheck_Mid4 

        jmp ICK_next6
        Incheck_Mid4: jmp far ptr InCheck
        
        ;down2 left
        ICK_next6:
        mov cx,dx
        add si, 18

        cmp cl, 7
        jae ICK_next7
        cmp ch, 1
        je ICK_next7
        add cl,2
        dec ch
        cmp [si], ah
        jne ICK_CheckAttack7
        jmp ICK_next7

        ICK_CheckAttack7:
        mov al,[Di]
        cmp [si], al
        je ICK_next7
        mov al, '1'
        cmp [si+1], al
        je Incheck_Mid4 

        ;down2 right    
        ICK_next7:
        mov cx,dx
        add si, 4


        cmp ch, 8
        je Pawn
        add cl,2
        inc ch
        cmp [si], ah
        jne ICK_CheckAttack8
        jmp Pawn

        ICK_CheckAttack8:
        mov al,[Di]
        cmp [si], al
        je Pawn
        mov al, '1'
        cmp [si+1], al
        je InCheck 

    Pawn:
    ;=======================
    ;Pawn Moves
    ;=======================
        mov cx,dx
        mov al, 'W'
        cmp [di], al
        jne BPwn_Moves1
        cmp cl, 2
        jbe NoCheck

        ;white king check for black pawns
        W_Check_Attacks1:
        mov al,'B'
        sub di, 14
        inc ch
        cmp ch,9
        je IC_skip1
        cmp [di], al
        jne IC_skip1
        mov al,'5'
        cmp [di+1], al
        je InCheck

        IC_skip1:
        mov al,'B'
        sub ch,2
        cmp ch,0
        je NoCheck
        sub di, 4
        cmp [di], al
        jne NoCheck
        mov al,'5'
        cmp [di+1], al
        je InCheck
        jmp NoCheck

        BPwn_Moves1:

        cmp cl,7
        jae NoCheck


        B_Check_Attacks1:
        mov al,'W'
        add di,2
        inc ch
        cmp ch,9
        je IC_skip2
        cmp [di], al
        jne IC_skip2
        mov al,'5'
        cmp [di+1], al
        je InCheck
        IC_skip2:
        mov al,'W'
        sub ch,2
        cmp ch,0
        je NoCheck
        sub di, 4
        cmp [di], al
        jne NoCheck
        mov al,'5'
        cmp [di+1], al
        je InCheck  


    jmp NoCheck
    InCheck:
        ; mov ah,2
        ; mov dl, '8'
        ; int 21h
        popa
        mov al,1
        ret
    NoCheck:
        ; mov ah,2
        ; mov dl, '9'
        ; int 21h
        popa
        mov al,0
        ret
Is_Check ENDP

;description
UpdateCheck PROC
           mov dl, 13
        mov ah,2       
        int 21h

        mov ch, W_King_X
        mov cl, W_King_Y 
        call Is_Check
        cmp al,1
        jne NotCheck1

        mov ah,2
        mov dl, 'W'
        int 21h
        mov ah,2
        mov dl, '1'
        int 21h
        jmp MovePiece1
        
        NotCheck1:
            mov ah,2
            mov dl, 'W'
            int 21h
            mov ah,2
            mov dl, '0'
            int 21h

        MovePiece1:
        ;===========Black king is in check??==================
        mov ch, B_King_X
        mov cl, B_King_Y 
        call Is_Check

        cmp al,1
        jne NotCheck3

        mov ah,2
        mov dl, 'B'
        int 21h
        mov ah,2
        mov dl, '1'
        int 21h

        ret

        NotCheck3:
        mov ah,2
        mov dl, 'B'
        int 21h
        mov ah,2
        mov dl, '0'
        int 21h
    ret
UpdateCheck ENDP

;this function gets the time since the last frame / call in 1/100 s
; returned in ax & frame
GetFrameTime PROC far
    mov ah, 2Ch    
    int 21h
    mov al,dh ; move el seconds
    mov ah,0
    Calculate:
        mov bl, 100
        mul bl
        mov bl, dl
        mov bh, 0
        add ax , bx
        mov bx, ax        
    
        cmp dh, prevs
        jae DontAddMin
        
        AddMin:
            add ax, 6000
        DontAddMin:            
        sub ax, Prevtime 

        mov frame, al
        mov Prevtime, bx   
        mov prevs, dh
        ret
GetFrameTime ENDP
END ;MAIN