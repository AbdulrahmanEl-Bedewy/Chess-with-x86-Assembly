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
EXTRN DrawCooldown:FAR
EXTRN DrawRightPiece:FAR

EXTRN name1:byte
EXTRN name2:byte
EXTRN CursorColor:byte
EXTRN MovesColor:byte
EXTRN AttackColor:byte
;EXTRN Moves_bishop:FAR
Public to_idx
; Public GameScreenLocal
Public GameScreenMulti  
Public SendByte
Public ReceiveByte
Public PrintNumber

Public chessBoard
Public ValidMoves
Public ValidAttacks
Public ValidMoves2
Public ValidAttacks2
Public B_DeadPiece
Public W_DeadPiece
Public px
Public py
Public Player
Public Mode
Public RMsg
Public SMsg
include Macro.inc
.286
.Model small
.Stack 400h
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
counter db 0
seconds db 0
minutes db 0 

CoolDownPieces dw 64 dup(0), '$'
;AnimateArray db 10 dup('$$$$$$$$');00cur pos 00;end pos 0;timer 00;piece symbol
AnimateArray db 10 dup('$$$$$$$');00cur pos 00;end pos 0;timer 00;el symbool
Winner db 0 ;0 no winner 1 i won 2 i lose 3 other player left

; You_Lose_Message db " Black Wins ";'Black Wins'
; You_Win_Message db " White Wins ";'White Wins'
You_Win_Message db " You Win ";'Black Wins' 9
You_Lose_Message db " You Lose ";'White Wins' 10
Player_Left_Message db " Your opponent has left the game ";'White Wins' 33

BTag db "Black:"
WTag db "White:"

IncheckMsg db "Check"
EmptyMsg db "     "

RMsg db 4 dup(9)
SMsg db 4 dup(9)

Player db 'W' ; dh el by7aded whether im player1 or player 2
Mode db 0 ; dh el by7aded whether im player1 or player 2

IX db 0
IY db 15d

OX db 33d
OY db 15d


.Code

;The main game loop for local mode
; GameScreenLocal PROC FAR
;     MOV AX , @DATA
;     MOV DS , AX
    
    
;     call InitBoard
;     call far ptr InitGame


;     mov ah, 2Ch    
;     int 21h
;     mov al,dh ; move el seconds
;     mov ah,0

;     mov bl, 100
;     mul bl
;     mov bl, cl
;     mov bh, 0
;     add ax , bx
;     mov bx, ax
;     mov Prevtime, bx
;     mov prevs, dh
;     ;======================================================================================
;     ;MAIN GAME LOOP
;     GameLP:
;         call far ptr GetFrameTime
;         pusha
;         call Animate
;         popa
        
;         lea bx, CoolDownPieces
;         mov dx, 300 ; => 9 sec for testing purposes. should be 3
;         mov si, 0
;         mov cx, 0
;         mov di,ax ; ax contains the frame time
;         UpdateCooldown:
;             CD_Lp:
;                 ;check if piece in this position has a cooldown if not skip it
;                 cmp [word ptr bx], si ;0
;                 je NotInCoolDown

;                 ;if piece is on cooldown check if the cooldown is done
;                 cmp [word ptr bx], dx ;300 
;                 jae DoneCooldown
                
;                 ;cooldown isn't done
;                 ;check if 37/100 sec has passed since last time, if yes then update the drawing.
;                 ; there are 8 frames for the cooldown and the cooldown duration is 3 sec 
;                 ; so each frame should be displayed for roughly 37/100 sec
;                 push dx
;                 mov ax, [word ptr bx] ; move the time that passed since start of the cooldown timer until the last loop  
;                 mov dl, 37
;                 div dl
;                 mov dh,al 
;                 ;dh now has the previous frame index

;                 ;check if enough time has passed and the frame needs to be updated
;                 ;calculate new frame index
;                 add [word ptr bx], di
;                 mov ax, [word ptr bx]
;                 mov dl, 37
;                 div dl
;                 cmp al,dh ;if frame indexes equal no need to update frame thus skip 
;                 je CD_Lp_Skip   
;                 push cx          
;                 inc ch
;                 inc cl ;drawing takes position 1 indexed. the loop is 0 indexed since its an array
;                 call RedrawBoardSq
                
;                 call DrawingChecks

;                 call DrawCooldown
;                 call RedrawPiece
;                 pop cx

;                 CD_Lp_Skip:
;                 pop dx               

;                 jmp NotInCoolDown

;                 DoneCooldown:
;                 mov [word ptr bx], si ;0
;                 call RedrawPiece

;                 NotInCoolDown:
;                 add bx, 2
;                 inc ch
;                 cmp ch,8
;                 jne CD_Lp
;                 inc cl ; if reaches 8 ie end of row
;                 mov ch,0
;                 cmp cl,8
;             jne CD_Lp


;         mov ch,px
;         mov cl,py
;         mov ah,1
;         int 16h   
;         jz GameLP1

;         mov ah,0
;         int 16h   
        
;         cmp ah, 01
;         je ending


;         ;Check if select key pressed
;         call far ptr HandleInput
;         call far ptr HandleInput2

;         cmp winner,0
;         je GameLP1
;         ; Press any key to exit
;         ; mov ah,2
;         ; mov dx,1407h
;         ; int 10h
;         cmp winner,1
;         je White_Wins
;         lea bp, You_Lose_Message
;         jmp DispMsg
;         White_Wins:
;         lea bp, You_Win_Message
;         DispMsg:
;          mov al, 1
;         mov bh, 0
;         mov bl, 4
;         mov cx, 12  
;         mov dl, 14
;         mov dh, 12
;         push ds
;         pop es
;         ;mov bp, offset msg
;         mov ah, 13h
;         int 10h
            
;         MOV AH , 0
;         INT 16h
;         ret
;         GameLP1:
;         jmp GameLP
        
;     ending:
    
;     ; ; Press any key to exit
;     ; MOV AH , 0
;     ; INT 16h
    
    
;     ; ;Change to Text MODE
;     ; MOV AH,0          
;     ; MOV AL,03h
;     ; INT 10h 

;     ; ; return control to operating system
;     ; MOV AH , 4ch
;     ; INT 21H
;     ret
; GameScreenLocal ENDP

;The main game loop for multiplayer mode
GameScreenMulti PROC
    MOV AX , @DATA
    MOV DS , AX
    
    
    call InitBoard
    call far ptr InitGame


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
    GameLPMulti:
        call far ptr GetFrameTime
        call Clock
        call ReceiveMsg
        call Animate
        
        
        lea bx, CoolDownPieces
        mov dx, 300 ; => 9 sec for testing purposes. should be 3
        mov si, 0
        mov cx, 0
        mov di,ax ; ax contains the frame time
        UpdateCooldownMulti:
            CD_LpMulti:
                ;check if piece in this position has a cooldown if not skip it
                cmp [word ptr bx], si ;0
                je NotInCoolDownMulti

                ;if piece is on cooldown check if the cooldown is done
                cmp [word ptr bx], dx ;300 
                jae DoneCooldownMulti
                
                ;cooldown isn't done
                ;check if 37/100 sec has passed since last time, if yes then update the drawing.
                ; there are 8 frames for the cooldown and the cooldown duration is 3 sec 
                ; so each frame should be displayed for roughly 37/100 sec
                push dx
                mov ax, [word ptr bx] ; move the time that passed since start of the cooldown timer until the last loop  
                mov dl, 37
                div dl
                mov dh,al 
                ;dh now has the previous frame index

                ;check if enough time has passed and the frame needs to be updated
                ;calculate new frame index
                add [word ptr bx], di
                mov ax, [word ptr bx]
                mov dl, 37
                div dl
                cmp al,dh ;if frame indexes equal no need to update frame thus skip 
                je CD_Lp_SkipMulti   
                push cx          
                inc ch
                inc cl ;drawing takes position 1 indexed. the loop is 0 indexed since its an array
                call RedrawBoardSq
                
                call DrawingChecks

                call DrawCooldown
                call RedrawPiece
                pop cx

                CD_Lp_SkipMulti:
                pop dx               

                jmp NotInCoolDownMulti

                DoneCooldownMulti:
                mov [word ptr bx], si ;0
                call RedrawPiece

                NotInCoolDownMulti:
                add bx, 2
                inc ch
                cmp ch,8
                jne CD_LpMulti
                inc cl ; if reaches 8 ie end of row
                mov ch,0
                cmp cl,8
            jne CD_LpMulti


       
        cmp winner,1
        je I_Win

        cmp winner,2
        je I_Lose

        cmp winner, 3
        je Player_Left

        mov ch,px
        mov cl,py

        mov ah,1
        int 16h   
        jz GameLP1Multi

        mov ah,0
        int 16h   
        
        cmp ah, 01
        je endingMulti


        ;Check if select key pressed
        call far ptr HandleInput
        ; call far ptr HandleInput2

        jmp GameLPMulti
        
        I_Lose:
        lea bp, You_Lose_Message
        jmp DispMsgMulti
        
        Player_Left:
        lea bp, Player_Left_Message
        mov al, 1
        mov bh, 0
        mov bl, 4
        mov cx, 33
        mov dl, 5
        mov dh, 12
        push ds
        pop es
        mov ah, 13h
        int 10h

        AwaitESC2:
        mov ah,1
        int 16h   
        jz AwaitESC2
        mov ah,0
        int 16h  
        cmp ah,1
        jne AwaitESC2
        ret
        
        I_Win:
        lea bp, You_Win_Message

        DispMsgMulti:
        mov al, 1
        mov bh, 0
        mov bl, 4
        mov cx, 10
        mov dl, 14
        mov dh, 12
        push ds
        pop es
        ;mov bp, offset msg
        mov ah, 13h
        int 10h
        


        AwaitESC:
        call Animate

        ;display win/lose msg
        mov al, 1
        mov bh, 0
        mov bl, 4
        mov cx, 10
        mov dl, 14
        mov dh, 12
        push ds
        pop es
        mov ah, 13h
        int 10h

        mov ah,1
        int 16h   
        jz AwaitESC
        mov ah,0
        int 16h  
        cmp ah,1
        jne AwaitESC
        ret
    
        GameLP1Multi:
        jmp GameLPMulti
        
    endingMulti:
    mov SMsg, 200 ;  200 for end
    lea di,SMsg
    call far ptr SendByte
    ret
GameScreenMulti ENDP

;Initializes all variables to default values
InitGame PROC Far
        pusha
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

        lea si, AnimateArray       
        mov cx,10*7
        mov ah,'$'
        CL_Animations:
            mov [si],ah
            inc si
            LOOP CL_Animations

        lea si, CoolDownPieces
        mov cx, 64
        mov ax, 0
        IG_Clear:
            mov [si], ax
            add si,2
            loop IG_Clear
         lea si, CoolDownPieces
         ;mov ax, [si + 112]

        cmp Player, 'B'
        je IAmBlack
        lea si, name1
        lea di, name2
        mov px , 1
        mov py , 8
        mov CursorColor, 68h
        mov MovesColor, 36h
        mov AttackColor, 4h
        jmp I_G_1

        IAmBlack: ;name1 ytktb 3nd el black
        lea si, name2
        lea di, name1
        mov px , 1
        mov py , 1
        mov CursorColor, 2Ah
        mov MovesColor, 2bh
        mov AttackColor, 6Bh

        I_G_1:
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
        mov winner, 0
        mov seconds,0
        mov minutes,0
        mov counter,0
        mov IX,0
        mov IY,15d
        mov OX,33
        mov OY,15d
; initialize the board and draws all pieces in place
        call InitBoard   
        DrawSq px, py
        MOV ch,px
        MOV cl,py
        call RedrawPiece
        ; DrawSq2 px2, py2
        ; MOV ch,px2
        ; MOV cl,py2
        ; call RedrawPiece
        

        PrintNames:
         mov al, 1
        mov bh, 0
        mov bl, 1Eh
        mov cx, 6
        mov dl, 0
        mov dh, 0
        push ds
        pop es
        mov bp, offset WTag
        mov ah, 13h
        int 10h

        mov dh, 1
        mov dl, 0
        mov bh, 0
        mov ah, 2
        int 10h
;white name

        mov dx, si
		mov ah, 9
		int 21h

        mov al, 1
        mov bh, 0
        mov bl, 4fh
        mov cx, 6
        mov dl, 73
        mov dh, 0
        push ds
        pop es
        mov bp, offset BTag
        mov ah, 13h
        int 10h

        mov dh, 1
        mov dl, 73
        mov bh, 0
        mov ah, 2
        int 10h
     ;black name   
        mov dx, di
		mov ah, 9
		int 21h

        call UpdateCheck
        popa
        ret
InitGame ENDP


;white / should handle input for black and white but in single player mode in phase 2,
; just need to uncomment some parts. for now it handles white player's input
HandleInput PROC Far   ; the user input is in ax => al:ascii ah:scan code
                    ;Handles all game logic
                    ;Including:
                    ;   1-Check for input
                    ;   2-Move piece
                    ;   3-Getting available moves
    ; MOV ch,px
    ; MOV cl,py
    ; cmp al,'q'
    ; je Player1
    ; cmp al,'d'
    ; je RightP1
    ; cmp al,'a'
    ; je LeftP1
    ; cmp al,'w'
    ; je upP1
    ; cmp al,'s'
    ; je downP1

    cmp ah, 52h
    je Player1
    cmp ah, 4Dh
    je RightP2
    cmp ah, 4Bh
    je LeftP2
    cmp ah, 48h
    je upP2
    cmp ah, 50h
    je downP2
    mov SMsg,al
    lea di,SMsg
    call far ptr SendByte
    mov dh,IY
    mov dl,IX
    mov bh,0
    mov ah,2
    int 10h
    pusha
    call WRITEINPUT
    popa
    ret
;==================================
;This part is responsible for updating the selector position on key press
    ; RightP1:  
    ; cmp px,8
    ; je Return
    ; add px,1
    ; jmp Draw_Highlighted
    ; LeftP1:
    ; cmp px,1
    ; je Return
    ; sub px,1
    ; jmp Draw_Highlighted
    ; upP1:
    ; cmp py,1
    ; je Return
    ; sub py,1
    ; jmp Draw_Highlighted
    ; downP1:
    ; cmp py,8
    ; je Return
    ; add py,1

    RightP2:  
    cmp px,8
    je Return
    add px,1
    jmp Draw_Highlighted
    LeftP2:
    cmp px,1
    je Return
    sub px,1
    jmp Draw_Highlighted
    upP2:
    cmp py,1
    je Return
    sub py,1
    jmp Draw_Highlighted
    downP2:
    cmp py,8
    je Return
    add py,1


    jmp Draw_Highlighted
    select_mid:jmp select

    Return:
        ret

    Player1:
    mov dl, Player ; W indicates that it is player one that is selecting
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


        ; cmp hx2,0
        ; je skip2
        ; ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
        ; DrawSq2 hx2, hy2
        ; mov ch, hx2
        ; mov cl, hy2
        ; call RedrawPiece
        ; skip2:
       
        cmp hx,0
        je skip
        ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
        DrawSq hx, hy
        mov ch, hx
        mov cl, hy
        call RedrawPiece
        skip:

        ; mov ch,px2
        ; mov cl,py2
        ; DrawSq2 px2, py2 ;draw higlighting of new square
        ; call RedrawPiece ;
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

            ; send the move i just made to the other player
            lea di,SMsg
            mov cl,hx
            mov [di], cl
            mov cl,hy
            mov [di+1], cl
            mov cl,px
            mov [di+2], cl
            mov cl,py
            mov [di+3], cl
            call SendMsg

        call Move_Piece

        jmp H_I_ClearValidLists
        
        Check_Valid_Attack:
            lea si, ValidAttacks
            call List_Contains
            cmp al,0 ; selected pos is not a valid attack 
            jne Skip_Check_Empty
            mov al, '0'
            cmp [di], al ; check if position is empty and not valid move or attack
            je H_I_ClearValidLists_Mid
            jmp Sel
            ;============================
            H_I_ClearValidLists_Mid:
            jmp H_I_ClearValidLists
            ;============================

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
            


            mov bl,'4'  ; king number ex B4 is black king
            cmp [di+1],bl
            jne HI_AppendDeadPiece
            mov Winner,1 ;winner 0 => no winner 1 => I win 2 => Other player wins

            HI_AppendDeadPiece:
                mov bl,'$'  ;get last element in array to append at the end
                sub si,2
                GetEnd:add si,2
                    cmp [si],bl
                    jne GetEnd
                mov bh,[di]
                mov bl,[di+1]
                mov [si],bh
                mov [si+1],bl
            call DrawDeadP
        pusha
         ; send the move i just made to the other player
            lea di,SMsg
            mov cl,hx
            mov [di], cl
            mov cl,hy
            mov [di+1], cl
            mov cl,px
            mov [di+2], cl
            mov cl,py
            mov [di+3], cl
            call SendMsg
        popa
        
            call Move_Piece 
            
            ; cmp hx2,0
            ; je H_I_ClearValidLists
            
            ; mov ch, px
            ; mov cl, py
            ; cmp hx2,ch
            ; jne G3
            ; cmp hy2,cl
            ; jne G3
            ; DeselectPlayer2
            ; G3:
            ; call ClearValidLists2

            ; mov ch,hx2
            ; mov cl,hy2
            ; call GetValidMoves
            ;call DrawPossibleMoves
;==================================
;This part is responsible for re-inializing valid attack/move lists and drawing the updates of attacking/moving 
        H_I_ClearValidLists:
            call ClearValidLists

            ; cmp hx2,0
            ; je HI_S1
            ; DrawSq2 hx2,hy2 ;ba redraw el background of el selected piece bta3t el team el tany 
            ;                 ;34an lw kan selected w kan valid attack lama ams7 el attack el selector maytms74
            ; mov ch,hx2
            ; mov cl,hy2
            ; call RedrawPiece
            ; HI_S1:         

            DeselectPlayer1
            ; DrawSq2 px2,py2
            ; call DrawPossibleMoves
            ; call DrawPossibleAttacks
            ; call DrawDeadP
            ret

;==================================
;This part is responsible for selecting a new piece and drawing its valid moves & attacks 
        Sel:
            mov ch, px
            mov cl, py
            call to_idx
            cmp [di], dl ;dl has the player type W:white or B:black
            je Valid_Sel
            ; should Deslect player1 if Q is pressed and Deselect player2 when Space is pressed
            DeselectPlayer1

            call ClearValidLists
            call DrawPossibleAttacks       

            ret
            Valid_Sel:
            ;Make sure the piece to be selected isn't on cooldown
                ;Calculating the current piece's position in the piece cooldown array
                mov al, px
                dec al ; cause the cooldown array is 0 indexed
                mov ah,0
                mov bl, 2
                mul bl
                mov bh, al
                
                mov al, py
                dec al ; cause the cooldown array is 0 indexed
                mov ah,0
                mov bl, 16
                mul bl

                add al, bh
                mov si, ax

                ;checking if the piece is on cooldown. 0 ==> no cooldown  otherwise ==> on cooldown
                mov ax, 0
                lea bx, CoolDownPieces
                cmp [word ptr bx + si], ax
                je HI_SelectNewPiece
                DeselectPlayer1

                call ClearValidLists

                ret
            HI_SelectNewPiece:
            mov ch, hx
            mov cl, hy
            call RedrawBoardSq
            call RedrawPiece

            mov ch, px
            mov cl, py

            mov hx, ch
            mov hy, cl 
            call ClearValidLists

            mov ch, px
            mov cl, py
            call GetValidMoves
            call DrawPossibleMoves
            call DrawPossibleAttacks
            
            ; DrawSq2 px2,py2
            ; mov ch, px2
            ; mov cl, py2
            ; call RedrawPiece

            ; cmp hx2,0
            ; je HI_S2
            ; DrawSq2 hx2,hy2
            ; mov ch,hx2
            ; mov cl,hy2
            ; call RedrawPiece
            ; HI_S2:
            ret    
HandleInput ENDP

;black
; HandleInput2 PROC Far   ; the user input is in ax => al:ascii ah:scan code
;                     ;Handles all game logic
;                     ;Including:
;                     ;   1-Check for input
;                     ;   2-Move piece
;                     ;   3-Getting available moves

                     
;     MOV ch,px2
;     MOV cl,py2


;     cmp ah, 39h ;space
;     je Player2 
;     cmp ah, 4Dh ;right arrow
;     je RightP2
;     cmp ah, 4Bh ;left arrow
;     je LeftP2
;     cmp ah, 48h ;up arrow
;     je upP2
;     cmp ah, 50h ;down arrow
;     je downP2
    
;     ret
; ;==================================
; ;This part is responsible for updating the selector position on key press
;     RightP2:  
;     cmp px2,8
;     je Return2
;     add px2,1
;     jmp Draw_Highlighted2
;     LeftP2:
;     cmp px2,1
;     je Return2
;     sub px2,1
;     jmp Draw_Highlighted2
;     upP2:
;     cmp py2,1
;     je Return2
;     sub py2,1
;     jmp Draw_Highlighted2
;     downP2:
;     cmp py2,8
;     je Return2
;     add py2,1


;     jmp Draw_Highlighted2
;     select_mid2:jmp select2

;     Return2:
;         ret

;     Player2:

;     mov dl, Player ; B indicates that it is player two that is selecting
;     jmp select2
; ;==================================
; ;This part is responsible for drawing selector position update
;     Draw_Highlighted2:
;         call RedrawBoardSq ; redaraw the current as a normal board square (not higlighted) before moving 
;         call DrawPossibleMoves ; need to redraw possible moves so that it is not erased if selector gets on a valid move's square
;         call DrawPossibleAttacks ; need to redraw possible moves so that it is not erased if selector gets on a valid attack's square
;         call RedrawPiece ; redraw piece if any at the old location

        

;         cmp hx,0
;         je skip4
;         ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
;         DrawSq hx, hy
;         mov ch, hx
;         mov cl, hy
;         call RedrawPiece
;         skip4:

;         cmp hx2,0
;         je skip3
;         ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
;         DrawSq2 hx2, hy2
;         mov ch, hx2
;         mov cl, hy2
;         call RedrawPiece
;         skip3:

;         mov ch,px
;         mov cl,py
;         DrawSq px, py ;draw higlighting of new square
;         call RedrawPiece
;         mov ch,px2
;         mov cl,py2
;         DrawSq2 px2, py2 ;draw higlighting of new square
;         call RedrawPiece ;
;         ; mov ch,px2
;         ; mov cl,py2
;         ; DrawSq px2, py2 ;draw higlighting of new square
;         ; call RedrawPiece ; 
;         ret
; ;==================================
; ;This part is responsible for moving and attacking logic
;     select2:
        
;     ;check if valid move
;         mov ch,px2
;         mov cl,py2
;         call to_idx
;         lea si,ValidMoves2
;         call List_Contains
;         cmp al,0 ; selected pos is not a valid move
;         je Check_Valid_Attack2
;         call Move_Piece2
    

;         ; call ClearValidLists
;         ; mov ch,hx
;         ; mov cl,hy
;         ; call GetValidMoves

;         jmp H_I_ClearValidLists2
        
;         Check_Valid_Attack2:
;             lea si, ValidAttacks2
;             call List_Contains
;             cmp al,0 ; selected pos is not a valid attack 
;             jne Skip_Check_Empty2
;             ; mov al, '0'
;             ; cmp [di], al ; check if position is empty and not valid move or attack
;             ; je H_I_ClearValidLists_Mid2
;             jmp Sel2
;             ;============================
;             H_I_ClearValidLists_Mid2:
;             jmp H_I_ClearValidLists2
;             ;============================

;             Skip_Check_Empty2:
;             mov al, 'B'
;             cmp [di], al
;             je kill_Black2
;             ;kill white
;             lea si, W_DeadPiece
;             jmp Kill_Piece2
;             kill_Black2:
;             lea si, B_DeadPiece
;             Kill_Piece2:


;             mov bl,'4'
;             cmp [di+1],bl
;             jne HI_AppendDeadPiece2
;             mov Winner,2 ;winner 0 => no winner 1 => white wins 2 => Black wins


;             HI_AppendDeadPiece2:


;             mov bl,'$'  ;get last element in array to append at the end
;             sub si,2
;             GetEnd2:add si,2
;                 cmp [si],bl
;                 jne GetEnd2
;             mov bh,[di]
;             mov bl,[di+1]
;             mov [si],bh
;             mov [si+1],bl
;             call Move_Piece2 

;             cmp hx,0
;             je H_I_ClearValidLists2
            
;             mov ch, px2
;             mov cl, py2
;             cmp hx,ch
;             jne G4
;             cmp hy,cl
;             jne G4
;             DeselectPlayer1
;             G4:
;             ; call ClearValidLists

;             ; mov ch,hx
;             ; mov cl,hy
;             ; call GetValidMoves
;             ;call DrawPossibleMoves
                       
; ;==================================
; ;This part is responsible for re-inializing valid attack/move lists and drawing the updates of attacking/moving 
;         H_I_ClearValidLists2:
;             call ClearValidLists2
            
;             cmp hx,0
;             je HI_S3
;             DrawSq hx,hy ;lw player el tany kan 3aml select 7aga haroo7 arsm el selector tany
;             mov ch,hx
;             mov cl,hy
;             call RedrawPiece
    

;             HI_S3:
;             DeselectPlayer2
;             DrawSq px,py
;             ; call DrawPossibleMoves
;             ; call DrawDeadP
    
;             ret

; ;==================================
; ;This part is responsible for selecting a new piece and drawing its valid moves & attacks 
;         Sel2:

;             mov ch, px2
;             mov cl, py2
;             call to_idx

;             cmp [di], dl
;             je Valid_Sel2
;             ; should Deslect player1 if Q is pressed and Deselect player2 when Space is pressed
;             DeselectPlayer2
;             call ClearValidLists2 
;             call DrawPossibleAttacks

;             ret

;             Valid_Sel2:
;             ;Make sure the piece to be selected isn't on cooldown
;                 ;Calculating the current piece's position in the piece cooldown array
;                 mov al, px2
;                 dec al ; cause the cooldown array is 0 indexed
;                 mov ah,0
;                 mov bl, 2
;                 mul bl
;                 mov bh, al
                
;                 mov al, py2
;                 dec al ; cause the cooldown array is 0 indexed
;                 mov ah,0
;                 mov bl, 16
;                 mul bl

;                 add al, bh
;                 mov si, ax

;                 ;checking if the piece is on cooldown. 0 ==> no cooldown  otherwise ==> on cooldown
;                 mov ax, 0
;                 lea bx, CoolDownPieces
;                 cmp [word ptr bx + si], ax
;                 je HI_SelectNewPiece2
;                 DeselectPlayer2
;                 call ClearValidLists2
;                 ret
;             HI_SelectNewPiece2:

;             mov ch, hx2
;             mov cl, hy2
;             call RedrawBoardSq
;             call RedrawPiece

;             mov ch, px2
;             mov cl, py2

;             mov hx2, ch
;             mov hy2, cl 
;             call ClearValidLists2

;             mov ch, px2
;             mov cl, py2
;             call GetValidMoves
;             call DrawPossibleMoves
;             call DrawPossibleAttacks

;             DrawSq px,py
;             mov ch, px
;             mov cl, py
;             call RedrawPiece

;             cmp hx,0
;             je HI_S4
;             DrawSq hx,hy
;             mov ch,hx
;             mov cl,hy
;             call RedrawPiece
;             HI_S4:
;             ret    
; HandleInput2 ENDP

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
    
    ; mov al, px
    ; dec al
    ; mov ah,0
    ; mov bl, 2
    ; mul bl
    ; mov bh, al
         
    ; mov al, py
    ; dec al
    ; mov ah,0
    ; mov bl, 16
    ; mul bl     

    ; add al, bh
    ; mov si, ax
    ; mov ax, 0
    ; lea bx, CoolDownPieces

    ; mov ax,1
    ; mov [word ptr bx + si], ax



    MP_CheckKing:
    


    ;check if the piece is a king then update the king's position variable
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

    mov al,'B'
    cmp Player, al
    je Black_King
    mov W_King_X,ch
    mov W_King_Y,cl
    jmp SkipKingUpdate

    Black_King:
    mov B_King_X,ch
    mov B_King_Y,cl
    SkipKingUpdate: 
    

    MovePiece2:
    ;get the index of the destination, in chessboard array
    mov ch,px
    mov cl,py
    call to_idx
    ;get the index of the selected piece to be moved, in chessboard array
    mov bx,di
    mov ch,hx
    mov cl,hy
    call to_idx
    ;moving the piece's code to the destination in chessboard array and moving '00' to the original position
    mov dx,[di]
    ;mov dx,cx
    mov ax,3030h
    mov [bx],ax
    mov [di],ax

    ;mov di,bx
    lea bx, AnimateArray
    mov al,'$'
    MP_LP:
        cmp [bx], al
        je MP_Append
        add bx,7
        jmp MP_LP    
    MP_Append:
    ; ;00cur pos 00;end pos 0;timer 00;piece symbol    
    ;mov ah, '$'
    ; mov cl,[di]
    ; mov ch,[di+1]
    mov [bx+5], dl
    mov [bx+6], dh

    mov ch,hx
    mov cl,hy
    mov [bx], ch
    mov [bx+1], cl

    mov ch,px
    mov cl,py
    mov [bx+2], ch
    mov [bx+3], cl
    mov ah,0
    mov [bx+4], ah

 
    ret
Move_Piece ENDP

Move_Piece2 PROC

    ; mov al, px2
    ; dec al
    ; mov ah,0
    ; mov bl, 2
    ; mul bl
    ; mov bh, al
         
    ; mov al, py2
    ; dec al
    ; mov ah,0
    ; mov bl, 16
    ; mul bl     

    ; add al, bh
    ; mov si, ax
    ; mov ax, 0
    ; lea bx, CoolDownPieces

    ; mov ax,1
    ; mov [word ptr bx + si], ax


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

    mov al,'B'
    cmp Player, al
    jne Black_King2
    mov W_King_X,ch
    mov W_King_Y,cl
    jmp SkipKingUpdate2

    Black_King2:
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

    mov dx,[di]
    ;mov dx,cx
    mov ax,3030h
    mov [bx],ax
    mov [di],ax



    ;mov di,bx
    lea bx, AnimateArray
    mov al,'$'
    MP_LP2:
        cmp [bx], al
        je MP_Append2
        add bx,7
        jmp MP_LP2    
    MP_Append2:
    ; ;00cur pos 00;end pos 0;timer 00;piece symbol    
    ;mov ah, '$'
    ; mov cl,[di]
    ; mov ch,[di+1]
    mov [bx+5], dl
    mov [bx+6], dh

    mov ch,hx2
    mov cl,hy2
    mov [bx], ch
    mov [bx+1], cl

    mov ch,px2
    mov cl,py2
    mov [bx+2], ch
    mov [bx+3], cl
    mov ah,0
    mov [bx+4], ah

    
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


    call to_idx
    mov bl, '0'

    ;Check if multiplayer then always load the moves in ValidMoves
    ; cmp Mode, 1
    ; jne LocalGame

    mov dx,0
    lea si, ValidMoves
    ;lea bx, ValidAttacks


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

    lea bx, ValidMoves
    lea si, ValidAttacks


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


    lea bx, ValidMoves
    lea dx, ValidAttacks


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
            mov al, '4'
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
            mov al, '4'
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
            mov al, '4'
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
            mov al, '4'
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
            mov al, '4'
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
            mov al, '4'
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
            mov al, '4'
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
            mov al, '4'
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
        add di, 16

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

;Checks if a king is in check and prints a notification fo the players
UpdateCheck PROC
    pusha
            mov dl, 13
            mov ah,2       
            int 21h

            mov ch, W_King_X
            mov cl, W_King_Y 
            call Is_Check
            cmp al,1
            jne NotCheck1

            mov al, 1
            mov bh, 0
            mov bl, 1Eh
            mov cx, 5
            mov dl, 0
            mov dh, 23
            push ds
            pop es
            mov bp, offset IncheckMsg
            mov ah, 13h
            int 10h

            jmp MovePiece1
            
            NotCheck1:
               mov al, 1
            mov bh, 0
            mov bl, 1Eh
            mov cx, 5
            mov dl, 0
            mov dh, 23
            push ds
            pop es
            mov bp, offset EmptyMsg
            mov ah, 13h
            int 10h

            MovePiece1:
            ;===========Black king is in check??==================
            mov ch, B_King_X
            mov cl, B_King_Y 
            call Is_Check

            cmp al,1
            jne NotCheck3

            mov al, 1
            mov bh, 0
            mov bl, 4fh
            mov cx, 5
            mov dl, 73
            mov dh, 23
            push ds
            pop es
            mov bp, offset IncheckMsg
            mov ah, 13h
            int 10h
                 
            POPA
            ret

            NotCheck3:
            mov al, 1
            mov bh, 0
            mov bl, 4fh
            mov cx, 5
            mov dl, 73
            mov dh, 23
            push ds
            pop es
            mov bp, offset EmptyMsg
            mov ah, 13h
            int 10h  

    popa
        
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

ClearValidLists proc 
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
    ; DrawSq px,py
    ; mov ch,px
    ; mov cl,py
    ; call RedrawPiece
    

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

    ; DrawSq2 px2,py2
    ; mov ch,px2
    ; mov cl,py2
    ; call RedrawPiece

    ret
ClearValidLists ENDP

ClearValidLists2 proc 
    

    lea si,ValidMoves2
    mov al,'$'
    Clear3:
        cmp [si], al
        je Cleared3
        mov ch, [si]
        mov cl, [si+1]
        call RedrawBoardSq
        ;  call RedrawPiece
        mov [si], al
        mov [si+1], al
        add si,2
        jmp Clear3
    Cleared3:
    ; DrawSq2 px2,py2
    ; mov ch,px2
    ; mov cl,py2
    ; call RedrawPiece


    lea si, ValidAttacks2
    mov al,'$'
    Clear4:
        cmp [si], al
        je Cleared4
        mov ch, [si]
        mov cl, [si+1]
        call RedrawBoardSq
        call RedrawPiece
        mov [si], al
        mov [si+1], al
        add si,2
        jmp Clear4
    Cleared4:
    DrawSq2 px2,py2
    mov ch,px2
    mov cl,py2
    call RedrawPiece
    DrawSq px,py
    mov ch,px
    mov cl,py
    call RedrawPiece
    ret
ClearValidLists2 ENDP

;Utility function used in GameScreen proc. to draw the required square when a piece is in cooldown
DrawingChecks PROC
    pusha
    ;check if current position has one of the selectors or both
    ; if so then draw it
        cmp ch, px
        jne GS_CheckPX2
        cmp cl, py
        jne GS_CheckPX2
        DrawSq px , py
        ;jmp GS_CheckAttack

        GS_CheckPX2:

        ; cmp ch, px2
        ; jne GS_CheckAttack
        ; cmp cl, py2
        ; jne GS_CheckAttack
        ; DrawSq2 px2 , py2
        ;jmp GS_CheckAttack

        ;check if the current position is a valid attack for either black or white
        ;if so then draw the corresponding attack color
        GS_CheckAttack:
        lea si,ValidAttacks
        call List_Contains

        cmp al, 1
        je Drawattack_1
        jmp GS_Skip1
        ; lea si,ValidAttacks2
        ; call List_Contains

        ; cmp al, 0
        ; je GS_Skip1
        ; mov bl,ch
        ; mov al,cl
        ; mov dl, 6Bh
        ; call DrawSquare
        ; popa
        ; ret
        Drawattack_1:
        mov bl,ch
        mov al,cl
        mov dl, AttackColor
        call DrawSquare

    GS_Skip1:
    popa
    ret
DrawingChecks ENDP

PrintNumber proc   
    pusha
       mov bl,10
       ; mov al,ans
        mov ah,0
        div bl
        push ax
        mov ah,0
        div bl  
        
        mov dl, ah
        mov ah,2       
        add dl,48
        int 21h      
        
        
        pop ax
        mov dl, ah
        mov ah,2       
        add dl,48
        int 21h  
 
   popa  
   ret
PrintNumber ENDP

;takes al = frame time
Animate PROC
    pusha
    ; ;00cur pos 00;end pos ;0timer ;00 symbol
    lea bx, AnimateArray
    mov si,10
    A_lp:
        mov ah, '$'
        cmp [bx], ah
        je A_B
        ;jmp UPDATERZ
        add [bx+4], al
        mov ah, 5 ; the time a moving piece will stay aat each square 
        cmp [bx+4], ah
        jae UPDATERZ
        A_B:
        add bx,7
        dec si
        jnz A_lp
        jmp a_end
        UPDATERZ:
    ;      pusha
    ; mov ah,2
    ; mov dl, '1'
    ; int 21h         
    ; popa

        mov ch, [bx] ; ba7ot el current position abl ma a3mlo increment for later comparisions
        mov cl, [bx+1]
        
        mov ah,1
        cmp ch,[bx+2]
        jb INCX
        je Checks_El_Y

        sub [bx],ah
        jmp Checks_El_Y
        INCX:
        add [bx],ah 

        Checks_El_Y:
        cmp cl,[bx+3]
        jb INCY
        je Drawingkda

        sub [bx+1],ah
        jmp Drawingkda
        INCY:
        add [bx+1],ah 


        Drawingkda:
        call RedrawBoardSq
        call RedrawPiece
        mov ch, [bx]
        mov cl, [bx+1]
        mov di, bx
        add di,5
        call DrawRightPiece

        cmp ch,[bx+2]
        jne LKMID
        
        jmp checky
        LKMID:
        jmp LK

        checky:
        cmp cl, [bx+3]
        jne LKMID
        
        ; 7ot el symbol bta3 el piece fl chessboard array at el endpos lama twsl
        call to_idx
        mov dl,[bx+5]
        mov dh,[bx+6]
        mov [di],dl
        mov [di+1],dh

    ;      pusha
    ; mov ah,2
    ; mov dl, [bx]
    ; add dl,48
    ; int 21h         
    ; mov ah,2
    ; mov dl, [bx+1]
    ; add dl,48
    ; int 21h         
    ; popa

pusha 
    ;=========================
    ;setting the cooldown
    mov al, [bx+2]
    dec al
    mov ah,0
    mov cl, 2
    mul cl
    mov ch, al

    mov al, [bx+3]
    dec al
    mov ah,0
    mov cl, 16
    mul cl     

    add al, ch
    mov si, ax
    mov ax, 0
    lea bx, CoolDownPieces
    mov ax,1
    mov [word ptr bx + si], ax

    ;==========================
    ;refreshing the valid moves of the enemy 
    ; call ClearValidLists2
    ; mov ch,hx2
    ; mov cl,hy2
    ; call GetValidMoves

    ; mov ch, px2
    ; mov cl, py2
    ; cmp hx,ch
    ; jne G4
    ; cmp hy,cl
    ; jne G4
    ; DeselectPlayer1
    ; G4:
    ; ; call ClearValidLists

    ; call ClearValidLists
    ; mov ch,hx
    ; mov cl,hy
    ; call GetValidMoves

    ; call DrawPossibleMoves
    ; call DrawPossibleAttacks
    ; call DrawDeadP
        call ClearValidLists

    ; mov ch,hx
    ; mov cl,hy
    cmp hx,0
    je Skipat
    mov ch,hx
    mov cl,hy
    call GetValidMoves

    call DrawPossibleMoves
    call DrawPossibleAttacks
    Skipat:
;    call DrawDeadP
    

popa
;update the checkmates as the movement may cause any changes
    
    ;if there was a selected piece by the opponent update its valid-moves&attacks as the movement may cause any changes

        call UpdateCheck
    
        mov ah,'$'
        mov [bx],ah
        mov [bx+1],ah
        mov [bx+2],ah
        mov [bx+3],ah
        mov [bx+4],ah
        mov [bx+5],ah
        mov [bx+6],ah

        add bx,7
        dec si
        jnz A_LPMid
        jmp a_end
        
        LK:
        mov ah,0
        mov [bx+4],ah
        add bx,7
        dec si
        jnz A_LPMid

        jmp A_End
        A_LPMid: jmp A_lp

    

    A_End:
    popa
    ret
Animate ENDP


;Recieves 1 byte and puts it in di
;9 -> didnt recieve 7aga 
ReceiveByte PROC far
    pusha
        ;Check that Data Ready
            mov dx , 3FDH		; Line Status Register
    	    in al , dx 

            AND al , 1
            JNZ CHK
            
            mov al, 9
            mov [di], al 
            popa
            ret
    CHK:
    ;If Ready read the VALUE in Receive data register
            mov dx , 03F8H
            in al , dx 
            mov [di] , AL
            popa
            ret
ReceiveByte ENDP

;description
ReceiveMsg PROC
    pusha

    ;msg structure hyb2a 00 start pos 00 end pos

    lea di, RMsg
    call ReceiveByte
    cmp RMsg, 9
    jne Recieve    
    popa
    ret

    Recieve:
    
    cmp RMsg, 200
    je End2
    cmp RMsg,8
    jbe NotEnd
    mov al,RMsg
    mov dh,OY
    mov dl,OX
    mov bh,0
    mov ah,2
    int 10h
    pusha
    call WRITEOUTPUT
    popa
    popa
    ret
    End2:
    mov winner,3
    popa 
    ret

    NotEnd:
    mov ah, 9

    mov al, RMsg
    mov hx2, al
   ; call PrintNumber

    ;inc di
    J1:call ReceiveByte
    cmp RMsg, ah
    je J1 

    mov al, RMsg
    ;call PrintNumber
    mov hy2, al

    ;inc di
    J2:call ReceiveByte
    cmp RMsg, ah
    je J2

    mov al, RMsg
    ;call PrintNumber
    mov px2, al

    ;inc di
    J3:call ReceiveByte
    cmp RMsg, ah
    je J3
    
    mov al, RMsg
    ;call PrintNumber
    mov py2, al

    ;     mov ah,1
    ; int 21h
    ; mov al,RMsg
    ; mov hx2, al
    ; mov al,RMsg +1
    ; mov hy2, al
    ; mov al,RMsg + 2
    ; mov px2, al
    ; mov al,RMsg + 3
    ; mov py2, al

    mov ch,px2
    mov cl,py2
    cmp hx,ch
    jne CheckKill

    cmp hy,cl
    jne CheckKill
    mov hx,0
    mov hy,0

    CheckKill:
    call to_idx
    ; check if end pos had a piece
    mov dl, '0'
    cmp [di],dl
    je Skip_Appending_dp ;deadpiece

    mov al, 'B'
    cmp [di], al
    je kill_Black2
    ;kill white
    lea si, W_DeadPiece
    jmp Kill_Piece2
    kill_Black2:
    lea si, B_DeadPiece
    Kill_Piece2:

    mov bl,'4'  ; king number ex B4 is black king
    cmp [di+1],bl
    jne HI_AppendDeadPiece2
    mov Winner, 2 ;winner 0 => no winner 1 => I win 2 => Other player wins

    HI_AppendDeadPiece2:
        mov bl,'$'  ;get last element in array to append at the end
        sub si,2
        GetEnd2:add si,2
            cmp [si],bl
            jne GetEnd2
        mov bh,[di]
        mov bl,[di+1]
        mov [si],bh
        mov [si+1],bl
    call DrawDeadP

    Skip_Appending_dp:
    call Move_Piece2

    ;lea di,RMsg
    mov RMsg, 9
    ; mov [di], dl
    ; mov [di+1], dl
    ; mov [di+2], dl
    ; mov [di+3], dl

    popa
    ret
ReceiveMsg ENDP

;byb3at byte wa7da
;bya5odha f [di]
SendByte PROC far
    pusha
  ;Check that Transmitter Holding Register is Empty
            mov dx , 3FDH		; Line Status Register
    AGAIN:  
            In al , dx 			;Read Line Status
            AND al , 00100000b
            JZ AGAIN

            ; mov al, 9;==> error didnt send
            ; ret
    ;If empty put the VALUE in Transmit data register
            mov dx , 3F8H		; Transmit data register
            mov al, [di]
            out dx , al 
    popa
    ret
SendByte ENDP
    

SendMsg PROC
    pusha
    lea di,SMsg
    mov dl, 9
    cmp [di], dl
    jne StartSend
    
    popa
    ret

    StartSend:
    call SendByte

    inc di
    call SendByte

    inc di
    call SendByte

    inc di
    call SendByte

    ;clear send msg
    lea di,SMsg
    mov [di], dl
    mov [di+1], dl
    mov [di+2], dl
    mov [di+3], dl
    
    popa
    ret
SendMsg ENDP

Clock PROC ;frame/al time between last iteration in 1/100 of second
    pusha
    add counter,al
    mov al,counter
    cmp al,100
    jae incrementsec
    popa
    ret
    incrementsec:
    inc seconds
    sub counter,100d

    cmp seconds,60
    jae incrementminutes

    jmp displayclock
    incrementminutes:
    inc minutes
    mov seconds,0

    displayclock:
    ;display minutes:seconds  in 00:00 format
    ;in top center 
    mov dh,0
    mov dl,18d
    mov bh,0
    mov ah,2
    int 10h
    mov al,minutes
    call printNumber
    mov ah,2
    mov dl,':'
    int 21h
    mov al,seconds
    call printNumber
    
    popa
    ret
Clock ENDP


WRITEINPUT PROC
    cmp IY,21d
    jb kammelI
    call SCROLLInputScreen
    mov IY,20d
    mov IX,0
    kammelI:
	cmp al,8
	jne notback
	mov ah,2
	mov dl,8
	int 21h
	mov dl,' '
	int 21h
	mov dl,8
	int 21h
	cmp ix,0
	je ohno3
	dec iX
	mov AH,2
	mov DL,iX
	MOV DH,iY
	int 10h
	ret
	ohno3:
	mov ix,6
	cmp iy,15d
	jne ohno4
	mov ix,0
	mov AH,2
	mov DL,iX
	MOV DH,iY
	int 10h
	ret
	ohno4:
	dec iy
	mov AH,2
	mov DL,iX
	MOV DH,iY
	int 10h
	ret
	notback:
	cmp al,13d        ;to check if the value entered is an enter key
	jne cont1
	cmp Iy,20d
	jb cont1
	cmp ix,6d        ;to check the borders before writing the char
	jb cc
	CALL newILine
    mov Iy,20d
	mov AH,2
	mov DL,IX
	MOV DH,IY
	int 10h
	call SCROLLInputScreen
	RET
	cc:
	call SCROLLInputScreen
	call newILine
	mov IY,20d
	mov AH,2
	mov DL,IX
	MOV DH,IY
	int 10h
	ret
	cont1:
	CMP AL,13d
	JE IENTER
	CMP ix,6
	jb p1
	mov IX,0
	inc IY
	p1:
	mov AH,2
	mov DL,IX
	MOV DH,IY
	int 10h

	
	mov ah,2
	mov dl,al
	int 21h
	; MOV AH,09H
	; MOV BH, 0      ; Set page number
	; MOV BL, 0fh      ; Color (blue)
	; MOV CX, 1      ; Character count
	; INT 10h

	INC IX
	RET
	IENTER:
	CALL newILine
	mov AH,2
	mov DL,IX
	MOV DH,IY
	int 10h
	RET
WRITEINPUT ENDP

  
WRITEOUTPUT PROC
    cmp OY,21d
    jb kammelO
    call SCROLLOutputScreen
    mov OY,20d
    mov OX,33
    kammelO:
	cmp al,8
	jne notback2
	cmp ox,33
	je fo
	mov ah,2
	mov dl,8
	int 21h
	mov dl,' '
	int 21h
	mov dl,8
	int 21h
	fo:
	cmp ox,33
	jbe ohno1
	dec OX
	mov AH,2
	mov DL,oX
	MOV DH,oY
	int 10h
	ret
	ohno1:
	mov ox,39d
	cmp oy,15d
	jne ohno2
	mov ox,33d
	mov AH,2
	mov DL,oX
	MOV DH,oY
	int 10h
	ret
	ohno2:
	dec oy
	mov AH,2
	mov DL,oX
	MOV DH,oY
	int 10h
	ret
	notback2:
	cmp al,13d
	jne cont2
	cmp oy,20d
	jb cont2
	cmp ox,39d
	jb cc1
	CALL newOLine
    mov Oy,20d
	mov AH,2
	mov DL,OX
	MOV DH,OY
	int 10h
	call SCROLLOutputScreen
	RET
	cc1:
	call SCROLLOutputScreen
	call newOLine
	mov oY,20d
	mov AH,2
	mov DL,oX
	MOV DH,oY
	int 10h
	ret
	cont2:
	CMP AL,13d
	JE OENTER
	CMP ox,39
	jb p2
	mov oX,33
	inc oY
	p2:
	mov AH,2
	mov DL,oX
	MOV DH,oy
	int 10h

	mov ah,2
	mov dl,al
	int 21h
	; MOV AH,09H
	; MOV BH, 0      ; Set page number
	; MOV BL, 0fh      ; Color (red)
	; MOV CX, 1      ; Character count
	; INT 10h

	INC oX
	RET
	OENTER:
	CALL newOLine
	mov AH,2
	mov DL,OX
	MOV DH,OY
	int 10h
	RET
WRITEOUTPUT ENDP


SCROLLInputScreen proc
	pusha
	mov al,1h     ; function 6
	mov ah,6h
	mov bh,0h       ; normal video attribute         
	mov ch,15       ; upper left Y
	mov cl,0        ; upper left X
	mov dh,21    ; lower right Y
	mov dl,6      ; lower right X 
	int 10h  
	mov ah,3
	mov bh,0
	int 10h   
	;mov ah,2
	;mov dl,' '
	;int 21h
	popa
	ret
SCROLLInputScreen endp


SCROLLOutputScreen proc
	pusha
	mov al,1h     ; function 6
	mov ah,6h
	mov bh,0h       ; normal video attribute         
	mov ch,15       ; upper left Y
	mov cl,33        ; upper left X
	mov dh,21    ; lower right Y
	mov dl,39      ; lower right X 
	int 10h  
	mov ah,3
	mov bh,0
	int 10h   
	;mov ah,2
	;;mov dl,' '
	;int 21h
	popa
	ret
SCROLLOutputScreen endp

newILine proc
	mov IX,0
	inc IY
	ret
newILine endp

newOLine proc
	mov OX,33
	inc OY
	ret
newOLine endp


END ;MAIN
