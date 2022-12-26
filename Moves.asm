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
EXTRN LoadAssets:FAR

EXTRN DrawRightPiece:FAR
EXTRN w_rookData:byte

.286
.Model small
.Stack 100h
.Data

prevms db ? 
prevs db ?  
Prevtime dw ?
frame db ?   
timer dw 0  
x dw 0
y dw 0
startx db 0
starty db 0
endx db 20
endy db 40
vdx db 20
vdy db 40
.Code
;print el rakam el f Al
PrintNumber MACRO   
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
ENDM

;description
Main PROC    
    mov ax,@data
    mov ds,ax      
    mov ax,0A000h
    mov es,ax      

    ;clears screen
    mov ax, 3
    int 10h  
    
    mov al, 13h
	mov ah, 0
	int 10h 

    call far ptr LoadAssets
    ; call far ptr InitBoard

 ;MAIN GAME LOOP
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
        cmp dh, prevs
mov ax,0
mov cx,x
mov dx ,y
lea bx, w_rookData
call far ptr DrawPiece_px

  Calculate1:
    call far ptr GetFrameTime 
    
    cmp x, 20; endx
    ja s2
    mov bl,vdx
    mul bl
    mov bl,100
    div bl
    mov ah,0
    add x,ax
    
    s2:
    cmp y, 40;endy
    ja s
    mov bl,vdy
    mul bl
    mov bl,100
    div bl
    mov ah,0
    add y,ax
    s:
    ;add Y,ax
    ; cmp x, 168
    ; jae ending
    mov cx,x
    mov dx ,y
    lea bx, w_rookData
    call far ptr DrawPiece_px
    cmp x, 20;endx
    jb Calculate1
    cmp y, 40;endy
    jb Calculate1

     
        
;   Calculate1:
    ;      call far ptr GetFrameTime 
    ;      mov al,frame
    ;      mov ah,0
    ;      add timer, ax
    ;      cmp timer, 700
    ;      jae ending


    ;      ;PrintNumber
;      jmp Calculate1
     
       
    ending:
    mov cx, 168
    mov dx ,168
    lea bx, w_rookData
    call far ptr DrawPiece_px
    mov ah,2
    mov dl, 'D'
    int 21h 

    ; return control to operating system
    MOV AH , 4ch
    INT 21H

    hlt
Main ENDP

;description
GetFrameTime PROC
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

;description
DrawRec PROC
	

    mov al, 4
    mov bl, 20 
    mov dx, y
    sidebar1:  ; draw 1 square
        mov cx, x
        mov bh,20
    kp:    
        mov ah, 0ch
        int 10h     ; set pixel.
        inc cx
        dec bh
        jnz kp
    inc dx
    dec bl
    JNZ sidebar1
    ret
DrawRec ENDP


DrawPiece_px PROC  ; load cx:dx starting position x:y col:row 1-8:1-8
    mov AH,0
    mov al,cl


    MOV AH,0ch    
    mov Si, Cx
    add Si, 20
    mov Di, dx
    add Di,20
    ; Drawing loop
    drawLoop:
        MOV AL,[BX]
        ;MOV AL, 4dh
        cmp AL, 0fh
        jz skip
        INT 10h
        skip: 
        INC CX
        INC BX
        CMP CX, Si
    JNE drawLoop 
        
        MOV CX , Si
        Sub CX, 20
        INC DX
        CMP DX , Di
    JNE drawLoop

    ret
DrawPiece_px ENDP

;description
; RedrawBoardSq_px PROC FAR ; load ch:cl starting position x:y col:row 1-8:1-8
;     pusha

;     Lea Bx,  BoardData
    
;     mov AH,0
;     mov al,cl
;     push cx


;     mov Si, 198D
;     mul Si
;     ;mov Dx, Ax
;     add bx,ax

;     mov Al,ch
;     mov ah,0
;     mov cl, 20D
;     mul cl
;  ;   mov Cx, Ax
;     add bx,ax
; ;    add Cx , 80

;     pop cx
;     mov AH,0
;     mov al,cl
;     mov Si, 20D
;     mul Si
;     mov Dx, Ax
;     ;add bx,ax

;     mov Al,ch
;     mov ah,0
;     mov cl, 20D
;     mul cl
;     mov Cx, Ax
;     ;add bx,ax
;     add Cx , 60

;     MOV AH,0ch    
;     mov Si, Cx
;     add Si, 20
;     mov Di, dx
;     add Di,20

    
;     ; Drawing loop
;     drawLoop4:
;         MOV AL,[BX]
;         INT 10h 
;         INC CX
;         INC BX
;         CMP CX, Si
;     JNE drawLoop4             
;         ;MOV CX , Si
;         Sub CX, 20
;         Sub bX, 20
;         add bx, 198
;         INC DX
;         CMP DX , Di
;     JNE drawLoop4
;     popa
;     ret
; RedrawBoardSq ENDP

; RedrawPiece_px PROC FAR
;     pusha
;     ;check if square had piece and draw it
    
;     ;Gets start idx of Position (CX) and puts it in DI
;     ;call far ptr to_idx
;     dec cl
;     dec ch
;     Lea di, chessBoard
;     mov al,cl
;     mov ah,0
;     mov bl,16d
;     mul bl
;     add di, ax

;     mov al,ch
;     mov ah,0
;     mov bl,2d
;     mul bl
;     add di,ax


;     mov bl,'0'
;     cmp [di],bl
;     je nopiece
;     inc cl
;     inc ch
;     call far ptr DrawRightPiece

;     ;no piece
;     nopiece: 
;     popa
;     ret
; RedrawPiece ENDP







END main

;handle player1 input
HandleInput PROC ; the user input is in ax => al:ascii ah:scan code
    MOV ch,px
    MOV cl,py
    cmp al,'q'
    je select
    cmp al,'d'
    je Right
    cmp al,'a'
    je Left
    cmp al,'w'
    je up
    cmp al,'s'
    je down

    ret
     
    Right:
    cmp px,8
    je Draw_Highlighted
    add px,1
    ret
    Left:
    cmp px,1
    je Draw_Highlighted
    sub px,1
    ret
    up:
    cmp py,1
    je Draw_Highlighted
    sub py,1
    ret
    down:
    cmp py,8
    je Draw_Highlighted
    add py,1

    Draw_Highlighted:
        call far ptr RedrawBoardSq ; redaraw the current as a normal board square (not higlighted) before moving 
        call far ptr DrawPossibleMoves ; need to redraw possible moves so that it is not erased if selector gets on a valid move's square
        call far ptr DrawPossibleAttacks ; need to redraw possible moves so that it is not erased if selector gets on a valid attack's square
        call far ptr RedrawPiece ; redraw piece if any at the old location

        cmp hx,0
        je skip
        ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
        DrawSq hx, hy
        mov ch, hx
        mov cl, hy
        call far ptr RedrawPiece
        skip:
        mov ch,px
        mov cl,py
        DrawSq px, py ;draw higlighting of new square
        call far ptr RedrawPiece ; 

    ret

    select:

    ;check if valid move
        mov ch,px
        mov cl,py
        call far ptr to_idx
        lea si,ValidMoves
        call far ptr List_Contains
        cmp al,0 ; selected pos is not a valid move
        je Check_Valid_Attack
        call far ptr Move_Piece
        jmp H_I_ClearValidLists
        
        Check_Valid_Attack:
            lea si, ValidAttacks
            call far ptr List_Contains
            cmp al,0 ; selected pos is not a valid attack 
            jne Skip_Check_Empty
            mov al,0
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
            call far ptr Move_Piece 
            jmp H_I_ClearValidLists

        H_I_ClearValidLists:
        lea si,ValidMoves
        call far ptr ClearList
        lea si, ValidAttacks
        call far ptr ClearList
        ;===============
        ;General deselects piece in hx:hy
        mov ch, hx
        mov cl, hy
        call far ptr RedrawBoardSq
        call far ptr RedrawPiece
        mov hx,0
        mov hy,0
        ;===============
        ret
    ;ret
        Sel:
            mov ch, hx
            mov cl, hy
            call far ptr RedrawBoardSq
            call far ptr RedrawPiece

            mov ch, px
            mov cl, py

            mov hx, ch
            mov hy, cl 
            lea si, ValidMoves
            call far ptr ClearList
            lea si, ValidAttacks
            call far ptr ClearList
            call far ptr GetValidMoves
            call far ptr DrawPossibleMoves
            call far ptr DrawPossibleAttacks
            ret
    
HandleInput ENDP

;checks if a value in cx is contained in an array. lea si, array
;array's end is denoted by a $
;return al=1 if found al=0 if not found 
List_Contains PROC
    push si
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
    pop si
    ret
List_Contains ENDP

; move a piece's location in chessboard array
; from hx & hy to px & py
Move_Piece PROC
    mov ch,px
    mov cl,py
    call far ptr to_idx
    mov bx,di
    mov ch,hx
    mov cl,hy
    call far ptr to_idx
    mov cx,[di]
    mov [bx],cx
    mov ax,3030h
    mov [di],ax
    ret
Move_Piece ENDP

;Clears an array and replace all elements with $
;lea si, array
ClearList PROC
    mov al,'$'
    Clear:
        cmp [si],'$'
        je Cleared
        mov ch, [si]
        mov cl, [si+1]
        call far ptr RedrawBoardSq
        call far ptr RedrawPiece
        mov [si], al
        mov [si+1], al
        add si,2
        jmp Clear
    Cleared:
    ret
ClearList ENDP


HandleInput PROC ; the user input is in ax => al:ascii ah:scan code
    pusha
    push ax
    mov ah,2
    mov dl, '1'
    int 21h  
    pop ax
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
    
    popa
    ret
;==================================
;This part is responsible for updating the selector position on key press
    Right:
    mov ah,2
    mov dl, '2'
    int 21h  

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
        popa
        ret
;==================================
;This part is responsible for drawing selector position update
    Draw_Highlighted:
        mov ah,2
        mov dl, '3'
        int 21h  

        call far ptr RedrawBoardSq ; redaraw the current as a normal board square (not higlighted) before moving 
        call far ptr DrawPossibleMoves ; need to redraw possible moves so that it is not erased if selector gets on a valid move's square
        call far ptr DrawPossibleAttacks ; need to redraw possible moves so that it is not erased if selector gets on a valid attack's square
        call far ptr RedrawPiece ; redraw piece if any at the old location

        mov ah,2
        mov dl, '4'
        int 21h  

        cmp hx,0
        je skip
        ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
        DrawSq hx, hy
        mov ch, hx
        mov cl, hy
        call far ptr RedrawPiece
        skip:
         mov ah,2
        mov dl, '5'
        int 21h  

        mov ch,px
        mov cl,py
        DrawSq px, py ;draw higlighting of new square
        call far ptr RedrawPiece ; 
        popa
        ret

;==================================
;This part is responsible for moving and attacking logic
    select:
        
    ;check if valid move
        mov ch,px
        mov cl,py
        call far ptr to_idx
        lea si,ValidMoves
        call far ptr List_Contains
        cmp al,0 ; selected pos is not a valid move
        je Check_Valid_Attack
        call far ptr Move_Piece

         mov ah,2
        mov dl, '9'
        int 21h  

        jmp H_I_ClearValidLists
        
        Check_Valid_Attack:
            lea si, ValidAttacks
            call far ptr List_Contains
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
            call far ptr Move_Piece 
            ;jmp H_I_ClearValidLists
;==================================
;This part is responsible for re-inializing valid attack/move lists and drawing the updates of attacking/moving 
        H_I_ClearValidLists:
         mov ah,2
        mov dl, '6'
        int 21h 
        ClearValidMoves
        ClearValidAttacks
         mov ah,2
        mov dl, '7'
        int 21h 

       
        ;===============
        ;General deselects piece in hx:hy
        mov ch, hx
        mov cl, hy
        call far ptr RedrawBoardSq
        call far ptr RedrawPiece
        mov hx,0
        mov hy,0
        ;===============
        call far ptr DrawDeadP
        popa
        ret

;==================================
;This part is responsible for selecting a new piece and drawing its valid moves & attacks 
        Sel:
            mov ah,2
            mov dl, '8'
            int 21h 

            mov ch, hx
            mov cl, hy
            call far ptr RedrawBoardSq
            call far ptr RedrawPiece

            mov ch, px
            mov cl, py

            mov hx, ch
            mov hy, cl 
            ClearValidMoves
            ClearValidAttacks

            mov ch, px
            mov cl, py
            call far ptr GetValidMoves
            call far ptr DrawPossibleMoves
            call far ptr DrawPossibleAttacks
            popa
            ret
    
HandleInput ENDP

; MAIN PROC FAR
;     MOV AX , @DATA
;     MOV DS , AX
    
;     MOV AH, 0
;     MOV AL, 13h
;     INT 10h
	
;     ;call far ptr Init
	
;     ; Press any key to exit
;     MOV AH , 0
;     INT 16h
    
    
;     ;Change to Text MODE
;     MOV AH,0          
;     MOV AL,03h
;     INT 10h 

;     ; return control to operating system
;     MOV AH , 4ch
;     INT 21H

;     hlt
; MAIN ENDP

        ; call far ptr InitGame
        ; GameLoop:

        ;     mov ah,1
        ;     int 16h   
        ;     jz GameLoop

        ;     mov ah,0
        ;     int 16h   
            
        ;     cmp al, 'e'

        ;     ;Check if select key pressed
        ;     call far ptr HandleInput
        ;     call far ptr HandleInput2
        ;     jmp GameLoop

