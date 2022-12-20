.286
.Model small
.Stack 100h
.Data
timer db 0
prev db ? 
prevs db ?  
frame db 0
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
              

        
              
    mov ah, 2Ch    
    int 21h
    ;add timer    
    mov prev,dl
    mov prevs, dh
    mov al, ch
    sub al,12
    PrintNumber

    mov dl, '-'
    mov ah,2       
    int 21h 

    mov al,cl
    PrintNumber

    mov dl, '-'
    mov ah,2       
    int 21h 

    mov al,dh
    PrintNumber

    mov dl, '-'
    mov ah,2       
    int 21h 

    mov al,dl
    PrintNumber

    lp:
        mov ah, 2Ch    
        int 21h

        cmp dh, prevs
        je lp

        mov prev, dl
push dx
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h
        mov dl, 8
        mov ah,2       
        int 21h

        mov al, ch
        sub al,12
        PrintNumber

        mov dl, '-'
        mov ah,2       
        int 21h 

        mov al,cl
        PrintNumber

        mov dl, '-'
        mov ah,2       
        int 21h 

        mov al,dh
        PrintNumber
        
        mov dl, '-'
        mov ah,2       
        int 21h 
pop dx
        mov al,dl
        PrintNumber
        jmp lp 
        
        hlt

        ; inc frame  
        ; cmp dh, prevs
        ; je SameSecond
        
        ; add dl,100

        ; SameSecond:
        ; sub dl,prev
        ; add timer, dl

        ; cmp timer, 100
        ; jb lp

        
        ; second:
        ; ; el mafrood print y3ny  
        ; mov dl, 8
        ; mov ah,2       
        ; int 21h
        ; mov dl, 8
        ; mov ah,2       
        ; int 21h
        ; mov dl, 8
        ; mov ah,2       
        ; int 21h
        
        ; mov bl,10
        ; mov al,frame
        ; mov ah,0
        ; div bl
        ; push ax
        ; mov ah,0
        ; div bl  
        ; push ax  
        ; mov dl, al
        ; mov ah,2       
        ; add dl,48
        ; int 21h      
        ; pop ax
        ; mov dl, ah
        ; mov ah,2       
        ; add dl,48
        ; int 21h
        
        ; pop ax
        ; mov dl, ah
        ; mov ah,2       
        ; add dl,48
        ; int 21h 
        ; mov timer,0
        ; mov frame , 0
        ; jmp lp
        
    
    
Main ENDP

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
            call Move_Piece 
            jmp H_I_ClearValidLists

        H_I_ClearValidLists:
        lea si,ValidMoves
        Call ClearList
        lea si, ValidAttacks
        call ClearList
        ;===============
        ;General deselects piece in hx:hy
        mov ch, hx
        mov cl, hy
        call RedrawBoardSq
        call RedrawPiece
        mov hx,0
        mov hy,0
        ;===============
        ret
    ;ret
        Sel:
            mov ch, hx
            mov cl, hy
            call RedrawBoardSq
            call RedrawPiece

            mov ch, px
            mov cl, py

            mov hx, ch
            mov hy, cl 
            lea si, ValidMoves
            Call ClearList
            lea si, ValidAttacks
            call ClearList
            call GetValidMoves
            call DrawPossibleMoves
            call DrawPossibleAttacks
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
ClearList PROC
    mov al,'$'
    Clear:
        cmp [si],'$'
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

        call RedrawBoardSq ; redaraw the current as a normal board square (not higlighted) before moving 
        call DrawPossibleMoves ; need to redraw possible moves so that it is not erased if selector gets on a valid move's square
        call DrawPossibleAttacks ; need to redraw possible moves so that it is not erased if selector gets on a valid attack's square
        call RedrawPiece ; redraw piece if any at the old location

        mov ah,2
        mov dl, '4'
        int 21h  

        cmp hx,0
        je skip
        ;===== drawing a background behind the selected piece bs need to erase it b3d kda when another piece is selected
        DrawSq hx, hy
        mov ch, hx
        mov cl, hy
        call RedrawPiece
        skip:
         mov ah,2
        mov dl, '5'
        int 21h  

        mov ch,px
        mov cl,py
        DrawSq px, py ;draw higlighting of new square
        call RedrawPiece ; 
        popa
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

         mov ah,2
        mov dl, '9'
        int 21h  

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
        call RedrawBoardSq
        call RedrawPiece
        mov hx,0
        mov hy,0
        ;===============
        call DrawDeadP
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
            popa
            ret
    
HandleInput ENDP

; MAIN PROC FAR
;     MOV AX , @DATA
;     MOV DS , AX
    
;     MOV AH, 0
;     MOV AL, 13h
;     INT 10h
	
;     ;call Init
	
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

        ; call InitGame
        ; GameLoop:

        ;     mov ah,1
        ;     int 16h   
        ;     jz GameLoop

        ;     mov ah,0
        ;     int 16h   
            
        ;     cmp al, 'e'

        ;     ;Check if select key pressed
        ;     call HandleInput
        ;     call HandleInput2
        ;     jmp GameLoop