Public InitBoard
Public LoadAssets
Public DrawSquare
Public DrawPiece
Public DrawPieces
Public DrawBoard
Public RedrawBoardSq
Public RedrawPiece
Public DrawPossibleMoves
Public DrawPossibleAttacks
Public DrawDeadP
Public DrawCooldown
Public DrawRightPiece


EXTRN to_idx:FAR
EXTRN chessBoard:byte
EXTRN ValidMoves:byte
EXTRN ValidAttacks:byte
EXTRN ValidMoves2:byte
EXTRN ValidAttacks2:byte
EXTRN B_DeadPiece:byte
EXTRN W_DeadPiece:byte
EXTRN px:byte
EXTRN py:byte
include Macro.inc
.286
.Model Small
.Stack 64
.Data


BoardFilename DB 'assets\Binfiles\board.bin', 0
b_rookFilename DB 'assets\Binfiles\b_rook.bin', 0
w_rookFilename DB 'assets\Binfiles\w_rook.bin', 0
b_knightFilename DB 'assets\Binfiles\b_knight.bin', 0
w_knightFilename DB 'assets\Binfiles\w_knight.bin', 0
b_bishopFilename DB 'assets\Binfiles\b_bishop.bin', 0
w_bishopFilename DB 'assets\Binfiles\w_bishop.bin', 0
b_kingFilename DB 'assets\Binfiles\b_king.bin', 0
w_kingFilename DB 'assets\Binfiles\w_king.bin', 0
b_queenFilename DB 'assets\Binfiles\b_queen.bin', 0
w_queenFilename DB 'assets\Binfiles\w_queen.bin', 0
b_pawnFilename DB 'assets\Binfiles\b_pawn.bin', 0
w_pawnFilename DB 'assets\Binfiles\w_pawn.bin', 0

cooldown1Filename DB 'assets\Binfiles\cd1.bin', 0
cooldown2Filename DB 'assets\Binfiles\cd2.bin', 0
cooldown3Filename DB 'assets\Binfiles\cd3.bin', 0
cooldown4Filename DB 'assets\Binfiles\cd4.bin', 0
cooldown5Filename DB 'assets\Binfiles\cd5.bin', 0
cooldown6Filename DB 'assets\Binfiles\cd6.bin', 0
cooldown7Filename DB 'assets\Binfiles\cd7.bin', 0
cooldown8Filename DB 'assets\Binfiles\cd8.bin', 0

Filehandle DW ?


BoardData DB 198*198 dup(0)
b_rookData DB 20*20 dup(0)
w_rookData DB 20*20 dup(0)
b_knightData DB 20*20 dup(0)
w_knightData DB 20*20 dup(0)
b_bishopData DB 20*20 dup(0)
w_bishopData DB 20*20 dup(0)
b_kingData DB 20*20 dup(0)
w_kingData DB 20*20 dup(0)
b_queenData DB 20*20 dup(0)
w_queenData DB 20*20 dup(0)
b_pawnData DB 20*20 dup(0)
w_pawnData DB 20*20 dup(0)

cooldown1Data DB 20*20 dup(0)
cooldown2Data DB 20*20 dup(0)
cooldown3Data DB 20*20 dup(0)
cooldown4Data DB 20*20 dup(0)
cooldown5Data DB 20*20 dup(0)
cooldown6Data DB 20*20 dup(0)
cooldown7Data DB 20*20 dup(0)
cooldown8Data DB 20*20 dup(0)


.Code


;initializes graphics, loads all pictures and draw initial board config
InitBoard PROC FAR
    pusha
    MOV AH, 0
    MOV AL, 13h
    ; mov ax, 4f02h
    ; mov bx, 257
    INT 10h

    ;Background of Chessboard
    mov Di, 0
    mov Bl, 200
    mov ax,0A000h
    mov es,ax
    sidebar1:  ; draw 1 square
        mov al,14h
        mov cx,60d
        rep STOSB
        SUB DI,60d
        ADD DI,320D
        DEC BL
    JNZ sidebar1

    mov Di, 320-60
    mov Bl, 200
    mov ax,0A000h
    mov es,ax
    sidebar2:  ; draw 1 square
        mov al,14h
        mov cx,60d
        rep STOSB
        SUB DI,60d
        ADD DI,320D
        DEC BL
    JNZ sidebar2


    call DrawBoard

    DrawP b_rookData,     1, 1 ;BL contains index at the current drawn pixel	
    DrawP b_knightData,   2, 1 ; BL contains index at the current drawn pixel	
    DrawP b_bishopData,   3, 1 ; BL contains index at the current drawn pixel	
    DrawP b_queenData,    4, 1 ; BL contains index at the current drawn pixel	
    DrawP b_kingData,     5, 1 ; BL contains index at the current drawn pixel	
    DrawP b_bishopData,   6, 1 ; BL contains index at the current drawn pixel	
    DrawP b_knightData,   7, 1 ; BL contains index at the current drawn pixel	
    DrawP b_rookData,     8, 1 ; BL contains index at the current drawn pixel	
    DrawP b_pawnData,     1, 2 ; BL contains index at the current drawn pixel	
    DrawP b_pawnData,     2, 2 ; BL contains index at the current drawn pixel	
    DrawP b_pawnData,     3, 2 ; BL contains index at the current drawn pixel	
    DrawP b_pawnData,     4, 2 ; BL contains index at the current drawn pixel	
    DrawP b_pawnData,     5, 2 ; BL contains index at the current drawn pixel	
    DrawP b_pawnData,     6, 2 ; BL contains index at the current drawn pixel	
    DrawP b_pawnData,     7, 2 ; BL contains index at the current drawn pixel	
    DrawP b_pawnData,     8, 2 ; BL contains index at the current drawn pixel	


    DrawP w_rookData,     1, 8 ;BL contains index at the current drawn pixel	
    DrawP w_knightData,   2, 8 ; BL contains index at the current drawn pixel	
    DrawP w_bishopData,   3, 8 ; BL contains index at the current drawn pixel	
    DrawP w_queenData,    4, 8 ; BL contains index at the current drawn pixel	
    DrawP w_kingData,     5, 8 ; BL contains index at the current drawn pixel	
    DrawP w_bishopData,   6, 8 ; BL contains index at the current drawn pixel	
    DrawP w_knightData,   7, 8 ; BL contains index at the current drawn pixel	
    DrawP w_rookData,     8, 8 ; BL contains index at the current drawn pixel	
    DrawP w_pawnData,     1, 7 ; BL contains index at the current drawn pixel	
    DrawP w_pawnData,     2, 7 ; BL contains index at the current drawn pixel	
    DrawP w_pawnData,     3, 7 ; BL contains index at the current drawn pixel	
    DrawP w_pawnData,     4, 7 ; BL contains index at the current drawn pixel	
    DrawP w_pawnData,     5, 7 ; BL contains index at the current drawn pixel	
    DrawP w_pawnData,     6, 7 ; BL contains index at the current drawn pixel	
    DrawP w_pawnData,     7, 7 ; BL contains index at the current drawn pixel	
    DrawP w_pawnData,     8, 7 ; BL contains index at the current drawn pixel			
  
    

    popa
    ret
InitBoard ENDP

;Loads all images used in the game
LoadAssets PROC far
    LoadImage BoardFilename, 198, BoardData
    LoadImage b_rookFilename, 20, b_rookData
    LoadImage w_rookFilename, 20, w_rookData
    LoadImage b_knightFilename, 20, b_knightData
    LoadImage w_knightFilename, 20, w_knightData
    LoadImage b_bishopFilename, 20, b_bishopData
    LoadImage w_bishopFilename, 20, w_bishopData
    LoadImage b_kingFilename, 20, b_kingData
    LoadImage w_kingFilename, 20, w_kingData
    LoadImage b_queenFilename, 20, b_queenData
    LoadImage w_queenFilename, 20, w_queenData
    LoadImage b_pawnFilename, 20, b_pawnData
    LoadImage w_pawnFilename, 20, w_pawnData

    LoadImage cooldown1Filename, 20, cooldown1Data
    LoadImage cooldown2Filename, 20, cooldown2Data
    LoadImage cooldown3Filename, 20, cooldown3Data
    LoadImage cooldown4Filename, 20, cooldown4Data
    LoadImage cooldown5Filename, 20, cooldown5Data
    LoadImage cooldown6Filename, 20, cooldown6Data
    LoadImage cooldown7Filename, 20, cooldown7Data
    LoadImage cooldown8Filename, 20, cooldown8Data
    ret
LoadAssets ENDP

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

;draw square // for highlighting 
 DrawSquare PROC FAR ; put y in al
                ; x in bl
                ; color in dl
    pusha
    push dx

    ; pusha
    ;     mov ah,2
    ;     mov dl, 'B'
    ;     int 21h 
    ; popa

    ;push ax
    ;push bx
    ;============
    ;calculating the position in video memory that correspond to the given coordinates
    mov AH,0
    mov BH,0

    mov Cx, 20D ; since each square is 20px by 20px to calculate the number of rows to move down in the video memory
    mul Cx
    push ax
    mov Cx, 320D; each row is 320 px so to move down one row we need to add 320
    mul Cx
    mov Di, Ax
    
    ; add the horizontal offset in the row.  
    mov Ax,Bx
    mov Cx, 20D 
    mul Cx
    push ax
    add Di, AX
    add Di , 60 ; the whole board has an offset of 60px from the left

    
    mov ax,0A000h
    mov es,ax
    pop bx
    pop ax
    add bx, 60

    ;get the pixel color of top left corner
    mov cx, bx
    add cx, 1
    mov dx, ax
    add dx, 2
    mov bh,0
    mov ah, 0Dh
    int 10h

    ;PrintNumber
    
    ;compare the color of the upper left corner to that of a normal board square i.e. white or black
    cmp al, 0
    je DrawSolidColor
    cmp al, 0Fh
    je DrawSolidColor
    
    push ax
    ; ;get the pixel color of top right corner
    add cx, 16
    ;add dx, 1 
    mov bh,0
    mov ah, 0Dh
    int 10h

    pop bx
    pop dx
    ;compare the color of the upper left and right corners if equal then the square had only 1 color previously 
    ;and now we need draw a second one 

    ;if the square already contains 1/2 colors compare the color we want to draw in dl, 
    ;if it is equal to one of the 2 colors then no need to draw again
    cmp al, dl ; al is the upper trianle color, bl is the lower triangle color, dl is the new color to be drawn
    je NoDraw
    cmp bl, dl
    je NoDraw

    cmp al,bl
    je Draw2MIxedColors


    ;the square has 2 colors and neither is equal to the one we want to draw which is stored in dl,
    ;we need to draw 3 colors in the square
    jmp Draw3MIxedColors

    NoDraw:
    popa
    ret 

    DrawSolidColor:
    pop dx
    MOV BL, 19d 
    mov al,dl
    Square:  ; draw 1 square
        ;mov al,4dh
        mov cx,19d
        rep STOSB
        SUB DI,19d
        ADD DI,320D
        DEC BL
    JNZ Square
    
    ; mov ah,2
    ; mov dl, 'C'
    ; int 21h 
    popa
    ret

    Draw2MIxedColors:
    ; we need to draw the second color on the right side
    ; draws a triangle in the upper right part of the square of color (dl)
    ; _____
    ;|\_   |   
    ;|  \_ |   
    ;|____\|
    ; so the upper part is the new color and lower part is the previous color
    ;pop dx
    
    MOV ah, 19d
    mov bx, 20
    mov ch, 0
    mov al,dl
    Square2:  ; draw 1/2 square
        ;mov al,4dh
        mov cx, bx 
        rep STOSB
        dec bx
        SUB DI,bx
        ADD DI,320D
        DEC ah
    JNZ Square2
    
    ; mov ah,2
    ; mov dl, 'D'
    ; int 21h 
    popa
    ret

    Draw3MIxedColors:
    ; we need to draw the third color on the right side
    ; cover up part of the existing colors to create a triangle at the top of color (al)
    ; and a trapezium on left of color (bl) and trapezium on right of color (dl)
    
        ; draws a triangle in the upper right part of the square   
        ;_________
        ;|       /|
        ;|      /_|
        ;|________|
        add di,20     
        MOV ah, 10d
        mov bx, 0
        mov ch, 0
        mov al,dl
        Square6:  ; draw small triangle
            ;mov al,4dh
            mov cx, bx
            rep STOSB
            inc bx
            SUB DI,bx
            ADD DI,320D
            DEC ah
        JNZ Square6
        ;inc di

        ; draws a small square in the lower right part
        ; ________
        ;|        |
        ;|      __|
        ;|_____|__|
        MOV ah, 9d
    ; mov bx, 1
        mov ch, 0
        mov al,dl
        Square5:  ; 
            ;mov al,4dh
            mov cx, 10
            rep STOSB
            SUB DI,10
            ADD DI,320D
            inc bx
            DEC ah
        JNZ Square5
   

    ;pop dx
    popa
     ret
 DrawSquare ENDP

DrawPiece PROC FAR  ; load cx:dx starting position x:y col:row 1-8:1-8
                ; lea bx, data array
    mov AH,0
    mov al,cl

    mov cl, 20D
    mul cl
    mov Dx, Ax
    
    mov Al,ch
    mov ah,0
    mov cl, 20D
    mul cl
    mov Cx, Ax
    add Cx , 60

    MOV AH, 0ch    
    mov Si, Cx
    add Si, 20
    mov Di, dx
    add Di, 20
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
DrawPiece ENDP

;description
RedrawBoardSq PROC FAR ; load ch:cl starting position x:y col:row 1-8:1-8
    pusha

    Lea Bx,  BoardData
    ; get offset in the Board data array
    mov AH,0
    mov al,cl
    push cx

    mov Si, 20D
    mul Si
    mov Si, 198D
    mul Si
    ;mov Dx, Ax
    add bx,ax

    mov Al,ch
    mov ah,0
    mov cl, 20D
    mul cl
 ;   mov Cx, Ax
    add bx,ax
;    add Cx , 80

; get x and y coordinates in px of the square that is to be drawn

    pop cx
    mov AH,0
    mov al,cl
    mov Si, 20D
    mul Si
    mov Dx, Ax
    ;add bx,ax

    mov Al,ch
    mov ah,0
    mov cl, 20D
    mul cl
    mov Cx, Ax
    ;add bx,ax
    add Cx , 60


    MOV AH,0ch    
    mov Si, Cx
    add Si, 20
    mov Di, dx
    add Di,20
    ; Drawing loop
    drawLoop4:
        MOV AL,[BX]
        INT 10h 
        INC CX
        INC BX
        CMP CX, Si
    JNE drawLoop4             
        ;MOV CX , Si
        Sub CX, 20
        Sub BX, 20
        add bx, 198
        INC DX
        CMP DX , Di
    JNE drawLoop4
    popa
    ret
RedrawBoardSq ENDP

RedrawPiece PROC FAR
    pusha
    ;check if square had piece and draw it
    
    ;Gets start idx of Position (CX) and puts it in DI
    ;call to_idx
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


    mov bl,'0'
    cmp [di],bl
    je nopiece
    inc cl
    inc ch
    call far ptr DrawRightPiece

    ;no piece
    nopiece: 
    popa
    ret
RedrawPiece ENDP

DrawBoard PROC 
    pusha 
    LEA BX , BoardData ; BL contains index at the current drawn pixel	
    MOV CX,60
    MOV DX,0
    MOV AH,0ch
	 
    ; Drawing loop
    BoarddrawLoop:
        MOV AL,[BX]
        INT 10h 
        INC CX
        INC BX
        CMP CX, 198 + 60
    JNE BoarddrawLoop 
        
        MOV CX , 60
        INC DX
        CMP DX , 198
    JNE BoarddrawLoop
    popa
    ret
DrawBoard ENDP

;Loops on the board array and Draws all pieces
DrawPieces PROC
    Lea bx, chessBoard

DrawPieces ENDP

OpenFile PROC ;load offset of file name in Dx
              ;return filehandle in Filehandle variable 

    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    INT 21h
    
    ; you should check carry flag to make sure it worked correctly
    ; carry = 0 -> successful , file handle -> AX
    ; carry = 1 -> failed , AX -> error code
     
    MOV [Filehandle], AX
    
    RET

OpenFile ENDP

ReadData PROC ; load file size in cx
              ; load image data array offset in Dx

    MOV AH,3Fh
    MOV BX, [Filehandle]
    INT 21h
    RET
ReadData ENDP 

;description
DrawPossibleMoves PROC FAR
    pusha
    lea di, ValidMoves
    mov cl, '$'
    DPM_Draw:
        cmp [di], cl
        je DPM_end
        mov bl,[di]
        mov al,[di+1]
        mov dl, 36h ;66h
        call DrawSquare
        add di, 2
        jmp DPM_Draw

    DPM_end:
     lea di, ValidMoves2
    mov cl, '$'
    DPM_Draw2:
        cmp [di], cl
        je DPM_end2
        mov bl,[di]
        mov al,[di+1]
        mov dl, 2bh ;8Ch
        call DrawSquare
        add di, 2
        jmp DPM_Draw2

    DPM_end2:
    popa
    ret
DrawPossibleMoves ENDP

DrawPossibleAttacks PROC FAR
    pusha
    lea di, ValidAttacks
    mov ah, '$'
    DPA_Draw:
        cmp [di], ah
        je DPA_end
        ; mov bl,24h
        ; mov al, 24h
        mov bl,[di]
        mov al,[di+1]
        mov dl, 4h
        call DrawSquare
        mov ch,[di]
        mov cl,[di+1]
        call RedrawPiece
        add di, 2
        jmp DPA_Draw

    DPA_end:
    lea di, ValidAttacks2
    mov ah, '$'
    DPA_Draw2:
        cmp [di], ah
        je DPA_end2
        ; mov bl,24h
        ; mov al, 24h
        mov bl,[di]
        mov al,[di+1]
        mov dl, 6Bh
        call DrawSquare
        mov ch,[di]
        mov cl,[di+1]
        call RedrawPiece
        add di, 2
        jmp DPA_Draw2

    DPA_end2:
    popa
    ret
DrawPossibleAttacks ENDP


CloseFile PROC 
	MOV AH, 3Eh
	MOV BX, [Filehandle]

	INT 21h
	RET
CloseFile ENDP

DrawDeadP PROC FAR
    pusha
    lea di,B_DeadPiece
    mov al,'$'
    
    mov ch,-3
    mov cl,0
    lp4:
        cmp ch,0
        je n1
        cmp [di],al
        je drawW
        call far ptr DrawRightPiece
        inc ch
        add di,2
        jmp lp4
        n1:
        inc cl
        mov ch,-3
        jmp lp4
    ;White
    drawW:
    lea di,W_DeadPiece
    mov ch,10d
    mov cl,1
    lp5:
        cmp ch,13
        je n2
        cmp [di],al
        je done
        call far ptr DrawRightPiece
        inc ch
        add di,2
        jmp lp5
        n2:
        inc cl
        mov ch,10d
        jmp lp5
    done:
    popa
    ret
DrawDeadP ENDP

;takes el piece symbols in di & position in CX
DrawRightPiece PROC far
    pusha

    ; pusha
    ; mov ah,2
    ; mov dl, [di]
    ; int 21h         
    ; popa

    mov bl,'B'
    cmp [di],bl
    ja White
    inc di
    mov bl,'0'
    cmp [di],bl
    je br
    inc bl
    cmp [di],bl
    je bkt
    inc bl
    cmp [di],bl
    je bb
    inc bl
    cmp [di],bl
    je bq
    inc bl
    cmp [di], bl
    je bk
    inc bl
    cmp [di],bl
    je bpw
    

    White:
    inc di
    mov bl,'0'
    cmp [di],bl
    je wr
    inc bl
    cmp [di],bl
    je wkt
    inc bl
    cmp [di],bl
    je wb
    inc bl
    cmp [di],bl
    je wq
    inc bl
    cmp [di], bl
    je wk
    inc bl
    cmp [di],bl
    je wpw

    jmp nopiece1
    br: lea bx,b_rookData
    jmp onepiece
    bkt:lea bx,b_knightData
    jmp onepiece
    bb:lea bx,b_bishopData
    jmp onepiece
    bk:lea bx,b_kingData
    jmp onepiece
    bq:lea bx,b_queenData
    jmp onepiece
    wr:lea bx,w_rookData
    jmp onepiece
    wkt:lea bx,w_knightData
    jmp onepiece
    wb:lea bx,w_bishopData
    jmp onepiece
    wk:lea bx,w_kingData
    jmp onepiece
    wq:lea bx,w_queenData
    jmp onepiece
    bpw:lea bx,b_pawnData
    jmp onepiece
    wpw:lea bx,w_pawnData

    onepiece:
    call DrawPiece
    nopiece1:

    ; mov ah,1
    ; int 21h
    popa 
    ret
DrawRightPiece ENDP

;takes position in CX
;takes the picture index to draw in al
DrawCooldown PROC FAR
    pusha
    mov bl, 0
    cmp al,bl
    je DC_nopiece1
    inc bl
    cmp al,bl
    je DC_cd2
    inc bl
    cmp al,bl
    je DC_cd3
    inc bl
    cmp al,bl
    je DC_cd4
    inc bl
    cmp al,bl
    je DC_cd5
    inc bl
    cmp al,bl
    je DC_cd6
    inc bl
    cmp al,bl
    je DC_cd7
    inc bl
    cmp al,bl
    je DC_cd8



    jmp DC_nopiece1

    DC_cd1: 
    lea bx,cooldown1Data
    jmp DC_onepiece
    DC_cd2:
    lea bx,cooldown2Data
    jmp DC_onepiece
    DC_cd3:
    lea bx,cooldown3Data
    jmp DC_onepiece
    DC_cd4:
    lea bx,cooldown4Data
    jmp DC_onepiece
    DC_cd5:
    lea bx,cooldown5Data
    jmp DC_onepiece
    DC_cd6:
    lea bx,cooldown6Data
    jmp DC_onepiece
    DC_cd7:
    lea bx,cooldown7Data
    jmp DC_onepiece
    DC_cd8:
    lea bx,cooldown8Data
    jmp DC_onepiece
    
    DC_onepiece:
    call DrawPiece
    call RedrawPiece
    DC_nopiece1:
    popa 
    ret
DrawCooldown ENDP


END
;END MAIN