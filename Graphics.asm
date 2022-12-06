Public Init
Public DrawSquare
Public DrawPiece
Public DrawBoard
Public RedrawBoardSq
EXTRN chessBoard:byte
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

.Code



MAIN PROC FAR
    MOV AX , @DATA
    MOV DS , AX
    
    MOV AH, 0
    MOV AL, 13h
    INT 10h
	
    call Init
	
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
MAIN ENDP


;initializes graphics, loads all pictures and draw initial board config
Init PROC
    pusha
    MOV AH, 0
    MOV AL, 13h
    INT 10h

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

    
    call DrawBoard

   DrawP b_rookData,     1, 1 ;BL contains index at the current drawn pixel	
   DrawP b_knightData,   2, 1 ; BL contains index at the current drawn pixel	
   DrawP b_bishopData,   3, 1 ; BL contains index at the current drawn pixel	
   DrawP b_queenData,    4, 1 ; BL contains index at the current drawn pixel	
   DrawP b_kingData,     5, 1 ; BL contains index at the current drawn pixel	
   DrawP b_knightData,   6, 1 ; BL contains index at the current drawn pixel	
   DrawP b_bishopData,   7, 1 ; BL contains index at the current drawn pixel	
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
   DrawP w_knightData,   6, 8 ; BL contains index at the current drawn pixel	
   DrawP w_bishopData,   7, 8 ; BL contains index at the current drawn pixel	
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
Init ENDP

;draw square // for highlighting 
DrawSquare PROC ; put y in al
                ; x in bl
    pusha
    mov AH,0
    mov BH,0

    mov Cx, 20D
    mul Cx
    mov Cx, 320D
    mul Cx
    mov Di, Ax
    
    mov Ax,Bx
    mov Cx, 20D
    mul Cx
    add Di, AX
    add Di , 60

    
    MOV BL, 20d

    mov ax,0A000h
    mov es,ax
    Square:  ; draw 1 square
        mov al,4dh
        mov cx,20d
        rep STOSB
        SUB DI,20d
        ADD DI,320D
        DEC BL
    JNZ Square
    popa
    ret
DrawSquare ENDP

DrawPiece PROC ; load cx:dx starting position x:y col:row 1-8:1-8
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
DrawPiece ENDP

;description
RedrawBoardSq PROC ; load cx:dx starting position x:y col:row 1-8:1-8
    pusha
    Lea Bx,  BoardData
    
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
        Sub bX, 20
        add bx, 198
        INC DX
        CMP DX , Di
    JNE drawLoop4
    popa
    ret
RedrawBoardSq ENDP

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
    ;Lea 
DrawPieces ENDP

OpenFile PROC ;load offset of file name in Dx
              ;return filehandle in Filehandle variable 

    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    ;LEA DX, chickenFilename
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
   ; MOV CX,chickenWidth*chickenHeight ; number of bytes to read
   ; LEA DX, chickenData
    INT 21h
    RET
ReadData ENDP 


CloseFile PROC 
	MOV AH, 3Eh
	MOV BX, [Filehandle]

	INT 21h
	RET
CloseFile ENDP

END
;END MAIN