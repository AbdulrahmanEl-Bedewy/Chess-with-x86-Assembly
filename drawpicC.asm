.Model Small
.Stack 64
.Data

chickenWidth EQU 200
chickenHeight EQU 200

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

LoadImage MACRO name, size, array
    LEA DX, name
    CALL OpenFile
    mov Cx, size * size
    LEA DX, array
    CALL ReadData	
    call CloseFile
ENDM LoadImage

DrawP MACRO Dataarray , x, y
    LEA BX , Dataarray ; BL contains index at the current drawn pixel	
    MOV CX,x
    MOV DX,y
    call DrawPiece
ENDM

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

    LEA BX , BoardData ; BL contains index at the current drawn pixel	
    MOV CX,60
    MOV DX,0
    call DrawBoard

   DrawP b_rookData , 80, 20 ; BL contains index at the current drawn pixel	
   DrawP b_knightData , 100, 20 ; BL contains index at the current drawn pixel	
   DrawP b_bishopData , 120, 20 ; BL contains index at the current drawn pixel	
   DrawP b_queenData , 140 ,  20 ; BL contains index at the current drawn pixel	
   DrawP b_kingData , 160, 20 ; BL contains index at the current drawn pixel	
   DrawP b_knightData , 180, 20 ; BL contains index at the current drawn pixel	
   DrawP b_bishopData , 200, 20 ; BL contains index at the current drawn pixel	
   DrawP b_rookData , 220, 20 ; BL contains index at the current drawn pixel	
   DrawP b_pawnData , 80,  40 ; BL contains index at the current drawn pixel	
   DrawP b_pawnData , 100, 40 ; BL contains index at the current drawn pixel	
   DrawP b_pawnData , 120, 40 ; BL contains index at the current drawn pixel	
   DrawP b_pawnData , 140, 40 ; BL contains index at the current drawn pixel	
   DrawP b_pawnData , 160, 40 ; BL contains index at the current drawn pixel	
   DrawP b_pawnData , 180, 40 ; BL contains index at the current drawn pixel	
   DrawP b_pawnData , 200, 40 ; BL contains index at the current drawn pixel	
   DrawP b_pawnData , 220, 40 ; BL contains index at the current drawn pixel	

;=============================================================
    ; trying to highlight a square
    ;LEA BX , Dataarray ; BL contains index at the current drawn pixel	
    MOV CX,80
    MOV DX,159
    MOV AH,0ch    
    mov Si, Cx
    add Si, 20
    mov Di, dx
    add Di,20
    ; Drawing loop
    drawLoop2:
      ;  MOV AL,[BX]
        MOV AL, 4dh
        cmp AL, 0fh
        jz skip2
        INT 10h
        skip2: 
        INC CX
        INC BX
        CMP CX, Si
    JNE drawLoop2 
        
        MOV CX , Si
        Sub CX, 20
        INC DX
        CMP DX , Di
    JNE drawLoop2



;=============================================================

   DrawP w_rookData , 80, 159 ; BL contains index at the current drawn pixel	
   DrawP w_knightData , 100, 159 ; BL contains index at the current drawn pixel	
   DrawP w_bishopData , 120, 159 ; BL contains index at the current drawn pixel	
   DrawP w_queenData , 140 , 159 ; BL contains index at the current drawn pixel	
   DrawP w_kingData , 160, 159 ; BL contains index at the current drawn pixel	
   DrawP w_knightData , 180, 159 ; BL contains index at the current drawn pixel	
   DrawP w_bishopData , 200, 159 ; BL contains index at the current drawn pixel	
   DrawP w_rookData , 220, 159 ; BL contains index at the current drawn pixel	
   DrawP w_pawnData , 80,  140 ; BL contains index at the current drawn pixel	
   DrawP w_pawnData , 100, 140 ; BL contains index at the current drawn pixel	
   DrawP w_pawnData , 120, 140 ; BL contains index at the current drawn pixel	
   DrawP w_pawnData , 140, 140 ; BL contains index at the current drawn pixel	
   DrawP w_pawnData , 160, 140 ; BL contains index at the current drawn pixel	
   DrawP w_pawnData , 180, 140 ; BL contains index at the current drawn pixel	
   DrawP w_pawnData , 200, 140 ; BL contains index at the current drawn pixel	
   DrawP w_pawnData , 220, 140 ; BL contains index at the current drawn pixel		
  
    ret
Init ENDP


DrawPiece PROC ; load cx:dx starting position x:y in pixels
    
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


DrawBoard PROC 
    
    
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

    ret
DrawBoard ENDP


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


END MAIN