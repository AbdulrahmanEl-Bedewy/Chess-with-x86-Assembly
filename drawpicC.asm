.Model Small
.Stack 64
.Data

chickenWidth EQU 200
chickenHeight EQU 200

chickenFilename DB 'assets\Binfiles\board.bin', 0

chickenFilehandle DW ?

chickenData DB chickenWidth*chickenHeight dup(0)

.Code
MAIN PROC FAR
    MOV AX , @DATA
    MOV DS , AX
    
    MOV AH, 0
    MOV AL, 13h
    INT 10h
	
    CALL OpenFile
    CALL ReadData
	
    LEA BX , chickenData ; BL contains index at the current drawn pixel
	
    MOV CX,60
    MOV DX,0
    MOV AH,0ch
	
; Drawing loop
drawLoop:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,chickenWidth + 60
JNE drawLoop 
	
    MOV CX , 60
    INC DX
    CMP DX , chickenHeight
JNE drawLoop

	
    ; Press any key to exit
    MOV AH , 0
    INT 16h
    
    call CloseFile
    
    ;Change to Text MODE
    MOV AH,0          
    MOV AL,03h
    INT 10h 

    ; return control to operating system
    MOV AH , 4ch
    INT 21H
    
MAIN ENDP




OpenFile PROC 

    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, chickenFilename
    INT 21h
    
    ; you should check carry flag to make sure it worked correctly
    ; carry = 0 -> successful , file handle -> AX
    ; carry = 1 -> failed , AX -> error code
     
    MOV [chickenFilehandle], AX
    
    RET

OpenFile ENDP

ReadData PROC

    MOV AH,3Fh
    MOV BX, [chickenFilehandle]
    MOV CX,chickenWidth*chickenHeight ; number of bytes to read
    LEA DX, chickenData
    INT 21h
    RET
ReadData ENDP 


CloseFile PROC
	MOV AH, 3Eh
	MOV BX, [chickenFilehandle]

	INT 21h
	RET
CloseFile ENDP

END MAIN