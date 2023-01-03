Public ChatScreen
EXTRN SendByte:FAR
EXTRN Ana_El_Tl3t:Byte
EXTRN SMsg:Byte
EXTRN RMsg:Byte
.286
.model small
.stack 64
.data
IY DB 0D
IX DB 0D
OX DB 40D
OY DB 0D
VLine db '#'
Player_Exited_Msg db 'The other person left the chat :(', '$'
.code
ChatScreen proc far  
                 
    mov ax,@data
    mov ds,ax
	
	call inializeScreen
	;CALL intializePort

	
	
	;CODE
	CHECKKEYPRESSED:
	
	;CHECK IF THERE IS A KEY PRESSESD SEND TO THE OTHER USER
	MOV AH,01h
	INT 16H
	JZ CHECKKEYSENT     ;KEY recieved 
	oout:
	MOV AH,00
	INT 16H  

	CMP AL,1BH
	JE EXIT       
	
    cmp al,' '         ;if space or enter it is ok
    je vvv
	cmp al,8
	je vvv
    cmp al,0dh
    je vvv
    
   
     
    cmp al, 'z'      ;out of range for letters
    ja oout
    cmp al, 21h
    jb oout
   ; cmp al, 'Z'
	;    jb oout
	;    cmp al, 'a'
	;    jb oout
    
    vvv:  
	
	call WRITEINPUT
	CALL SENDKEY
	

	CHECKKEYSENT:
	;CHECK STATE IF THERE IS DATA RECIVED
	;IF THERE IS NO DATA RECIVED
	MOV DX,3FDH
	IN AL,DX
	AND AL,1
	JZ CHECKKEYPRESSED
	;IF THERE IS DATA RECIVED
	;RECIVE DATA AND CALL WRITE IN OUTPUT PROC
	MOV DX,03F8H
	IN AL,DX
	cmp al, 200
	je OtherPlayerExited
	CALL WRITEOUTPUT

	JMP CHECKKEYPRESSED
	;END CODE
	
	
    OtherPlayerExited:
		lea bp, Player_Exited_Msg
        mov al, 1
        mov bh, 0
        mov bl, 4
        mov cx, 33
        mov dl, 24
        mov dh, 12
        push ds
        pop es
        mov ah, 13h
        int 10h
		
		AwaitESC:
        mov ah,1
        int 16h   
        jz AwaitESC
        mov ah,0
        int 16h  
        cmp ah,1
        jne AwaitESC

		mov SMsg, 201
		lea di, SMsg
		call SendByte
        ret         
                 
     EXIT:
	 mov al, 200
	 call SENDKEY
	 call SCROLLInputScreen
	 call SCROLLOutputScreen
	 mov Ana_El_Tl3t, 1
	ret 
	
ChatScreen ENDP
	
	
	
	
	
	
intializePort proc
	mov dx,3fbh ;line control register
	mov al,10000000b ;set divisor latch access bit
	out dx,al ;out it

	mov dx,3f8h 
	mov al,0ch
	out dx,al

	mov dx,3f9h
	mov al,00h
	out dx,al

	mov dx,3fbh
	mov al,00011011b
	out dx,al

	ret
intializePort endp

WRITEINPUT PROC
	cmp IY,24d
    jb kammelI
    cmp IX,37
    jb kammelI
    call SCROLLInputScreen
    mov IY,24d
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
	mov ix,38
	cmp iy,0
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
	cmp Iy,24d
	jb cont1
	cmp ix,38d        ;to check the borders before writing the char
	jb cc
	CALL newILine
	mov AH,2
	mov DL,IX
	MOV DH,IY
	int 10h
	call SCROLLInputScreen
	RET
	cc:
	call SCROLLInputScreen
	call newILine
	mov IY,24d
	mov AH,2
	mov DL,IX
	MOV DH,IY
	int 10h
	ret
	cont1:
	CMP AL,13d
	JE IENTER
	CMP ix,38
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
	cmp OY,24d
    jb kammelO
    cmp Ox,78
    jb KammelO
    call SCROLLOutputScreen
    mov OY,24d
    mov Ox,40
    kammelO:
	cmp al,8
	jne notback2
	cmp ox,40
	je fo
	mov ah,2
	mov dl,8
	int 21h
	mov dl,' '
	int 21h
	mov dl,8
	int 21h
	fo:
	cmp ox,40
	jbe ohno1
	dec OX
	mov AH,2
	mov DL,oX
	MOV DH,oY
	int 10h
	ret
	ohno1:
	mov ox,78
	cmp oy,0
	jne ohno2
	mov ox,40
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
	cmp oy,24d
	jb cont2
	cmp ox,78d
	jb cc1
	call SCROLLOutputScreen      ;before scrolling the screen ->check the y if equal 24 and x if equal 79 the last place of the crusor ,if true scroll
	RET
	cc1:
	call SCROLLOutputScreen
	call newOLine
	mov oY,24d
	mov AH,2
	mov DL,oX
	MOV DH,oY
	int 10h
	ret
	cont2:
	CMP AL,13d
	JE OENTER
	CMP ox,78
	jb p2
	mov oX,40
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

SENDKEY PROC
MOV DX,3F8H
OUT DX,AL
RET
SENDKEY ENDP

inializeScreen proc
	mov ah,0
	mov al,3h          ;to open text mode
	int 10h

	mov ah,2
	mov dl,39
	mov dh,0
	mov cx,25         ;the length of the screen
	lp:
		mov ah,2      ;to move the cursor the the values of(dl,dh)
		int 10h
		mov dl,186   ;to print H to separate the screen at the middle
		int 21h
		mov dl,39
		inc dh
	LOOP lp

	mov al,0h     ; function 6
	mov ah,6h
   mov bh, 8Fh       ; normal video attribute         
   mov ch,0       ; upper left Y
   mov cl,40        ; upper left X
   mov dh,24    ; lower right Y
   mov dl,79      ; lower right X 
   int 10h  

   mov al,0h     ; function 6
	mov ah,6h
   mov bh,8Fh       ; normal video attribute         
   mov ch,0       ; upper left Y
   mov cl,0        ; upper left X
   mov dh,24    ; lower right Y
   mov dl,38      ; lower right X 
   int 10h  
   
   	
	mov IY , 0D
	mov IX , 0D
	mov OX , 40D
	mov OY , 0D 
	
	mov ah,2
	mov bh,0
	mov dl,0
	mov dh,0
	int 10h

	mov ax,1003h
	mov bx,0
	int 10h

	ret
inializeScreen endp


SCROLLInputScreen proc
	pusha
	mov al,1h     ; function 6
	mov ah,6h
	mov bh,8Fh       ; normal video attribute         
	mov ch,0       ; upper left Y
	mov cl,0        ; upper left X
	mov dh,24    ; lower right Y
	mov dl,38      ; lower right X 
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
	mov bh,8Fh       ; normal video attribute         
	mov ch,0       ; upper left Y
	mov cl,40        ; upper left X
	mov dh,24    ; lower right Y
	mov dl,79      ; lower right X 
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
	mov OX,40
	inc OY
	ret
newOLine endp

end 

