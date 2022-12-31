; EXTRN GameScreenLocal:FAR
EXTRN GameScreenMulti:FAR
EXTRN LoadAssets:FAR
EXTRN RecieveByte:FAR
EXTRN SendByte:FAR
EXTRN PrintNumber:FAR
EXTRN Player:Byte
EXTRN SMsg:Byte
EXTRN RMsg:Byte
EXTRN Mode:Byte
Public name1
Public name2



.286
.model small
.stack 64    

.data       

    name1 db 15, ?, 15 dup("$")
    name2 db 15, ?, 15 dup("$")
    ;name2 db 16 dup("$")
    Enter_Name db 'Please Enter Your Name','$'
    Press_Enter db 'Press any button To contiue','$';27
    Greeting db 'Hello $'
    ChatMsg db 'To start chatting press F1','$'; 25
    StartGameMsgLocal db 'To start the game press local mode F2','$';37
    StartGameMsgMulti db 'To start the game press F3','$';26
    CloseMsg db 'To end the program press ESC','$' ;28
    numB db 0     
    numAns db 0


.code




;description
Main PROC
    mov ax,@data
    mov ds,ax

    call LoadAssets

    ; initinalize COM
    ;Set Divisor Latch Access Bit
    mov dx,3fbh 			; Line Control Register
    mov al,10000000b		;Set Divisor Latch Access Bit
    out dx,al				;Out it
    ;Set LSB byte of the Baud Rate Divisor Latch register.
    mov dx,3f8h			
    mov al,0ch			
    out dx,al

    ;Set MSB byte of the Baud Rate Divisor Latch register.
    mov dx,3f9h
    mov al,00h
    out dx,al

    ;Set port configuration
    mov dx,3fbh
    mov al,00011011b
    out dx,al
    
    
    lea bx, name1
    call GetNameScreen
    lea bx,name2
    call GetNameScreen
    
    MainMenu:
        call MainMenuScreen
        lea di,RMsg
        GetInput:
            call RecieveByte
            cmp RMsg, 'G' ;recieved the letter 'G' means that the other player is sending a game invite
            ;mov al, RMsg
            ;call PrintNumber
            je BGameMode2

            mov ah, 1
            int 16h  
            jz GetInput

            mov ah, 0
            int 16h  
            

            ; cmp ah, 3Bh
            ; je Chat    
            
            ; cmp ah, 3Ch
            ; je GameMode1    
            
            cmp ah, 3Dh
            je SendInvite   

            cmp ah, 01
            je EndLabel   

            jmp GetInput

    Chat: 
        ;To Be Implemented          


    ; GameMode1: ; 2 players on same device
    ;     mov Mode, 0
    ;     call GameScreenLocal
    ;     jmp MainMenu
    

    BGameMode2:
        mov RMsg, 9
        mov Player, 'B'
        jmp GameMode2 
    SendInvite:
        mov SMsg, 'G'
        lea di,SMsg
        call SendByte
        mov SMsg, 9
        mov Player , 'W'
    GameMode2: ; 2 players on different devices
        mov Mode, 1
        call GameScreenMulti
        jmp MainMenu
    
    

    
    EndLabel:
    ;clears screen
    mov ax, 3
    int 10h
    ; return control to operating system
    MOV AH , 4ch
    INT 21H
    hlt
Main ENDP

;description
MainMenuScreen PROC

     ; clear screen
    mov ax, 3
    int 10h

    mov ah,00     ;\ 
    mov al,02     ;| These select the 80x25 video mode
    int 10h
    
    
    MOV AH,2
    MOV BH,00
    MOV DL,27
    MOV DH,8
    INT 10H

    ; print msg 'pls enter name'
    mov ah, 9
    mov dx, offset ChatMsg
    int 21h

     MOV AH,2
    MOV BH,00
    MOV DL,22
    MOV DH,11
    INT 10H

    mov ah, 9
    mov dx, offset StartGameMsgLocal
    int 21h

     MOV AH,2
    MOV BH,00
    MOV DL,27
    MOV DH,14
    INT 10H

    mov ah, 9
    mov dx, offset StartGameMsgMulti
    int 21h

    MOV AH,2
    MOV BH,00
    MOV DL,26
    MOV DH,17
    INT 10H
    ; print 'hello ' + name
    mov ah, 9
    mov dx, offset CloseMsg
    int 21h


   


    ; ; return control to operating system
    ; MOV AH , 4ch
    ; INT 21H
    ret
MainMenuScreen ENDP

;lea buffer in bx
GetNameScreen PROC
    
    mov si,bx

     ; clear screen
    mov ax, 3
    int 10h

    mov ah,00     ;\ 
    mov al,02     ;| These select the 80x25 video mode
    int 10h
    
    ; print msg 'pls enter name'
    mov ah, 9
    mov dx, offset Enter_Name
    int 21h

    mov dl,10
    mov ah,2
    int 21h
    mov dl,13
    mov ah,2

    mov cx, 15d
    ;lea bx, name1
   ; mov bx, si
    call Read_Limited_Input
    ; print 'hello ' + name
    mov ah, 9
    mov dx, offset Greeting
    int 21h

    mov ah, 9
    mov dx, si
    int 21h
    ; mov si,bx
    ; mov cl,[si+1] 
    ; add si,2
    ; GN_lp:
    ;     mov ah,2
    ;     mov dl, [si]
    ;     int 21h 
    ;     inc si
    ;     dec cl
    ;     jnz GN_lp

    MOV AH,2
    MOV BH,00
    MOV DL,27
    MOV DH,12
    INT 10H
    ; print msg 'press enter to cont'
    mov ah, 9
    mov dx, offset Press_Enter
    int 21h

    mov ah, 7
    int 21h

    ; ; return control to operating system
    ; MOV AH , 4ch
    ; INT 21H
    ret
GetNameScreen ENDP

Read_Limited_Input proc ; Cx ->  max number of characters to read  
                        ; Bx -> address of variable to put input 
  pusha 
   First_Letter:
        mov ah,07 ; read char without echo
        int 21h    
        jmp Check 
   Valid:
        mov [bx],al          
        inc Bx
        dec cx
        mov dl, al
        mov ah,2
        int 21h
   lp: 
        mov ah,07 ; read char without echo
        int 21h    
        cmp al,8
        jz Back_Space 
        cmp al,13
        jz  Ent 
    ;   jmp Check
    ;   Valid:
        mov dl, al
        mov ah,2
        int 21h
        mov [bx],al          
        inc Bx
  loop lp        
  jmp full

    Check:
        cmp al, 'z'
        ja First_Letter
        cmp al, 'A'
        jb First_Letter
        cmp al, 'Z'
        jb Valid
        cmp al, 'a'
        jb First_Letter
        jmp Valid  
    
    Back_Space:  
        mov dl,8
        mov ah,2
        int 21h
                                                                      
        mov dl,32
        mov ah,2
        int 21h
        
        mov dl,8
        mov ah,2
        int 21h 
        
        dec Bx 
        mov al,'$'
        mov [Bx], al
        inc cx
        cmp cx , 15
        jz First_Letter
        jmp lp
    
    full:
        mov ah,07 ; read char with echo
        int 21h    
        cmp al,8
        jz Back_Space
        cmp al,13
        jz  Ent
        jmp full
    Ent:
        mov dl,10
        mov ah,2
        int 21h
        mov dl,13
        mov ah,2
        int 21h
        
    popa
    ret 
Read_Limited_Input endp


end main