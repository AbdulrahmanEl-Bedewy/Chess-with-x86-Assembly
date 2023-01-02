; EXTRN GameScreenLocal:FAR
EXTRN GameScreenMulti:FAR
EXTRN ChatScreen:FAR
EXTRN LoadAssets:FAR
EXTRN ReceiveByte:FAR
EXTRN SendByte:FAR
EXTRN PrintNumber:FAR
EXTRN Player:Byte
EXTRN SMsg:Byte
EXTRN RMsg:Byte
EXTRN Mode:Byte
Public name1
Public name2
Public Ana_El_Tl3t



.286
.model small
.stack 64    

.data       

    name1 db 15 dup("$"),'$'
    name2 db 15 dup("$"),'$'
    ;name2 db 16 dup("$")
    Enter_Name db 'Please Enter Your Name','$'
    Press_Enter db 'Press any button To contiue','$';27
    Greeting db 'Hello $'
    ChatMsg db 'To start chatting press F1','$'; 25
    ; StartGameMsgLocal db 'To start the game press local mode F2','$';37
    StartGameMsgMulti db 'To start the game press F2','$';26
    CloseMsg db 'To end the program press Esc','$' ;28
    ; CloseMsg db 'To end the program press ESC','$' ;28
    InvitMsg db ' invited you to a game to accept press f2','$' ;43
    InvitChatMsg db ' invited you to chat to accept press f1','$' ;43
    InvitMsgClear db '                                                           ','$' ;43
    Waiting_For_Msg db 'Waiting for player 2','$' ;19
    numB db 0     
    numAns db 0
    Invited_To_Game db 0 ;0 default 1: i was sent and invite 2: i sent and invite 
    Invited_To_Chat db 0 ;0 default 1: i was sent and invite 2: i sent and invite 
    SentInvite db 0

    Ana_El_Tl3t db 0 


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

GetPlayer2Name:
    mov Invited_To_Game, 0
    mov Invited_To_Chat, 0
    mov al,'$'
    lea di, name2
    GPN:
        cmp [di], al
        je GPN1
        mov [di], al
        inc di
        jmp GPN
    GPN1:

    call MainMenuScreen
    
    MOV AH,2
    MOV BH,00
    MOV DL,0
    MOV DH,22
    INT 10H
    ; print 'hello ' + name
    mov ah, 9
    mov dx, offset Waiting_For_Msg
    int 21h

    call ExchangeNames

    MOV AH,2
    MOV BH,00
    MOV DL,0
    MOV DH,22
    INT 10H
    ; print 'hello ' + name
    mov ah, 9
    mov dx, offset InvitMsgClear
    int 21h

        
    MainMenu:
        call MainMenuScreen

        lea di,RMsg
        GetInput:
            call ReceiveByte
            cmp RMsg, 'G' ;recieved the letter 'G' means that the other player is sending a game invite G:game
            je SetInvitationGame
            
            cmp RMsg, 'B' ;recieved the letter 'B' means that the other player has left the program. B:bye
            je GetPlayer2Name

            cmp RMsg, 'C' ;recieved the letter 'B' means that the other player is sending a chat invite. C:chat
            je SetInvitationChat


    
            mov ah, 1
            int 16h  
            jz GetInput
            mov ah, 0
            int 16h  
            

            cmp ah, 3Bh
            je SendInviteChatMid    
            
            ; cmp ah, 3Ch
            ; je GameMode1    
            
            cmp ah, 3Ch ;3Dh
            je SendInviteGame   

            cmp ah, 01h
            je EndLabelMid   

            jmp GetInput

    EndLabelMid:jmp EndLabel
    SendInviteChatMid:jmp SendInviteChat

    SetInvitationGame:
        cmp Invited_To_Game,2
        jne SI_1
        mov Player , 'W'
        jmp GameMode2

        SI_1:
        mov Invited_To_Game, 1
        MOV AH,2
        MOV BH,00
        MOV DL,0
        MOV DH,22
        INT 10H
        ; print 'hello ' + name
        mov ah, 9
        mov dx, offset name2
        int 21h
        mov ah, 9
        mov dx, offset InvitMsg
        int 21h
        jmp GetInput

    SendInviteGame:
        mov SMsg, 'G'
        lea di,SMsg
        call SendByte

        cmp Invited_To_Game, 1
        jne SI_2
        mov Player , 'B'
        jmp GameMode2

        SI_2:
        lea di,RMsg
        mov Invited_To_Game, 2
        jmp GetInput

    
    SetInvitationChat:
    ;see if i already sent an invite before if so then this msg is a respond to the invite and we should join the chat
        cmp Invited_To_Chat,2
        jne SI_3
        jmp Chat
    ;if not then im being invited now, display invite and set flag
        SI_3:
        mov Invited_To_Chat, 1
        MOV AH,2
        MOV BH,00
        MOV DL,0
        MOV DH,20
        INT 10H
        ; print 'hello ' + name
        mov ah, 9
        mov dx, offset name2
        int 21h
        mov ah, 9
        mov dx, offset InvitChatMsg
        int 21h
        jmp GetInput

    SendInviteChat:
    ; send notification to other player
        mov SMsg, 'C'
        lea di,SMsg
        call SendByte
    ;check if i was previously invited then now im responding to the invite and should join chat
        cmp Invited_To_Chat, 1
        jne SI_4
        jmp Chat

    ;if i wasn't invited then update flag that i sent an invite
        SI_4:
        lea di,RMsg
        mov Invited_To_Chat, 2
        jmp GetInput
        


    Chat: 
        ;To Be Implemented 
        mov Invited_To_Game, 0
        mov Invited_To_Chat, 0
        call ChatScreen

        cmp Ana_El_Tl3t,1
        je ReturnFromScreen
        jmp MainMenu
             


    ; GameMode1: ; 2 players on same device
    ;     mov Mode, 0
    ;     call GameScreenLocal
    ;     jmp MainMenu
    

    BGameMode2:
        ; mov SMsg, '#'
        ; lea di, SMsg
        ; call SendByte



    
    GameMode2: ; 2 players on different devices
        mov Invited_To_Game, 0
        mov Invited_To_Chat, 0
        mov Mode, 1
        call GameScreenMulti
        ;mov Invited_To_Game , 0
        cmp Ana_El_Tl3t,1
        je ReturnFromScreen
        jmp MainMenu
    
    
    ReturnFromScreen:
        call MainMenuScreen
    
        MOV AH,2
        MOV BH,00
        MOV DL,0
        MOV DH,22
        INT 10H
        ; print 'hello ' + name
        mov ah, 9
        mov dx, offset Waiting_For_Msg
        int 21h

        lea di, RMsg
        mov dh, 201
         Q21:
            call ReceiveByte
            cmp [di], dh
            jne Q21
        mov Ana_El_Tl3t,0
        jmp MainMenu
    

    EndLabel:
    mov SMsg, 'B'
    lea di,SMsg
    call SendByte

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

    ;  MOV AH,2
    ; MOV BH,00
    ; MOV DL,22
    ; MOV DH,11
    ; INT 10H

    ; mov ah, 9
    ; mov dx, offset StartGameMsgLocal
    ; int 21h

    MOV AH,2
    MOV BH,00
    MOV DL,27
    MOV DH,11
    INT 10H

    mov ah, 9
    mov dx, offset StartGameMsgMulti
    int 21h

    MOV AH,2
    MOV BH,00
    MOV DL,26
    MOV DH,14
    INT 10H
    ; print 'hello ' + name
    mov ah, 9
    mov dx, offset CloseMsg
    int 21h

    MOV AH,2
    MOV BH,00
    MOV DL,0
    MOV DH,16
    INT 10H

    mov cx, 80
    PrintHLine:
        mov ah,2
        mov dl, 196
        int 21h         
    loop PrintHLine


   


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

;description
ExchangeNames PROC
    mov Rmsg , 9
    lea di, RMsg
    call ReceiveByte
    cmp RMsg, 9
    je SendName
    
    ; jmp SendName
    

    ; push di
    ;     mov SMsg, 'N'
    ;     lea di, SMsg
    ;     call SendByte
    ; pop di
    ReceiveName:
    ; call ReceiveByte
    ; cmp RMsg, '#'
    ; je ReceiveName

    lea si, name2
    ; lea di, RMsg
    mov al,[di]
    mov [si],al
    inc si
    push di
            mov SMsg, 'N'
            lea di, SMsg
            call SendByte
    pop di

    mov al, '$'
    mov dh, 9
    ReceiveNameLP :
            Q2:
            call ReceiveByte
            cmp [di], dh
            je Q2

        push di
            mov SMsg, 'N'
            lea di, SMsg
            call SendByte
        pop di

        cmp [di], al
        je NameRecieved
        mov ah,[di]
        mov [si], ah
        inc si
        jmp ReceiveNameLP

    NameRecieved:
    mov ah, 9
    mov dx, offset name2
    int 21h


    SendName:
        
    mov bl, 9
    ;     C1:
    ;         mov SMsg, '#'
    ;         lea di,SMsg
    ;         call SendByte
    ;         lea di,Rmsg
    ;         call ReceiveByte
    ;         cmp [di], bl
    ;         je C1

    lea di, name1
    mov bh, '$'
    SendName2:

        cmp [di],bh
        je NameSent2
        call SendByte
        
        Q9:

            mov ah, 1
            int 16h  
            jz SkipCheckInput
            cmp ah,1
            jne SkipInput
            ret
            SkipInput:
            mov ah, 0
            int 16h  
            SkipCheckInput:
            push di
            lea di,Rmsg
            call ReceiveByte
            cmp [di], bl
            pop di
            je Q9
            
    Continue_Send:
        inc di
        jmp SendName2
    NameSent2:
    call SendByte
    Q10:
    lea di,Rmsg
    call ReceiveByte
    cmp [di], bl
    je Q10

    cmp name2, '$'
    je ReceiveName2
    ret


   ReceiveName2:
    call ReceiveByte

    lea si, name2
    lea di, RMsg

    mov al, '$'
    mov dh, 9
    ReceiveNameLP2:
            Q22:
            call ReceiveByte
            cmp [di], dh
            je Q22

        push di
            mov SMsg, 'N'
            lea di, SMsg
            call SendByte
        pop di

        cmp [di], al
        je NameRecieved2
        mov ah,[di]
        mov [si], ah
        inc si
        jmp ReceiveNameLP2

    NameRecieved2:
    ret
ExchangeNames ENDP
end main