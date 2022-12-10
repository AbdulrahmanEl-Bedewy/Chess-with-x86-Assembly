.286
.model small
.stack 64    

.data       

    name1 db 16 dup("$")
    name2 db 16 dup("$")
    Enter_Name db 'Please Enter Your Name$'
    Press_Enter db 'Press Enter To contiue$'
    Greeting db 'Hello $'
    numB db 0     
    numAns db 0

.code
MAIN Proc far
    mov BX,@data
    mov DS,BX
    
    ; mov ax,0600h
    ; mov bh,07a
    ; mov cx,0
    ; mov dx,184FH
    ; int 10h

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
    ; move cursor to bottom of screen
    ; mov ah,2
    ; mov dx,1900h
    ; int 10h
    MOV AH,2
    MOV BH,00
    MOV DL,39
    MOV DH,12
    INT 10H
    ; print msg 'press enter to cont'
    mov ah, 9
    mov dx, offset Press_Enter
    int 21h
    ; move cursor to top of screen
    mov ah,2
    mov dx,0100h
    int 10h
    ; ; print enter
    ; mov dl,10
    ; mov ah,2
    ; int 21h
    ; mov dl,13
    ; mov ah,2
    ; int 21h
    ; input name
    mov cx, 15d
    lea bx, name1
    call Read_Limited_Input
    ; print 'hello ' + name
    mov ah, 9
    mov dx, offset Greeting
    int 21h
    mov ah, 9
    mov dx, offset name1
    int 21h

    ; mov cx, 15d
    ; lea bx, name2
    ; call Read_Limited_Input
    ; mov ah, 9
    ; mov dx, offset name2
    ; int 21h
    
    HLT
main endp



Read_Limited_Input proc ; Cx ->  max number of characters to read  
                        ; Bx -> address of variable to put input 
  pusha
  pushf 
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
    popf
    ret 
endp

end main