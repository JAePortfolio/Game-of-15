
; You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt 



;*************************************CREATED IN EMU8086***************************************;


;Computer Assembly- Tu/Thur 2-3:15pm
;John Arena - Last 4: 9781
;Used emu8086 some syntax may be different then TASM    
org 100h
 
   
start:
    mov ax, @data
    mov ds, ax
    
    mov ax, 0B800h
    mov es, ax
    
    mov ax, 0003h ; Size of 80x25(al=03, ah=00)
    int 10h
;    mov ah, 06h
;    xor al, al                  ; clear screen
;    xor cx, cx
;    mov dx, 184fh               ; upper left, lower right
;    mov bh, 2fh                 ; background green, foreground white
;    int 10h                     ; interrupt call

    xor di, di                  ; vid mem offset 0
    mov ax, 02f00h + ' '        ; green background, white foreground
    mov cx, 80*25               ; This amount is stored in cx and cx counts down from for the instruction below
    rep stosw                   ; clears screen by overwriting memory
    
    ;******* ALTERNATIVE WAY TO COLOR SCREEN BELOW********;
    mov ah, 06h                 ; Scroll up function
    mov ch, 2                   ; Upper left corner CH=row, CL=column
    mov cl, 2
    mov dx, 1634H               ; lower right corner DH=row, DL=column 
    mov bh, 6fh                 ; YellowOnBlue
    INT 10H
    
    
    ;formula for offsets are (y*80 +1)*2
    mov bx, (0*80+1)*2                   ; start at offset of [1,0] character ([x,y]) (top horizontal line of border)
    mov di, (24*80+1)*2                ; (bottom horizontal line of border)    
    l1:
       mov al, 196
       mov ah, 0fh
       ; mov ah, 9Fh- COLORTEST
       mov ptr es: [bx], ax    ; display '-' at "coordinate" bx
       mov ptr es: [di], ax    ; same as above
       add bx, 2                ; move 2 horizontally
       add di, 2                ; same as above
       cmp bx, 158              ; compare to the width of the window (160-2 because want room for a box corner)
       jl l1                    ; loop if less than the width
    mov bx, 160                 ; start offset 160 (left vertical line of border)
    l2: 
        mov al, 179
        mov ah, 0fh 
        mov ptr es: [bx], ax   ; display vertical dash at "coordinate"
        mov ptr es: [bx+158], ax   ; same as above
        add bx, 160             ; move "1" down horizontally
        cmp bx, 3840            ; compare to height of the window
        jl l2                   ; loop if less than the  height
    mov bx, 164                 ; start of offset for game "box" (top horizontal)
    grHor:
        mov  ptr es: [bx], 196   ; print a dash
        mov byte ptr es: [bx+3520], 196 ;3684
        add bx, 2               ; move 2 horizontally
        cmp bx, 266             ; compare with game "box" width (266-164=102)
        jl grHor                ; loop if less than width
    mov bx, 322                 ; start of offset for game "box" (left vertical)
    grVer:
        mov al, 179
        mov ah, 3fh
        mov  ptr es: [bx], ax   ; print vertical dash
        mov byte ptr es: [bx+104], 179 ; 426
        add bx, 160             ; move "1" vertically
        cmp bx, 3682            ; compare with game "box" height (3682-322=3660)
        jl grVer                ; loop if less than height
    mov bx, 964                 ; horizontal grid line 1
    grHorL:
        mov byte ptr es: [bx], 196   ; print dash
        mov byte ptr es: [bx+800], 196;1764
        mov byte ptr es: [bx+1760], 196 ; 2724
        add bx, 2               ; move 2 horizontally
        cmp bx, 1066            ; compare width of game "box",(1066-964=102)
        jl grHorL               ; loop if less than "box" width
    mov bx, 348                 ; vertical grid line 1
    grVerL:
        mov byte ptr es: [bx], 179   ; print vertical dash
        mov byte ptr es: [bx+26], 179 ;374
        mov byte ptr es: [bx+52], 179     ;400
        add bx, 160             ; move "1" vertically downward
        cmp bx, 3708            ; compare height of game "box" (3708-348=3360)
        jl grVerL
    mov bx, 988                 ; This is where I will start a "+" for grid line crossings
    grInt:
        mov byte ptr es: [bx], 197   ; ascii code for the "+" symbol
        mov byte ptr es: [bx+800], 197;1788
        mov byte ptr es: [bx+1760], 197;2748
        add bx, 26              ; move across a certain amount to be approximately centered-
        cmp bx, 1066            ; compare with the game "box" width
        jl grInt                ; loop of less than width
    mov bx, 188                 ; This is where I will start a special character that looks-
    grVerEnds:                  ; for the top of the game "box"
        mov byte ptr es: [bx], 194   ; special character for the left side
        mov byte ptr es: [bx+3520], 193   ; special character for the right side
        add bx, 26               
        cmp bx, 266             ; compare width game "box" width
        jl grVerEnds            ; loop of less than width
    
                                 
    mov al, 187
    mov ah, 0fh
    mov bx, 158                       ; Since there is no perfect way to display all of the-
    mov ptr es: [bx], ax              ; corners, you have to do each one individually
    mov byte ptr es: [266], 187       ; With that said, these each places corners for the border-
    mov al, 201
    mov ah, 0fh
    mov ptr es: [bx-158], ax       
    mov byte ptr es: [162], 201
    mov al, 188
    mov ah, 0fh
    mov ptr es: [bx+3840], ax
    mov byte ptr es: [3786], 188
    mov byte ptr es: [3682], 200
    mov al, 200
    mov ah, 0fh
    mov ptr es: [bx+3682], ax
    mov byte ptr es: [962], 195
    mov byte ptr es: [1762], 195
    mov byte ptr es: [2722], 195
    mov byte ptr es: [1066], 180
    mov byte ptr es: [1866], 180
    mov byte ptr es: [2826], 180 
    
    mov bx, 654                 ; start offset for printing board numbers
    printValuesLoop:
        mov di, arrayIndex
        mov si, arrayOne[di]
        add arrayIndex, 2
        mov ax, si              ; assign variable value
        mov cl, 10              
        div cl                  ; Divide by 10 to get answer and remainder
        mov digitOne, al        ; answer
        mov digitTwo, ah        ; remainder
        add al, 48              ; gives the ascii code 48 to the lower bit
        add ah, 48              
        mov byte ptr es: [bx], al    ; display number
        mov byte ptr es: [bx+2], ah
        cmp count, 3            ; counter for determining whether you get to edge of game "box"
        jz thenblock            ; if zero flag set to 0, jump to then block (3-3)
        inc count               ; increment counter (emu8086 doesnt support inc, byte count-
        add bx, 26              ; - so i used add instead
        cmp bx, 3306            ; above line is move horizontally 2, and cmp to bottom right -
        jl printValuesLoop      ; -edge of game "box". loop
        jmp                     ; jmp out of this if statement when task is done
    thenblock:
        mov count, 0            ; reset counter
        add bx, 722             ; move one row (row as in row of boxes) using this offset
        cmp bx, 3306            ; this compare to check if you are at the bottom right, -
        jl printValuesLoop      ; if not, jump back to if statement printValuesLoop     
    
    
    mov bx, 2194
    xor si, si
    printString:
		mov ah, 00h                   ;hollow out upper AX
        mov al, string1[si]           ;get character in string
        mov byte ptr es: [bx], al     ;print character at position
        mov al, string2[si]           ;Same for other strings just with offset
        mov byte ptr es: [bx+160], al
        mov al, string3[si]
        mov byte ptr es: [bx+320], al
        mov al, string4[si]
        mov byte ptr es: [bx+480], al
        mov al, string5[si]
        mov byte ptr es: [bx+640], al
        mov al, string6[si]
        mov byte ptr es: [bx+800], al
        mov al, string7[si]
        mov byte ptr es: [bx-1762], al
        mov al, string8[si]
        mov byte ptr es: [bx-1600], al 
        mov al, string9[si]
        mov byte ptr es: [bx-1440], al
        mov al, string10[si]
        mov byte ptr es: [bx-1280], al
        mov al, string11[si]
        mov byte ptr es: [bx-1120], al
        mov al, string12[si]
        mov byte ptr es: [bx-960], al
        mov al, string13[si]
        mov byte ptr es: [bx-160], al
        add bx, 2                     ; Next character over
        inc si                        ; Increment index
        cmp si, 21                    ; Length of longest string, a sort of cheat to loop-
        jb printString                ; -through all the strings by adding spaces to have = size
            
    mov ah, 02h                     ;Set to set cursor position mode
    mov bh, 0                       ;Page number
    mov dh, 14                      ;Set y value : these numbers will select number above empty-
    mov dl, 33                      ;Set x value : -space at start of the game
    int 10h                         ;Interrupt call 
    gameFunctions:
        mov ah, 01h                 ; Set cursor type mode
     	mov ch, 6
     	mov cl, 7
     	mov ah, 1                   ; Set to underline cursor
     	int 10h                     ; Interrupt call
        mov ah, 00h                 ; Get keystroke from keyboard buffer
        int 16h                     ; Interrupt call
        cmp ah, 72                  ; Check if keystroke equal to BIOS scan code for UP key-
        je keyUp                    ; if equal, jump to keyUp
        cmp ah, 80                  ; Check equal to BIOS scan code for DOWN key
        je keyDown                  ; equal than jump to keyDown
        cmp ah, 75                  ; Check equal to BIOS scan code for LEFT key
        je keyLeft                  ; equal than jump to keyLeft
        cmp ah, 77                  ; Check equal to BIOS scan code for RIGHT key
        je keyRight                 ; equal than jump to keyRight 
        cmp ah, 28                  ; Check equal to BIOS scan code for ENTER key-
        je keyEnter                 ; equal than jump to keyEnter
        cmp al, 27                  ; Check equal to ascii code for ESC key
        je keyESC                   ; equal than jump to keyESC
        jmp gameFunctions           ; otherwise loop back
        keyUp:                          
        mov ah, 02h                 ; Set cursor position mode  
        mov bh, 0                   ; Page number
        cmp dh, 4                   ; Check if cursor is at the highest it can go on board- 
        je gameFunctions            ; to prevent going off screen-If it is, don't move up, jmp back       
        sub dh, 5                   ; Otherwise, mov 5 positions "upward" (currentY-5) 
        int 10h                     ; Interrupt call
        jmp gameFunctions           ; Jump back to game functions
    keyDown:                          
        mov ah, 02h                 ; Set cursor position mode  
        mov bh, 0                   ; Page number
        cmp dh, 19                  ; Check if cursor is at the lowest it can go on board- 
        je gameFunctions            ; to prevent going off screen-If it is, don't move down, jmp back       
        add dh, 5                   ; Otherwise, mov 5 positions "downward" (currentY+5) 
        int 10h                     ; Interrupt call
        jmp gameFunctions           ; Jump back to game functions
    keyLeft:
        mov ah, 02h                 ; Set cursor position mode
        mov bh, 0                   ; Page number
        cmp dl, 7                   ; Check if cursor is at the left most side of the board-
        je gameFunctions            ; to prevent going off screen-Ifit is, don't move left, jmp back
        sub dl, 13                  ; Otherwise, mov 7 positions left
        int 10h                     ; Interrupt call
        jmp gameFunctions           ; Jump back to game functions
    keyRight:
        mov ah, 02h                 ; Set cursor position mode
        mov bh, 0                   ; Page number
        cmp dl, 46                  ; Check if cursor is at the right most side of the board-
        je gameFunctions            ; to prevent going off screen-Ifit is, don't move right, jmp back
        add dl, 13                  ; Otherwise, mov 7 positions right
        int 10h                     ; Interrupt call
        jmp gameFunctions           ; Jump back to game functions 
    keyESC:
        xor di, di                  ; vid mem offset 0
        mov ax, 00700h + ' '        ; green background, white foreground
        mov cx, 80*25               ; This amount is stored in cx and cx counts down from for the instruction below
        rep stosw                   ; clears screen by overwriting memory
        ret
    keyEnter:
        mov ah, 01h                 ; Set text-cursor mode shape
        mov ch, 0                   ; Box shaped cursor
     	mov cl, 7
     	mov ah, 1
     	int 10h                     ; Interrupt call
        mov al, dh                  ; al equal to y position
        mov bl, 80                  ; bl equal to 80
        mul bl                      ; ax=y*80
        mov cl, dl                  ; cl holds x value
        mov ch, 0                   ; 0 out the higher bits (so we can increase data size)
        add ax,cx                   ; Formula (y*80+x)
        shl ax, 1                   ; Full formula (y*80+x)*2, so shift left for multiply by 2
        mov location, ax            ; Location on screen set to ax's value
        mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h
     	;mov bh, ah                 ; Store attribute of character in BH
     	mov digOneOrig, al          ; Store ascii of character in variable
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	inc dl                      ; Move x+1 to get second digit
     	int 10h
     	mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h                     ; Interrupt call
     	mov digTwoOrig, al          
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	dec dl                      ; Move (x+1)-1 to go back to first digit (just so 1st digit 
     	; is underlined, otherwise the player may think we are selecting the second digit 
     	int 10h                     ; Interrupt call
     	mov ah, 00h                 ; get keystroke from keyboard buffer
     	int 16h
     	;mov swapBoolean, 1         ; Setting the "boolean" swap to 1 to get ready for swapping
     	cmp ah, 28
     	je gameFunctions           ; If enter is pressed again, deselect. Return to game functi
        cmp ah, 72                  ; check if keystroke equal to BIOS scan code for UP key-
        je swapUp                    ; if equal, jump to keyUp
        ;mov swapBoolean, 1
        cmp ah, 80                  ; check equal to BIOS scan code for DOWN key
        je swapDown                  ; equal than jump to keyDown
        cmp ah, 75                  ; check equal to BIOS scan code for LEFT key
        je swapLeft                  ; equal than jump to keyLeft
        cmp ah, 77                  ; check equal to BIOS scan code for RIGHT key
        je swapRight                 ; equal than jump to keyRight
        jmp keyEnter 
    swapLeft:
        mov ah, 02h                 ; Set cursor position mode
        mov bh, 0                   ; Page number
        cmp dl, 7                   ; Check if cursor is at the left most side of the board-
        je keyEnter                 ; to prevent going off screen-Ifit is, don't move left, jmp back
        sub dl, 13                  ; Otherwise, mov 7 positions left
        int 10h  
        mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h
     	;mov bh, ah                 ; Store attribute of character in BH
     	mov digOneSwap, al          ; Store ascii of character in variable
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	inc dl                      ; Move x+1 to get second digit
     	int 10h
     	mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h                     ; Interrupt call
     	mov digTwoSwap, al          
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	dec dl                      ; Move (x+1)-1 to go back to first digit (just so 1st digit 
     	; is underlined, otherwise the player may think we are selecting the second digit
     	mov al, digOneSwap
     	mov ah, digTwoSwap 
     	mov cl, digOneOrig
     	mov ch, digTwoOrig
     	mov bx, location
     	mov byte ptr es: [bx], al
     	mov byte ptr es: [bx+2], ah
     	mov byte ptr es: [bx-24], ch
     	mov byte ptr es: [bx-26], cl
     	jmp gameFunctions
    swapRight:
        mov ah, 02h                 ; Set cursor position mode
        mov bh, 0                   ; Page number
        cmp dl, 46                   ; Check if cursor is at the right most side of the board-
        je keyEnter                 ; to prevent going off screen-Ifit is, don't move right, jmp back
        add dl, 13                  ; Otherwise, mov 7 positions right
        int 10h  
        mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h
     	;mov bh, ah                 ; Store attribute of character in BH
     	mov digOneSwap, al          ; Store ascii of character in variable
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	inc dl                      ; Move x+1 to get second digit
     	int 10h
     	mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h                     ; Interrupt call
     	mov digTwoSwap, al          
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	dec dl                      ; Move (x+1)-1 to go back to first digit (just so 1st digit 
     	; is underlined, otherwise the player may think we are selecting the second digit
     	mov al, digOneSwap
     	mov ah, digTwoSwap 
     	mov cl, digOneOrig
     	mov ch, digTwoOrig
     	mov bx, location
     	mov byte ptr es: [bx], al
     	mov byte ptr es: [bx+2], ah
     	mov byte ptr es: [bx+28], ch
     	mov byte ptr es: [bx+26], cl
     	jmp gameFunctions
    swapUp:
        mov ah, 02h                 ; Set cursor position mode
        mov bh, 0                   ; Page number
        cmp dh, 4                   ; Check if cursor is at the left most side of the board-
        je keyEnter                 ; to prevent going off screen-Ifit is, don't move left, jmp back
        sub dh, 5                  ; Otherwise, mov 7 positions left
        int 10h  
        mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h
     	mov digOneSwap, al          ; Store ascii of character in variable
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	inc dl                      ; Move x+1 to get second digit
     	int 10h
     	mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h                     ; Interrupt call
     	mov digTwoSwap, al          
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	dec dl                      ; Move (x+1)-1 to go back to first digit (just so 1st digit 
     	; is underlined, otherwise the player may think we are selecting the second digit
     	mov al, digOneSwap
     	mov ah, digTwoSwap 
     	mov cl, digOneOrig
     	mov ch, digTwoOrig
     	mov bx, location
     	mov byte ptr es: [bx], al
     	mov byte ptr es: [bx+2], ah
     	mov byte ptr es: [bx-798], ch  ; 798
     	mov byte ptr es: [bx-800], cl       ; 800
     	jmp gameFunctions
    swapDown:
        mov ah, 02h                 ; Set cursor position mode
        mov bh, 0                   ; Page number
        cmp dh, 19                  ; Check if cursor is at the left most side of the board-
        je keyEnter                 ; to prevent going off screen-Ifit is, don't move left, jmp back
        add dh, 5                   ; Otherwise, mov 7 positions left
        int 10h  
        mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h
     	mov digOneSwap, al          ; Store ascii of character in variable
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	inc dl                      ; Move x+1 to get second digit
     	int 10h
     	mov ah, 08h                 ; Set read character and attribute at cursor position mode
     	mov bh, 0                   ; Page number that cursor is on
     	int 10h                     ; Interrupt call
     	mov digTwoSwap, al          
     	mov ah, 02h                 ; Set cursor position mode
     	mov bh, 0                   ; Page number
     	dec dl                      ; Move (x+1)-1 to go back to first digit (just so 1st digit 
     	; is underlined, otherwise the player may think we are selecting the second digit
     	mov al, digOneSwap
     	mov ah, digTwoSwap 
     	mov cl, digOneOrig
     	mov ch, digTwoOrig
     	mov bx, location
     	mov byte ptr es: [bx], al
     	mov byte ptr es: [bx+2], ah
     	mov byte ptr es: [bx+800], ch
     	mov byte ptr es: [bx+802], cl
     	jmp gameFunctions
        
;*********************************************************************************************;
                                 ;* * * *DATA* * * *;
                                    
    ;arrayOne dw 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0                    ; testArray
    arrayOne dw 13,2,10,3,1,12,8,4,5,0,9,6,15,14,11,7                    ; testArray
    count db 0                      ; Counter for displaying numbers on board
    arrayIndex dw 0                 ; array index 
    digitOne db 0                   ; Holds 1st digit when transforming array values to ascii
    digitTwo db 0                   ; Holds 2nd digit when transforming array values to ascii
    digOneOrig db ?                 ; Undeclared variable for selected number's first digit
    digTwoOrig db ?                 ; Undeclared variable for selected number's second digit
    digOneSwap db ?                 ; Undeclared variable for swap number's first digit
    digTwoSwap db ?                 ; Undeclared variable for swap number's second digit
    ;swapBoolean db ?               ; 
    location dw ?                   ; Undeclared variable for location of number on screen
    string1 db '[ENTER]-Select/De #  ',0dh,0ah,0,"hi"
    string2 db '[UP]-Move # Up       '
    string3 db '[LEFT]-Move # Left   '
    string4 db '[RIGHT]-Move # Right '
    string5 db '[DOWN]-Move # Down   '
    string6 db '[ESC]-Close Game     '
    string7 db '     THE 15 GAME     '
    string8 db 'Put the numbers in   '
    string9 db 'order from 01-15! But'
    string10 db "Here's the catch, you"
    string11 db 'can only swap with 00'
    string12 db 'Goodluck!            '
    string13 db 'KEYS:                '


end start







