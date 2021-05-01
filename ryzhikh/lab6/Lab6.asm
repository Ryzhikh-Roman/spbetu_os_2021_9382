AStack SEGMENT  STACK
          DW 128 DUP(?)
AStack ENDS

DATA SEGMENT
	P_BLOCK DW 0
               dd 0
               dd 0
               dd 0

	FILE_NAME DB 'LAB2.com', 0	
	flag DB 0
	CMD DB 1h, 0dh
	POS db 128 DUP(0)

	KEEP_SS DW 0
	KEEP_SP DW 0
	KEEP_PSP DW 0

	STR_MEMORY_FREE DB 'Memory was free' , 0dh, 0ah, '$'

    	STR_ERROR_CRASH DB 'Error! MCB crashed!', 0dh, 0ah, '$' 
	STR_ERROR_NO_MEMORY DB 'Error! Not enough memory!', 0dh, 0ah, '$' 
	STR_WRONG_ADDRESS DB 'Error! Invalid memory addressess!', 0dh, 0ah, '$'
	STR_ERROR_NUMBER DB 'Error! Invalid function number!', 0dh, 0ah, '$' 
	STR_ERROR_NO_FILE DB 'Error! File not found!', 0dh, 0ah, '$' 
	STR_DISK_ERROR DB 'Error with disk!', 0dh, 0ah, '$' 
	STR_MEMORY_ERROR DB 'Error! Insufficient memory!', 0dh, 0ah, '$' 
	STR_ENVIRONMENT_ERROR DB 'Error! Wrong string of environment!', 0dh, 0ah, '$' 
	STR_FORMAT_ERROR DB 'Error! Wrong format!', 0dh, 0ah, '$' 
	STR_ERROR_DEVICE DB 0dh, 0ah, 'Error! DEVICE error!' , 0dh, 0ah, '$'
	
	STR_END_CODE DB 0dh, 0ah, 'The program successfully ended with code:    ' , 0dh, 0ah, '$'
	STR_END_CTR DB 0dh, 0ah, 'The program was INTERed by ctrl-break' , 0dh, 0ah, '$'
	STR_END_INTER DB 0dh, 0ah, 'The program was ended by INTERion int 31h' , 0dh, 0ah, '$'
	
	NEW_STRING DB 0DH,0AH,'$'

	DATA_END DB 0
DATA ENDS

CODE SEGMENT
   ASSUME CS:CODE, DS:DATA, SS:AStack


WRITESTRING PROC
 	push ax
 	mov ah, 09h
 	int 21h 
 	pop ax
 	ret
WRITESTRING ENDP


MEMORY_FREE PROC 
	push ax
	push bx
	push cx
	push dx
	
	mov ax, offset DATA_END
	mov bx, offset PR_END
	add bx, ax	
	mov cl, 4
	shr bx, cl
	add bx, 2bh
	mov ah, 4ah
	int 21h 
	jnc END_FREE_MEMORY
	mov flag, 1

CRASH_MCB:
	cmp ax, 7
	jne NOT_MEMORY
	mov dx, offset STR_ERROR_CRASH
	call WRITESTRING
	jmp RET_F

NOT_MEMORY:
	cmp ax, 8
	jne WRONG_ADDRESS
	mov dx, offset STR_ERROR_NO_MEMORY
	call WRITESTRING
	jmp RET_F

WRONG_ADDRESS:
	cmp ax, 9
	mov dx, offset STR_WRONG_ADDRESS
	call WRITESTRING
	jmp RET_F

END_FREE_MEMORY:
	mov flag, 1
	mov dx, offset NEW_STRING
	call WRITESTRING
	mov dx, offset STR_MEMORY_FREE
	call WRITESTRING
	
RET_F:
	pop dx
	pop cx
	pop bx
	pop ax
	ret

MEMORY_FREE ENDP


LOAD PROC
	push ax
	push bx
	push cx
	push dx
	push ds
	push es
	
	mov KEEP_SP, sp
	mov KEEP_SS, ss	
	mov ax, DATA
	mov es, ax
	mov bx, offset P_BLOCK
	mov dx, offset CMD
	mov [bx+2], dx
	mov [bx+4], ds 
	mov dx, offset POS	
	mov ax, 4b00h 
	int 21h 
	
	mov ss, KEEP_SS
	mov sp, KEEP_SP
	pop es
	pop ds
	jnc LOADS
	
	cmp ax, 1
	jne FILE_ERROR

	mov dx, offset STR_ERROR_NUMBER
	call WRITESTRING

	jmp END_LOAD

FILE_ERROR:
	cmp ax, 2
	jne DISK_ERROR
	mov dx, offset STR_ERROR_NO_FILE
	call WRITESTRING
	jmp END_LOAD

DISK_ERROR:
	cmp ax, 5
	jne MEMORY_ERROR
	mov dx, offset STR_DISK_ERROR
	call WRITESTRING
	jmp END_LOAD

MEMORY_ERROR:
	cmp ax, 8
	jne ENVIRONMENT_ERROR
	mov dx, offset STR_MEMORY_ERROR
	call WRITESTRING
	jmp END_LOAD

ENVIRONMENT_ERROR:
	cmp ax, 10
	jne FORMAT_ERROR
	mov dx, offset STR_ENVIRONMENT_ERROR
	call WRITESTRING
	jmp END_LOAD

FORMAT_ERROR:
	cmp ax, 11
	mov dx, offset STR_FORMAT_ERROR
	call WRITESTRING
	jmp END_LOAD

LOADS:
	mov ah, 4dh
	mov al, 00h
	int 21h 
	
	cmp ah, 0
	jne CTRL
	push di 
	mov di, offset STR_END_CODE
	mov [di+44], al 
	pop si
	mov dx, offset NEW_STRING
	call WRITESTRING
	mov dx, offset STR_END_CODE
	call WRITESTRING 
	jmp END_LOAD

CTRL:
	cmp ah, 1
	jne DEVICE
	mov dx, offset STR_END_CTR
	call WRITESTRING
	jmp END_LOAD

DEVICE:
	cmp ah, 2 
	jne INTER
	mov dx, offset STR_ERROR_DEVICE
	call WRITESTRING
	jmp END_LOAD

INTER:
	cmp ah, 3
	mov dx, offset STR_END_INTER
	call WRITESTRING

END_LOAD:
	pop dx
	pop cx
	pop bx
	pop ax
	ret

LOAD ENDP


PATH PROC 
	push ax
	push bx
	push cx 
	push dx
	push di
	push si
	push es
	
	mov ax, KEEP_PSP
	mov es, ax
	mov es, es:[2ch]
	mov bx, 0
	
FIND_PATH:
	inc bx
	cmp byte ptr es:[bx-1], 0
	jne FIND_PATH
	cmp byte ptr es:[bx+1], 0 
	jne FIND_PATH
	add bx, 2
	mov di, 0
	
LOOP_P:
	mov dl, es:[bx]
	mov byte ptr [POS + di], dl
	inc di
	inc bx
	cmp dl, 0
	je END_LOOP_P
	cmp dl, '\'
	jne LOOP_P
	mov cx, di
	jmp LOOP_P

END_LOOP_P:
	mov di, cx
	mov si, 0
	
END_F:
	mov dl, byte ptr [FILE_NAME + si]
	mov byte ptr [POS + di], dl
	inc di 
	inc si
	cmp dl, 0 
	jne END_F
		
	pop es
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax

	ret
PATH ENDP


BEGIN PROC far
	push ds
	xor ax, ax
	push ax
	mov ax, DATA
	mov ds, ax
	mov KEEP_PSP, es
	call MEMORY_FREE 
	cmp flag, 0
	je FINISH
	call PATH
	call LOAD
	
FINISH:
	xor al, al
	mov ah, 4ch
	int 21h

BEGIN ENDP


PR_END:
CODE ENDS
END BEGIN
