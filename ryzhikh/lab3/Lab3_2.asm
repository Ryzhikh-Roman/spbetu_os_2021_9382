TESTPC SEGMENT
   ASSUME CS:TESTPC, DS:TESTPC, ES:NOTHING, SS:NOTHING
   ORG 100H
START: JMP BEGIN
; Данные
   avb_mem db  'Available memory (b):                                    ',0DH,0AH,'$'
   exp_mem db  'Expanded memory (Kb):                                     ',0DH,0AH,'$'
   new_str db 0DH,0AH,'$'
   adr db 'Address:       ','$'
   mcb db 'MCB:','$'
   adr_psp db 'Address PSP:       ','$'
   area_size db 'Size:           ','$'
   sc_sd db 'SC/SD:','$'
   free db 0
; Процедуры
;-----------------------------------------------------
TETR_TO_HEX PROC near
   and AL,0Fh
   cmp AL,09
   jbe next
   add AL,07
next:
   add AL,30h
   ret
TETR_TO_HEX ENDP
;-------------------------------
BYTE_TO_HEX PROC near
;байт в AL переводится в два символа шест. числа в AX
   push CX
   mov AH,AL
   call TETR_TO_HEX
   xchg AL,AH
   mov CL,4
   shr AL,CL
   call TETR_TO_HEX ;в AL старшая цифра
   pop CX ;в AH младшая
   ret
BYTE_TO_HEX ENDP
;-------------------------------
WRD_TO_HEX PROC near
;перевод в 16 с/с 16-ти разрядного числа
; в AX - число, DI - адрес последнего символа
   push BX
   mov BH,AH
   call BYTE_TO_HEX
   mov [DI],AH
   dec DI
   mov [DI],AL
   dec DI
   mov AL,BH
   call BYTE_TO_HEX
   mov [DI],AH
   dec DI
   mov [DI],AL
   pop BX
   ret
WRD_TO_HEX ENDP
;--------------------------------------------------
BYTE_TO_DEC PROC near
; перевод в 10с/с, SI - адрес поля младшей цифры
   push CX
   push DX
   ;xor AH,AH
   ;xor DX,DX
   mov CX,10
loop_bd:
   div CX
   or DL,30h
   mov [SI],DL
   dec SI
   xor DX,DX
   cmp AX,10
   jae loop_bd
   cmp AL,00h
   je end_l
   or AL,30h
   mov [SI],AL
end_l:
   pop DX
   pop CX
   ret
BYTE_TO_DEC ENDP
;-------------------------------
WRITESTRING PROC near
   push ax
   mov AH,09h
   int 21h
   pop ax
   ret
WRITESTRING ENDP
;-------------------------------

;-------------------------------
PRINT_AVB_MEM PROC near
   mov ah, 4ah
   mov bx, 0FFFFh
   int 21h
   mov ax, 10h
   mul bx
   mov si, offset avb_mem
   add si, 26
   call BYTE_TO_DEC
   mov dx, offset avb_mem
   call WRITESTRING
   xor dx,dx
   ret
PRINT_AVB_MEM ENDP
;-------------------------------
PRINT_EXP_MEM PROC near
   mov AL,30h 
   out 70h,AL
   in AL,71h
   mov BL,AL;
   mov AL,31h 
   out 70h,AL
   in AL,71h
   mov ah, al
   mov si, offset exp_mem
   add si, 26
   call BYTE_TO_DEC
   mov dx, offset exp_mem
   call WRITESTRING
   ret
PRINT_EXP_MEM ENDP
;-------------------------------
PRINT_MCB PROC near
   push ax
   push bx
   push es
   push dx
   push cx

   mov dx, offset mcb
   call WRITESTRING
   
   mov di, offset adr
   add di, 11
   call WRD_TO_HEX
   mov dx, offset adr
   call WRITESTRING

   mov ax, es:[1]
   mov di, offset adr_psp
   add di, 15
   call WRD_TO_HEX
   mov dx, offset adr_psp
   call WRITESTRING

   mov ax, es:[3]
   mov bx, 10h
   mul bx
   mov si, offset area_size
   add si, 11
   call BYTE_TO_DEC
   mov dx, offset area_size
   call WRITESTRING

   mov dx, offset sc_sd
   call WRITESTRING
   mov bx, 8
   mov cx, 7

print_sc_sd:
   mov dl, es:[bx]
   mov ah, 02h
   int 21h
   inc bx
   loop print_sc_sd

   pop cx
   pop dx
   pop es
   pop bx
   pop ax
ret
PRINT_MCB ENDP
;-------------------------------
PRINT_BLOCK_CHAIN PROC near
   push ax
   push bx
   push es
   push dx
   push cx

   mov ah, 52h
   int 21h
   mov es, es:[bx-2]
   mov ax, es

print:
   call print_mcb
   mov dx, offset new_str
   call WRITESTRING
   mov al, es:[0]
   cmp al, 5ah
je end_print

   mov bx, es:[3]
   mov ax, es
   add ax, bx
   inc ax
   mov es, ax
   jmp print

end_print:
   pop cx
   pop dx
   pop es
   pop bx
   pop ax
ret
PRINT_BLOCK_CHAIN ENDP
;-------------------------------
FREE_MEM PROC near
   mov ah, 4ah
   mov bx, offset free
   int 21h
ret
FREE_MEM ENDP
;-------------------------------
; Код
BEGIN:
   call PRINT_AVB_MEM
   call PRINT_EXP_MEM
   call FREE_MEM
   call PRINT_BLOCK_CHAIN
   xor AL,AL
   mov AH,4Ch
   int 21H
TESTPC ENDS
END START