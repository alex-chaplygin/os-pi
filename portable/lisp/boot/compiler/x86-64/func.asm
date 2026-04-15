	;; макросы для работы функций

%macro LOCAL_REF 1
   mov BX, [frame_reg]
   and BX, OBJ_ADDR
   mov BX, [BX]
   mov AX, [BX + (%1 + 2) * WORD_SIZE]
%endmacro

%macro LOCAL_SET 1
   mov BX, [frame_reg]
   and BX, OBJ_ADDR
   mov BX, [BX]
   mov [BX + (%1 + 2) * WORD_SIZE], AX
%endmacro

%macro DEEP_REF 2
   mov BX, [frame_reg]
   %rep %1
       and BX, OBJ_ADDR ;; GET_ARRAY
       mov BX, [BX] ;; ->data
       mov BX, [BX] ;; [0]
   %endrep
   and BX, OBJ_ADDR
   mov BX, [BX]
   mov AX, [BX + (%2 + 2) * WORD_SIZE]
%endmacro

%macro DEEP_SET 2
   mov BX, [frame_reg]
   %rep %1
	and BX, OBJ_ADDR ;; GET_ARRAY
       mov BX, [BX] ;; ->data
       mov BX, [BX] ;; [0]
   %endrep
   and BX, OBJ_ADDR
   mov BX, [BX]
   mov [BX + (%2 + 2) * WORD_SIZE], AX
%endmacro	
	
%ifdef TARGET_x86
%define SAVE_ENV push dword [frame_reg]
%define RESTORE_ENV pop dword [frame_reg]	
%endif
	
%macro SET_ENV 1
	mov AX, [frame_reg]
	cmp AX, NULLOBJ
	je %%end
	and AX, OBJ_ADDR
	mov BX, [AX]
	mov CX, [BX + WORD_SIZE]
	shr CX, MARK_BIT
	sub CX, %1 - 1
%%env_loop:
	mov AX, [BX]
	and AX, OBJ_ADDR
	mov BX, [AX]
	loop %%env_loop
%%end:
	add AX, ARRAY
%endmacro	
	
%macro ALLOC 1
%ifdef TARGET_x86
	mov AX, %1
	add AX, 2
	push AX
	call new_empty_array	;AX - указатель на массив
	add SP, 4
%endif	
	mov DX, [frame_reg]
	mov BX, [AX]		; data
	mov [BX], DX		;сохранили data[0]
	cmp DX, NULLOBJ
	jne %%not_null
	mov dword [BX + WORD_SIZE], 0	;уровень 0
	jmp %%alloc
%%not_null:
	and DX, OBJ_ADDR	;DX - адрес массива frame_reg
	mov SI, [DX]
	mov DI, [SI + WORD_SIZE] ;data[1]
	add DI,1 << MARK_BIT
	mov [SI + WORD_SIZE], DI
%%alloc:
	pop DX			; old frame_reg
	mov DI, BX
	add DI, (%1 + 1) * WORD_SIZE
	%rep %1
	pop dword [DI]
	sub DI, WORD_SIZE
	%endrep
	push DX
	add AX, ARRAY
	mov [frame_reg], AX
%endmacro
