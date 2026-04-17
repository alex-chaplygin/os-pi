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
	mov AX, frame_reg
%%env_loop:
	mov BX, [AX]
	and BX, OBJ_ADDR
	mov AX, [BX]
	mov AX, [AX]
	loop %%env_loop
	mov [frame_reg], AX
%%end:
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

%macro FIX_CLOSURE 2
	mov AX, [frame_reg]
	cmp AX, NULLOBJ
	je %%end
	and AX, OBJ_ADDR
	mov BX, [AX]
	mov CX, [BX + WORD_SIZE]
	shr CX, MARK_BIT
	sub CX, %2 - 1
%%env_loop:
	mov AX, [BX]
	and AX, OBJ_ADDR
	mov BX, [AX]
	loop %%env_loop
	add AX, ARRAY
%ifdef TARGET_x86
	mov DX, NULLOBJ
	push DX
	push AX
	push dword %1
	push DX
	call new_function
	add SP, 16
%endif		
%%end:
%endmacro

%macro APPLY 0
	mov BX, [SP] ;fun
	and BX, OBJ_ADDR
	mov AX, [BX + 2 * WORD_SIZE] ;func
	cmp AX, NULLOBJ
	je %%func
%ifdef TARGET_x86
	mov DX, [BX]		; count
	push DX			
	push DX
	push dword [BX + WORD_SIZE] ; nary
	push dword [SP + WORD_SIZE] ; args
	push AX
	call call_form
	add SP, 7 * WORD_SIZE	; call_from - 5, apply - 2
	jmp %%end
%endif		
%%func:
	mov DX, [SP + WORD_SIZE] ; args
	xor CX, CX
%%args_loop:	
	cmp DX, NULLOBJ
	je %%call
	and DX, OBJ_ADDR
	push dword [DX]		; car
	mov DX, [DX + WORD_SIZE] ; cdr
	inc CX
	jmp %%args_loop
%%call:
	push dword [frame_reg]
	mov DX, [AX + 3 * WORD_SIZE] ; env
	mov [frame_reg], DX

	call [AX + WORD_SIZE]	; body
	
	pop dword [frame_reg]
	add SP, 2 * WORD_SIZE	; восстанавливаем параметры APPLY
%%end:	
%endmacro	
