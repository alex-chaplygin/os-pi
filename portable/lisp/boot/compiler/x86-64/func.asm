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
	
%define SAVE_ENV push MWORD [frame_reg]
%define RESTORE_ENV pop MWORD [frame_reg]
	
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
%%end:
	mov [frame_reg], AX
%endmacro	
	
%macro ALLOC 1
%ifdef TARGET_x86
	mov AX, %1 + 2
	push AX
	call new_empty_array	;AX - указатель на массив
	add SP, 4
%elifdef TARGET_x86_64
	mov DI, %1 + 2
	call new_empty_array
%endif	
	mov DX, [frame_reg]
	mov BX, [AX]		; data
	mov [BX], DX		;сохранили data[0]
	cmp DX, NULLOBJ
	jne %%not_null
	mov MWORD [BX + WORD_SIZE], 0	;уровень 0
	jmp %%alloc
%%not_null:
	and DX, OBJ_ADDR	;DX - адрес массива frame_reg
	mov SI, [DX]
	mov CX, [SI + WORD_SIZE] ;data[1]
	add CX, 1 << MARK_BIT
	mov [BX + WORD_SIZE], CX
%%alloc:
	pop DX			; old frame_reg
	mov DI, BX
	add DI, (%1 + 1) * WORD_SIZE
	%rep %1
	pop MWORD [DI]
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
	cld
	mov BX, [SP] ;fun
	and BX, OBJ_ADDR	     ; f
	mov AX, [BX + 2 * WORD_SIZE] ; func
	cmp AX, NULLOBJ
	je %%func
%ifdef TARGET_x86
	mov DX, [BX]		; count
	push DX			
	push DX
	push MWORD [BX + WORD_SIZE] ; nary
	push MWORD [SP + WORD_SIZE] ; args
	push AX
	call call_form
	add SP, 7 * WORD_SIZE	; call_from - 5, apply - 2
	jmp %%end
%endif		
%%func:
%ifdef TARGET_x86	
	mov AX, MAX_ARGS
	push AX
	call new_empty_array
	add SP, WORD_SIZE
%endif
	push MWORD [frame_reg]		; сохраняем текущий frame
	mov DX, [BX + 3 * WORD_SIZE] ; env
	mov DI, [AX]		; data
	mov [DI], DX		;сохранили data[0]
	cmp DX, NULLOBJ
	jne %%not_null
	mov MWORD [DI + WORD_SIZE], 0	;уровень 0
	jmp %%app_alloc
%%not_null:
	and DX, OBJ_ADDR	;DX - адрес массива frame_reg
	mov SI, [DX]
	mov CX, [SI + WORD_SIZE] ;data[1]
	add CX, 1 << MARK_BIT
	mov [DI + WORD_SIZE], CX ; записываем новый уровень
%%app_alloc:	
	mov SI, [SP + WORD_SIZE * 2] ; args
	add DI, 2 * WORD_SIZE	 ; куда записываются аргументы
%%args_loop:	
	cmp SI, NULLOBJ
	je %%call
	and SI, OBJ_ADDR
%ifdef TARGET_x86
	movsd			; записываем car в кадр
%endif	
	mov SI, [SI] ; cdr
	jmp %%args_loop
%%call:
	mov [frame_reg], AX	; устанавливаем новый кадр
	call [BX + WORD_SIZE]	; body
	
	pop MWORD [frame_reg]
	add SP, 2 * WORD_SIZE	; восстанавливаем параметры APPLY
%%end:	
%endmacro
