%macro PRIM 1
	mov BX, prims + %1 * 2 * WORD_SIZE ; адрес в таблице примитивов
%ifdef   TARGET_x86
	call [BX]  ; адрес функции примитива
	mov DX, [BX + WORD_SIZE] ; DX - число аргументов
	shl DX, 2
	add SP, DX 		; восстанавливаем стек
%elifdef TARGET_x86_64
	mov SI, [BX + WORD_SIZE]
	shl SI, 3
	jmp %%test
%%switch:
	dq %%prim0, %%prim1, %%prim2
%%test:
	jmp [SI + %%switch]
%%prim0:
	jmp %%call
%%prim1:
	pop DI
	jmp %%call
%%prim2:
	pop DI
	pop SI
%%call:	
	call [BX]
%endif
%endmacro

%macro PRIM_CLOSURE_ 2
%ifdef   TARGET_x86
	mov BX, prims + %1 * 2 * WORD_SIZE ; адрес в таблице примитивов
	push dword [BX + WORD_SIZE] ; число аргументов
	mov AX, %2
	push AX
	push dword [BX]
	call new_prim_function
	add SP, 12 		; восстанавливаем стек
%endif
%endmacro

%define PRIM_CLOSURE(n) PRIM_CLOSURE_ n, 0
%define NPRIM_CLOSURE(n) PRIM_CLOSURE_ n, 1
	
%macro PACK 1
	mov REG1, %1
	mov BX, NULLOBJ
%%pack_loop:
%ifdef   TARGET_x86
	pop AX
	push BX
	push AX
	call new_pair
	mov BX, AX
	add SP, 8
%elifdef TARGET_x86_64
	pop DI
	mov SI, BX
	call new_pair
	mov BX, AX	
%endif
	dec REG1
	cmp REG1, 0
	jne %%pack_loop
	push BX
%endmacro
	
%macro NPRIM 1
%ifdef   TARGET_x86
	mov BX, nprims + %1 * 2 * WORD_SIZE ; адрес в таблице примитивов

	call [BX]
	mov DX, [BX + WORD_SIZE] ; DX - число аргументов
	inc DX			 ; еще один аргумент - список
	shl DX, 2
	add SP, DX 		; восстанавливаем стек
%endif
%endmacro
