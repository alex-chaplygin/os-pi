%macro PRIM 1
%ifdef   TARGET_x86
	mov BX, prims + %1 * 2 * WORD_SIZE ; адрес в таблице примитивов
	call [BX]  ; адрес функции примитива
	mov DX, [BX + WORD_SIZE] ; DX - число аргументов
	shl DX, 2
	add SP, DX 		; восстанавливаем стек
%endif
%endmacro

%macro PACK 1
	mov SI, %1
	mov BX, NULLOBJ
%%pack_loop:
%ifdef   TARGET_x86
	pop AX
	push BX
	push AX
	call new_pair
	mov BX, AX
	add SP, 8
%endif
	dec SI
	cmp SI, 0
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
