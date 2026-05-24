%macro _MAKE_ARRAY 0
	mov AX, [SP]
	shr AX, MARK_BIT
%ifdef TARGET_x86
	sub SP, 4
	NEW_FRAME
	push AX
	call new_empty_array	;AX - указатель на массив
	mov BP, [SP + WORD_SIZE]
	add SP, 3 * WORD_SIZE
%elifdef TARGET_x86_64
	mov DI, AX
	call new_empty_array
%endif
	add AX, ARRAY
	add SP, WORD_SIZE
%endmacro

%macro _ARRAY_SIZE 0
	mov BX, [SP]
	and BX, OBJ_ADDR
	mov AX, [BX + WORD_SIZE]
	shl AX, MARK_BIT
	jno %%not_big
	jns %%not_big
	mov AX, [BX + WORD_SIZE]
%ifdef TARGET_x86
	sub SP, 4
	NEW_FRAME
	push AX
	call new_bignumber
	mov BP, [SP + WORD_SIZE]
	add SP, 3 * WORD_SIZE
%elifdef TARGET_x86_64
	mov DI, AX
	call new_bignumber
%endif	
%%not_big:	
	add SP, WORD_SIZE
%endmacro	

%macro GET_INDEX 0
	mov SI, [SP + WORD_SIZE]
	test SI, MASK
	jnz %%big_index
	shr SI, MARK_BIT
	jmp %%exit
%%big_index:
	and SI, OBJ_ADDR
	mov SI, [SI]
%%exit:
%endmacro	
	
%macro _SETA 0
	mov BX, [SP]
	and BX, OBJ_ADDR
	mov BX, [BX]
	mov DX, [SP + 2 * WORD_SIZE]
	GET_INDEX
	mov [BX + SI * WORD_SIZE], DX
	add SP, 3 * WORD_SIZE
%endmacro
	
%macro _AREF 0
	mov BX, [SP]
	and BX, OBJ_ADDR
	mov BX, [BX]
	GET_INDEX
	mov AX, [BX + SI * WORD_SIZE]
	add SP, 2 * WORD_SIZE
%endmacro
