CATCH_STRUCT equ 4 * WORD_SIZE	
	
%macro CATCH 1
	mov BX, [catch_top]
	mov [BX], AX		; label
	mov AX, %1
	mov [BX + WORD_SIZE], AX ; addr
	mov AX, [frame_reg]
	mov [BX + 2 * WORD_SIZE], AX ; frame_reg
	mov [BX + 3 * WORD_SIZE], SP ; stack_top
	sub BX, CATCH_STRUCT
	mov [catch_top], BX
%endmacro	

%macro THROW 0
%%loop:	
	mov BX, [catch_top]
	add BX, CATCH_STRUCT
	mov [catch_top], BX
	cmp BX, catch_stack + STACK_SIZE * CATCH_STRUCT
	jge %%end
	mov DX, [BX]		; label
	cmp DX, [SP]		; метка THROW в стеке
	jne %%loop
	mov DX, [BX + 2 * WORD_SIZE] ; frame_reg
	mov [frame_reg], DX
	mov SP, [BX + 3 * WORD_SIZE] ; stack_top
	jmp MWORD [BX + WORD_SIZE]   ; addr
%%end:
	add SP, WORD_SIZE
%endmacro
