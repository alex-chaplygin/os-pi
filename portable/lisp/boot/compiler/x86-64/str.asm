CHAR equ 7
	
%macro _CHAR 0
	mov BX, [SP]
	and BX, OBJ_ADDR
	mov BX, [BX]
	GET_INDEX
	xor AX, AX
	mov al, [BX + SI]
	shl AX, MARK_BIT
	add AX, CHAR
	add SP, 2 * WORD_SIZE
%endmacro
