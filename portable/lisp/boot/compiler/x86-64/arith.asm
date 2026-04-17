%macro ADD 0
	mov AX, [SP]
	add AX, [SP + WORD_SIZE]
	add SP, 2 * WORD_SIZE
%endmacro

%macro MUL 0
	mov AX, [SP]
	mul MWORD [SP + WORD_SIZE]
	add SP, 2 * WORD_SIZE
%endmacro
