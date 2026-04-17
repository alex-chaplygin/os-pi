%macro ADD 0
	mov AX, [SP]
	add AX, [SP + WORD_SIZE]
	add SP, 2 * WORD_SIZE
%endmacro

%macro MUL 0
	xor DX, DX
	mov AX, [SP]
	shr AX, MARK_BIT
	mov CX, [SP + WORD_SIZE]
	shr CX, MARK_BIT
	mul CX
	shl AX, MARK_BIT
	add SP, 2 * WORD_SIZE
%endmacro

%macro EQ 0
	mov AX, 1 << MARK_BIT
	mov DX, [SP]
	cmp DX, [SP + WORD_SIZE]
	je %%e
	mov AX, NULLOBJ
%%e:	
	add SP, 2 * WORD_SIZE
%endmacro

%macro LESS 0
	mov AX, 1 << MARK_BIT
	mov DX, [SP]
	cmp DX, [SP + WORD_SIZE]
	jl %%l
	mov AX, NULLOBJ
%%l:	
	add SP, 2 * WORD_SIZE
%endmacro
