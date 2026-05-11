%define MASK ((1 << MARK_BIT) - 1)

%macro _ARITH 2
	mov AX, [SP]
	test AX, MASK
	jnz %%com
	mov DX, [SP + WORD_SIZE]
	test DX, MASK
	jnz %%com
	%1 AX, DX
	jmp %%exit
%%com:
	call %2
%%exit:	
	add SP, 2 * WORD_SIZE
%endmacro

%macro _COMPARE 2
	mov AX, 1 << MARK_BIT
	mov DX, [SP]
	test DX, MASK
	jnz %%com
	mov CX, [SP + WORD_SIZE]
	test CX, MASK
	jnz %%com
	cmp DX, CX
	%1 %%l
	mov AX, NULLOBJ
%%l:
	jmp %%exit
%%com:
	call %2
%%exit:	
	add SP, 2 * WORD_SIZE
%endmacro
	
%define _ADD _ARITH add, add2
%define _SUB _ARITH sub, sub2
%define _XOR _ARITH xor, bitwise_xor2
%define _LESS _COMPARE jl, less
%define _GT _COMPARE jg, gt
%define _EQUAL _COMPARE je, equal

;;; дописать обработку знаков
%macro _MUL 0
	mov AX, [SP]
	test AX, MASK
	jnz %%com
	shr AX, MARK_BIT
	mov CX, [SP + WORD_SIZE]
	test CX, MASK
	jnz %%com
	mul CX
	cmp DX, 0
	je %%exit
	shl DX, WORD_SIZE - MARK_BIT
	shr AX, MARK_BIT
	add AX, DX
	NEW_FRAME
	push AX
	call new_bignumber
	add SP, WORD_SIZE
	RESTORE_FRAME
%%com:
	call mul2
%%exit:	
	add SP, 2 * WORD_SIZE
%endmacro

;;; дописать обработку знаков
%macro _DIV 0
	mov DX, 0
	mov AX, [SP]
	test AX, MASK
	jnz %%com
	mov CX, [SP + WORD_SIZE]
	test CX, MASK
	jnz %%com
	idiv CX
	shl AX, MARK_BIT
	jmp %%exit
%%com:
	call DIV2
%%exit:	
	add SP, 2 * WORD_SIZE
%endmacro	

%macro _MOD 0
	mov DX, 0
	mov AX, [SP]
	test AX, MASK
	jnz %%com
	mov CX, [SP + WORD_SIZE]
	test CX, MASK
	jnz %%com
	div CX
	mov AX, DX
	jmp %%exit
%%com:
	call mod
%%exit:	
	add SP, 2 * WORD_SIZE
%endmacro
	
%macro _EQ 0
	mov AX, 1 << MARK_BIT
	mov DX, [SP]
	cmp DX, [SP + WORD_SIZE]
	je %%e
	mov AX, NULLOBJ
%%e:	
	add SP, 2 * WORD_SIZE
%endmacro
