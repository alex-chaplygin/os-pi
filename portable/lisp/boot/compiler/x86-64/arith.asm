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
%ifdef TARGET_x86_64
	mov DI, [SP]
	mov SI, [SP + WORD_SIZE]
%endif	
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
%ifdef TARGET_x86_64
	mov DI, [SP]
	mov SI, [SP + WORD_SIZE]
%endif
	call %2
%%exit:	
	add SP, 2 * WORD_SIZE
%endmacro
	
%macro _ADD 0
	mov AX, [SP]
	test AX, MASK
	jnz %%com
	mov DX, [SP + WORD_SIZE]
	test DX, MASK
	jnz %%com
	add AX, DX
	jno %%exit
	sar AX, MARK_BIT - 1
	NEW_FRAME
%ifdef TARGET_x86
	push AX
	call new_bignumber
	add SP, WORD_SIZE
%elifdef TARGET_x86_64
	mov DI, AX
	call new_bignumber
%endif
	RESTORE_FRAME
	jmp %%exit
%%com:
%ifdef TARGET_x86_64
	mov DI, [SP]
	mov SI, [SP + WORD_SIZE]
%endif	
	call add2
%%exit:	
	add SP, 2 * WORD_SIZE
%endmacro
	
%define _SUB _ARITH sub, sub2
%define _XOR _ARITH xor, bitwise_xor2
%define _LESS _COMPARE jl, less
%define _GT _COMPARE jg, gt
%define _EQUAL _COMPARE je, equal

%macro _MUL 0
	mov AX, [SP]
	test AX, MASK
	jnz %%com
	sar AX, MARK_BIT
	mov CX, [SP + WORD_SIZE]
	test CX, MASK
	jnz %%com
	imul CX
	cmp DX, 0
	je %%exit
	shl DX, (WORD_SIZE << 3) - MARK_BIT
	shr AX, MARK_BIT
	add AX, DX
	NEW_FRAME
%ifdef TARGET_x86
	push AX
	call new_bignumber
	add SP, WORD_SIZE
%elifdef TARGET_x86_64
	mov DI, AX
	call new_bignumber
%endif
	RESTORE_FRAME
	jmp %%exit
%%com:
%ifdef TARGET_x86_64
	mov DI, [SP]
	mov SI, [SP + WORD_SIZE]
%endif	
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
%ifdef TARGET_x86_64
	mov DI, [SP]
	mov SI, [SP + WORD_SIZE]
%endif	
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
%ifdef TARGET_x86_64
	mov DI, [SP]
	mov SI, [SP + WORD_SIZE]
%endif
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
