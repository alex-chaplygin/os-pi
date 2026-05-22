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
