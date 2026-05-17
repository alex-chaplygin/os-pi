%macro _MAKE_ARRAY 0
	mov AX, [SP]
	shr AX, MARK_BIT
%ifdef TARGET_x86
	NEW_FRAME
	push AX
	call new_empty_array	;AX - указатель на массив
	add SP, 4
	RESTORE_FRAME
%elifdef TARGET_x86_64
	mov DI, AX
	call new_empty_array
%endif
	add AX, ARRAY
	add SP, WORD_SIZE
%endmacro
