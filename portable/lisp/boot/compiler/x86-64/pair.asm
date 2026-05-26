%macro _CAR 0
	mov BX, [SP]
	and BX, OBJ_ADDR
	mov AX, [BX]
	add SP, WORD_SIZE
%endmacro

%macro _CDR 0
	mov BX, [SP]
	and BX, OBJ_ADDR
	mov AX, [BX + WORD_SIZE]
	add SP, WORD_SIZE
%endmacro	

%macro _CONS 0
%ifdef TARGET_x86_64
	mov DI, [SP]
	mov SI, [SP + WORD_SIZE]
%endif
	call new_pair
	add SP, 2* WORD_SIZE
%endmacro	
