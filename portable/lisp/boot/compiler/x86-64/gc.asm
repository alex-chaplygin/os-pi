;;; пометка объекта
%macro MARK_OBJECT 1
%ifdef TARGET_x86
	push MWORD [%1]
%elifdef TARGET_x86_64
	mov DI, [%1]	
%endif
	call mark_object
%ifdef TARGET_x86
	add SP, WORD_SIZE
%endif
%endmacro
	
;;; цикл пометки массива %1 с числом объектов %2
%macro MARK_LOOP 2
	mov REG1, %1
%%loop:
	cmp REG1, %1 + %2 * WORD_SIZE
	je %%after
	MARK_OBJECT(REG1)
	add REG1, WORD_SIZE
	jmp %%loop
%%after:
%endmacro

;;; переместиться по стеку на один кадр
;;; BX - начало кадра
;;; <prev BP> <адрес возврата> <аргументы>
%macro NEXT_FRAME 0
	mov BX, [BX]
%endmacro

;;; сборка мусора
;;; пропускаем 2 кадра (garbage_collect <- создание объекта <- run)
;;; функции создания: new_bignumber, new_function, new_prim_function, new_float, new_pair, new_symbol,
;;; new_empty_array
garbage_collect:
	NEW_FRAME
	SAVE_REGS
	MARK_OBJECT(frame_reg)
	MARK_LOOP const_mem, NUM_CONSTS
	MARK_LOOP global_mem, NUM_GLOBALS
	mov BX, BP		; указывает на начало кадра
	NEXT_FRAME		; garbage collect
%ifdef TARGET_x86	
	NEXT_FRAME		; создание объекта
%endif	
	mov REG1, BX		; указатель внутри кадра
	add REG1, 2 * WORD_SIZE	; пропуск BP и адреса возврата
.stack_loop:
	cmp BX, [first_frame]	; если дошли до кадра run
	je .stack_end
.frame_loop:			; цикл внутри кадра
	cmp REG1, [BX]		; сравниваем с предыдущим BP
	je .frame_end
	MARK_OBJECT(REG1)
	add REG1, WORD_SIZE
	jmp .frame_loop
.frame_end:
	NEXT_FRAME
	add REG1, 2 * WORD_SIZE
	jmp .stack_loop
.stack_end:
	mov BX, [catch_top]
.catch_begin:
	add BX, CATCH_STRUCT
	cmp BX, catch_stack + STACK_SIZE * CATCH_STRUCT
	jge .catch_end
	MARK_OBJECT(BX)		; label
	jmp .catch_begin
.catch_end:	
	call sweep
	RESTORE_REGS
	RESTORE_FRAME
	ret
