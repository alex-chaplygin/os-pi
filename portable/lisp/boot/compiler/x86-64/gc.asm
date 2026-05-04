;;; цикл пометки массива %1 с числом объектов %2
%macro MARK_LOOP 2
	mov REG1, %1
%%loop:
	cmp REG1, %1 + %2 * WORD_SIZE
	je %%after
%ifdef TARGET_x86
	push MWORD [REG1]
%elifdef TARGET_x86_64
	mov DI, [REG1]
%endif
	call mark_object
%ifdef TARGET_x86
	add SP, WORD_SIZE
%endif
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
	push BP
	mov BP, SP
	SAVE_REGS
%ifdef TARGET_x86
	push MWORD [frame_reg]
%elifdef TARGET_x86_64
	mov DI, [frame_reg]
%endif
	call mark_object
%ifdef TARGET_x86
	add SP, 4
%endif
	MARK_LOOP const_mem, NUM_CONSTS
	MARK_LOOP global_mem, NUM_GLOBALS
	mov BX, BP		; указывает на начало кадра
	NEXT_FRAME		; garbage collect
	NEXT_FRAME		; создание объекта
	mov REG1, BX		; указатель внутри кадра
	add REG1, 2 * WORD_SIZE		; пропуск BP, адрес возврата
.stack_loop:
	cmp BX, [first_frame]	; если дошли до кадра run
	je .stack_end
.frame_loop:			; цикл внутри кадра
	cmp REG1, [BX]		; сравниваем с предыдущим BP
	je .frame_end
%ifdef TARGET_x86
	push MWORD [REG1]
%elifdef TARGET_x86_64
	mov DI, [REG1]	
%endif
	call mark_object
%ifdef TARGET_x86
	add SP, 4
%endif
	add REG1, WORD_SIZE
	jmp .frame_loop
.frame_end:
	NEXT_FRAME
	add REG1, 2 * WORD_SIZE
	jmp .stack_loop
.stack_end:
	call sweep
	RESTORE_REGS
	pop BP
	ret
