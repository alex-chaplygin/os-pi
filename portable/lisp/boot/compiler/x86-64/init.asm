;; устанавливаем T и NIL в глобальные переменные и константы
	mov AX, [t]
	mov [global_mem], AX
	mov AX, NULLOBJ
	mov [const_mem + WORD_SIZE], AX
	mov [global_mem + WORD_SIZE], AX
	mov [frame_reg], AX
;; загружаем константы
	mov AX, 1
	mov [boot_load], ax
	mov AX, consts
	mov [boot_code], AX
	mov CX, NUM_CONSTS
	xor DI, DI
load_consts:
	push CX
	ALIGN
	call parse
	UNALIGN
	pop CX
	mov [const_mem + DI], AX
	add DI, WORD_SIZE
	loop load_consts
