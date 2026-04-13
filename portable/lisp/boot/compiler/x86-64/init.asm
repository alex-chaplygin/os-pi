;; устанавливаем T и NIL в глобальные переменные и константы
	mov AX, [t]
	mov [global_mem], AX
	mov AX, NULLOBJ
	mov [const_mem + WORD_SIZE], AX
	mov [global_mem + WORD_SIZE], AX
	mov [frame_reg], AX
