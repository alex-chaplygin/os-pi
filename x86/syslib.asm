global syscall_open
global syscall_create
global syscall_close
global syscall_seek
global syscall_fstat
global syscall_set_attr
global syscall_read
global syscall_write
global syscall_fork
global syscall_exec
global syscall_exit
global syscall_wait

SYS_OPEN equ 0
SYS_CREATE equ 1
SYS_CLOSE equ 2
SYS_SEEK equ 3
SYS_FSTAT equ 4
SYS_SET_ATTR equ 5
SYS_WRITE equ 6
SYS_READ equ 7
SYS_FORK equ 8
SYS_EXEC equ 9
SYS_EXIT equ 10
SYS_WAIT equ 11
SYS_CALL equ 0x80

syscall_open:
	mov eax, SYS_OPEN ; syscall num
	mov ebx, [esp + 4] ; param 1
	int SYS_CALL
	ret

syscall_create:
	mov eax, SYS_CREATE ; syscall num
	mov ebx, [esp + 4] ; param 1
	int SYS_CALL
	ret

syscall_close:
	mov eax, SYS_CLOSE ; syscall num
	mov ebx, [esp + 4] ; param 1
	int SYS_CALL
	ret

syscall_seek:
	mov eax, SYS_SEEK ; syscall num
	mov ebx, [esp + 4] ; param 1
	mov ecx, [esp + 8] ; param 2
	int SYS_CALL
	ret

syscall_fstat:
	mov eax, SYS_FSTAT ; syscall num
	mov ebx, [esp + 4] ; param 1
	mov ecx, [esp + 8] ; param 2
	int SYS_CALL
	ret

syscall_set_attr:
	mov eax, SYS_SET_ATTR ; syscall num
	mov ebx, [esp + 4] ; param 1
	mov ecx, [esp + 8] ; param 2
	int SYS_CALL
	ret

syscall_read:
	mov eax, SYS_READ ; syscall num
	mov ebx, [esp + 4] ; param 1
	mov ecx, [esp + 8] ; param 2
	mov edx, [esp + 12] ; param 3
	int SYS_CALL
	ret

syscall_write:
	mov eax, SYS_WRITE ; syscall num
	mov ebx, [esp + 4] ; param 1
	mov ecx, [esp + 8] ; param 2
	mov edx, [esp + 12] ; param 3
	int SYS_CALL
	ret

syscall_fork:
	mov eax, SYS_FORK ; syscall num
	int SYS_CALL
	ret

syscall_exec:
	mov eax, SYS_EXEC ; syscall num
	mov ebx, [esp + 4] ; param 1
	int SYS_CALL
	ret

syscall_exit:
	mov eax, SYS_EXIT ; syscall num
	mov ebx, [esp + 4] ; param 1
	int SYS_CALL
	ret

syscall_wait:
	mov eax, SYS_WAIT ; syscall num
	mov ebx, [esp + 4] ; param 1
	int SYS_CALL
	ret 
