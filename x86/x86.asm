bits 32
section .text
        ;multiboot spec
        align 4
        dd 0x1BADB002              ;magic
        dd 0x00                    ;flags
        dd - (0x1BADB002 + 0x00)   ;checksum. m+f+c should be zero

global start, kernel_code, load_idt, load_gdt
global a_isrZeroDivisionException
global a_isrDebugException
global a_isrNonMaskableInterruptException
global a_isrBreakpointException
global a_isrIntoDetectedOverflowException
global a_isrOutOfBoundsException
global a_isrInvalidOpcodeException
global a_isrNoCoprocessorException
global a_isrDoubleFaultException
global a_isrCoprocessorSegmentOverrunException
global a_isrBadTSSException
global a_isrSegmentNotPresentException
global a_isrStackFaultException
global a_isrGeneralProtectionFaultException
global a_isrPageFaultException
global a_isrUnknownInterruptException
global a_isrCoprocessorFaultException
global a_isrAlignmentCheckException
global a_isrMachineCheckException
global a_isrNonExistent
global a_syscall
global test_syscall
global save_regs, restore_regs
global read_port, write_port	
global a_timer
global a_interrupt_handler
global disable_interrupts, enable_interrupts
global a_keyboard_interrupt
extern kmain, exception_handler, sys_call, timer_event,end_of_interrupt		;this is defined in the c file
extern interrupt_handler
extern current_proc
extern keyboard_interrupt	

start:
	cli 				;block interrupts
        mov ax, cs
        mov [kernel_code], ax
	mov esp, stack_space
	call kmain
	hlt 				;halt the CPU

load_idt:
        mov edx, [esp + 4]
	lidt [edx]
	sti
        ret

load_gdt:
	mov edx, [esp + 4]
	lgdt [edx]		
        ret

	return_esp dd 0
	proc_ip dd 0
	proc_cs dd 0
	proc_stack dd 0
	proc_flags dd 0
	;; сохранение регистров
save_regs:
	;; состояние стека:
	;; [адрес возврата в обрабочик]
	;; [IP процесса]
	;; [CS процесса]
	;; флаги
	;; -> стек процесса
	mov [return_esp], esp ; сохраняем указатель стека
	mov esp, [current_proc]
	add esp, 28 + 64 * 4		; &regs[63] с этого адреса начинаются сохраненные регистры
	pusha				; сохранение регистров EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI
	mov ebx, [return_esp] ; в ebx значение esp
	mov eax, [ebx + 12]	     ;EFLAGS
	push eax		     ; сохраняем флаги
	mov eax, [ebx + 8]	     ;CS
	push eax		     ; сохраняем CS
	mov eax, [ebx + 4]	     ;IP
	mov esp, [current_proc]
	add esp, 24		;current_proc->program_counter + 4
	push eax		; созраняем IP
	mov eax, ebx
	add eax, 16	; указатель стека процесса
	add esp, 8	; esp = current_proc->stack_pointer + 4
	push eax	; сохраняем esp
	mov esp, [return_esp]	; устанавливаем стек ядра (возврат в обработчик прерывания)
	ret

	;; восстановление регистров
restore_regs:
	mov esp, [current_proc]
	add esp, 20		; current_proc->program_counter 
	pop dword [proc_ip]
	pop dword [proc_stack]			; здесь указатель стека current_proc->stack_pointer
	mov esp, [current_proc]
	add esp, 28 + 64 * 4
	sub esp, 10 * 4		; восстанавливаем в обратном порядке
	pop dword [proc_cs]	; CS
	pop dword [proc_flags]	; EFLAGS
	popa			; восстанавливаем все регистры
	mov esp, [proc_stack]
				; установили сохраненное значение стека
	push dword [proc_flags]
	push dword [proc_cs]
	push dword [proc_ip]
	iretd
				; переключаем контекст

disable_interrupts:
	cli
	ret

enable_interrupts:
	sti
	ret

read_port:
	mov edx, [esp + 4]
	in al, dx
	ret

write_port:
	mov   edx, [esp + 4]    
	mov   al, [esp + 4 + 4]  
	out   dx, al  
	ret
	
a_syscall:
	push edx
	push ecx
	push ebx
	push eax
	call sys_call
	add esp, 16
	iret

a_timer:
	call save_regs
	push 0
        call end_of_interrupt
        add esp, 4
	call timer_event
	iret
	
test_syscall:
    mov eax, 0 ; syscall num
    mov ebx, 11 ; param 1
    mov ecx, 12 ; param 2
    mov edx, 13 ; param 3
    int 0x80
	ret

	;; обработчик прерывания
a_interrupt_handler:
	;; сохранить регистры текущего процесса
	;; установить стек ядра
	;; подтверждение контроллеру прерываний
	call end_of_interrupt
	push eax		; номер прерывания
	;; вызов обработчика прерывания
	call interrupt_handler
	add esp, 4
	iret

a_keyboard_interrupt:
	push 0
        call end_of_interrupt
        add esp, 4
	call keyboard_interrupt
	iretd

%macro exception 1
        cli
        push %1
        call exception_handler
	pop eax
        sti
        iretd
%endmacro

a_isrZeroDivisionException: exception 0
a_isrDebugException: exception 1
a_isrNonMaskableInterruptException: exception 2
a_isrBreakpointException: exception 3
a_isrIntoDetectedOverflowException: exception 4
a_isrOutOfBoundsException: exception 5
a_isrInvalidOpcodeException: exception 6
a_isrNoCoprocessorException: exception 7
a_isrDoubleFaultException: exception 8
a_isrCoprocessorSegmentOverrunException: exception 9
a_isrBadTSSException: exception 10
a_isrSegmentNotPresentException: exception 11
a_isrStackFaultException: exception 12
a_isrGeneralProtectionFaultException: exception 13
a_isrPageFaultException: exception 14
a_isrUnknownInterruptException: exception 15
a_isrCoprocessorFaultException: exception 16
a_isrAlignmentCheckException: exception 17
a_isrMachineCheckException: exception 18
a_isrNonExistent: exception 19
	
kernel_code dw 0

section .bss
resb 8192; 8KB for stack
stack_space:
