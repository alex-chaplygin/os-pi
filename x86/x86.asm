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
extern kmain, exception_handler, sys_call, timer_event,end_of_interrupt		;this is defined in the c file
extern interrupt_handler
extern current_proc	

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

	;; сохранение регистров
save_regs:
	mov [current_proc + 20], esp ; сохраняем указатель стека процесса regs[0]
	mov esp, current_proc
	add esp, 24 + 63 * 4		; &regs[63] с этого адреса начинаются сохраненные регистры
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	mov ebx, [current_proc + 20] ; в ebx значение esp
	mov eax, [ebx + 4]	     ;CS
	push eax
	mov eax, [ebx + 8]	     ;IP
	push eax
	mov esp, stack_space	; устанавливаем стек ядра
	ret

	;; восстановление регистров
restore_regs:
	mov ebx, [esp + 4]
	mov eax, [ebx + 0]
	mov ecx, [ebx + 4]
	mov edx, [ebx + 8]
	mov ebp, [ebx + 12]
	mov esi, [ebx + 16]
	mov edi, [ebx + 20]
	push eax
	mov ax, [ebx + 24]
	mov ds, ax
	mov ax, [ebx + 26]
	mov es, ax
	mov eax, [ebx + 28]
	pop eax
	push edx
	mov edx, ebx
	mov ebx, [edx + 32]
	pop edx
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
	call timer_event
	push 0
        call end_of_interrupt
        add esp, 4
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
	push eax		; номер прерывания
	;; вызов обработчика прерывания
	call interrupt_handler
	add esp, 4
	;; подтверждение контроллеру прерываний
	call end_of_interrupt
	iret

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
