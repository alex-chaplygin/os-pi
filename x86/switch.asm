; платформо-зависимые макросы
%ifidn __OUTPUT_FORMAT__, elf32
    %define TARGET_x86
%elifidn __OUTPUT_FORMAT__, elf64
    %define TARGET_x86_64
%endif

%define _AX 0
%define _BX 1
%define _CX 2
%define _DX 3
%define _SI 4
%define _DI 5
%define _BP 6
%define _SP 7

%ifdef TARGET_x86
       USE32
       %define WORD_SIZE 4
       %define MWORD dword
       %define _IP 8
       %define _FLAGS 9
       JMPBUF equ 11			; смещение поля regs в словах (44 байта / 4)
%elifdef TARGET_x86_64
       USE64
       %define WORD_SIZE 8
       %define MWORD qword
       %define _R8 8
       %define _R9 9
       %define _R10 10
       %define _R11 11
       %define _R12 12
       %define _R13 13
       %define _R14 14
       %define _R15 15
       %define _IP 16
       %define _FLAGS 17
       JMPBUF equ 10			; смещение поля regs в словах (80 байт / 8)
%else
       %error Unknown target
%endif

section .text

global save_regs
global restore_regs

;; int save_regs(thread_t *from)
;; Сохраняет регистры процессора в from->regs.
;; В regs[_AX] сохраняет 1 (значение RAX при последующем возобновлении потока).
;; При прямом вызове возвращает 0.
save_regs:
%ifdef TARGET_x86
	mov EAX, [ESP + WORD_SIZE]       ; EAX = from (первый аргумент)
	add EAX, WORD_SIZE * JMPBUF      ; EAX = &from->regs[0]
	mov [EAX + WORD_SIZE * _BX], EBX
	mov [EAX + WORD_SIZE * _CX], ECX
	mov [EAX + WORD_SIZE * _DX], EDX
	mov [EAX + WORD_SIZE * _SI], ESI
	mov [EAX + WORD_SIZE * _DI], EDI
	mov [EAX + WORD_SIZE * _BP], EBP
	lea ECX, [ESP + WORD_SIZE]       ; ESP после ret (вызывающий стек)
	mov [EAX + WORD_SIZE * _SP], ECX
	mov ECX, [ESP]                   ; адрес возврата
	mov [EAX + WORD_SIZE * _IP], ECX
	mov DWORD [EAX + WORD_SIZE * _AX], 1 ; при возобновлении EAX станет 1
	pushfd
	pop DWORD [EAX + WORD_SIZE * _FLAGS]
	xor EAX, EAX                     ; возвращаем 0 при прямом вызове
	ret
%elifdef TARGET_x86_64
	mov RAX, RDI                     ; RAX = from (1-й аргумент в RDI)
	add RAX, WORD_SIZE * JMPBUF      ; RAX = &from->regs[0]
	mov [RAX + WORD_SIZE * _BX], RBX
	mov [RAX + WORD_SIZE * _CX], RCX
	mov [RAX + WORD_SIZE * _DX], RDX
	mov [RAX + WORD_SIZE * _SI], RSI
	mov [RAX + WORD_SIZE * _DI], RDI
	mov [RAX + WORD_SIZE * _BP], RBP
	mov [RAX + WORD_SIZE * _R8], R8
	mov [RAX + WORD_SIZE * _R9], R9
	mov [RAX + WORD_SIZE * _R10], R10
	mov [RAX + WORD_SIZE * _R11], R11
	mov [RAX + WORD_SIZE * _R12], R12
	mov [RAX + WORD_SIZE * _R13], R13
	mov [RAX + WORD_SIZE * _R14], R14
	mov [RAX + WORD_SIZE * _R15], R15
	lea RCX, [RSP + WORD_SIZE]       ; RSP после ret (вызывающий стек)
	mov [RAX + WORD_SIZE * _SP], RCX
	mov RCX, [RSP]                   ; адрес возврата
	mov [RAX + WORD_SIZE * _IP], RCX
	mov QWORD [RAX + WORD_SIZE * _AX], 1 ; при возобновлении RAX станет 1
	pushfq
	pop QWORD [RAX + WORD_SIZE * _FLAGS]
	xor EAX, EAX                     ; возвращаем 0 при прямом вызове
	ret
%endif

;; void restore_regs(thread_t *to)
;; Восстанавливает регистры процессора из to->regs и передает управление в to->regs[_IP].
;; Никогда не возвращает управление в точку вызова.
restore_regs:
%ifdef TARGET_x86
	mov EDX, [ESP + WORD_SIZE]       ; EDX = to (первый аргумент)
	add EDX, WORD_SIZE * JMPBUF      ; EDX = &to->regs[0]

	mov ESP, [EDX + WORD_SIZE * _SP]

	push DWORD [EDX + WORD_SIZE * _FLAGS]
	popfd

	mov EBX, [EDX + WORD_SIZE * _BX]
	mov EBP, [EDX + WORD_SIZE * _BP]
	mov ESI, [EDX + WORD_SIZE * _SI]
	mov EDI, [EDX + WORD_SIZE * _DI]
	mov ECX, [EDX + WORD_SIZE * _IP]
	mov EAX, [EDX + WORD_SIZE * _AX]
	mov EDX, [EDX + WORD_SIZE * _DX] ; EDX восстанавливаем последним
	jmp ECX
%elifdef TARGET_x86_64
	mov RDX, RDI                     ; RDX = to (1-й аргумент в RDI)
	add RDX, WORD_SIZE * JMPBUF      ; RDX = &to->regs[0]

	mov RSP, [RDX + WORD_SIZE * _SP]

	push QWORD [RDX + WORD_SIZE * _FLAGS]
	popfq

	mov R8, [RDX + WORD_SIZE * _R8]
	mov R9, [RDX + WORD_SIZE * _R9]
	mov R10, [RDX + WORD_SIZE * _R10]
	mov R11, [RDX + WORD_SIZE * _R11]
	mov R12, [RDX + WORD_SIZE * _R12]
	mov R13, [RDX + WORD_SIZE * _R13]
	mov R14, [RDX + WORD_SIZE * _R14]
	mov R15, [RDX + WORD_SIZE * _R15]
	mov RBX, [RDX + WORD_SIZE * _BX]
	mov RBP, [RDX + WORD_SIZE * _BP]
	mov RCX, [RDX + WORD_SIZE * _IP]
	mov RDI, [RDX + WORD_SIZE * _DI]
	mov RSI, [RDX + WORD_SIZE * _SI]
	mov RAX, [RDX + WORD_SIZE * _AX]
	mov RDX, [RDX + WORD_SIZE * _DX] ; RDX восстанавливаем последним
	jmp RCX
%endif

global kill_current_thread
extern thread_finish
;; kill_current_thread()
;; Вызывается при выходе из eval через команду ret
kill_current_thread:
%ifdef TARGET_x86
	push EAX              ; результат eval (в EAX) как аргумент thread_finish
%elifdef TARGET_x86_64
	mov RDI, RAX          ; результат eval (в RAX) передается в RDI
%endif
	call thread_finish
.halt:
	hlt
	jmp .halt
