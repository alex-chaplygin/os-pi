%define TARGET_x86 ;;<целевая платформа (TARGET_x86 / TARGET_x86_64)>

;; глобальные макросы
%macro ENTRY 0
section .text	
run:
	SAVE_REGS
%include "init.asm"
%endmacro
	
%macro GLOBALS 1
	NUM_GLOBALS equ %1
	section .bss
global_mem: resb (%1 * WORD_SIZE)
%endmacro
	
%macro CONSTS 1
	NUM_CONSTS equ %1
const_mem: resb (%1 * WORD_SIZE)
section .data
%endmacro	

;; платформо-зависимые макросы
%ifdef   TARGET_x86
       USE32
       %define WORD_SIZE 4
       %define MWORD dword
       %define ALIGN
       %define UNALIGN
       %define AX eax
       %define BX ebx
       %define CX ecx
       %define DX edx
       %define DI edi
       %define SI esi
       %define SP esp
       %define BP ebp
       %define REG1 SI
       OBJ_ADDR equ 0xFFFFFFE0
       %macro SAVE_REGS 0
              push BX
              push SI
              push DI
              push BP
       %endmacro
       %macro RESTORE_REGS 0
              pop BP
              pop DI
              pop SI
              pop BX
       %endmacro
   %elifdef TARGET_x86_64
       USE64
       %define WORD_SIZE 8
       %define MWORD qword
       %define ALIGN sub rsp, 8
       %define UNALIGN add rsp, 8
       %define AX rax
       %define BX rbx
       %define CX rcx
       %define DX rdx
       %define DI rdi
       %define SI rsi
       %define SP rsp
       %define BP rbp
       %define REG1 r12
       OBJ_ADDR equ 0xFFFFFFFFFFFFFFE0
       %macro SAVE_REGS 0
              push BX
              push BP
              push r12
              push r13
              push r14
              push r15
              ALIGN
       %endmacro
       %macro RESTORE_REGS 0
              UNALIGN
              pop r15
              pop r14
              pop r13
              pop r12
              pop BP
              pop BX
       %endmacro
   %else
       %error Unknown target
   %endif

;; платформо-независимые макросы инструкций
%define HALT            
%define CONST(i)        mov AX, [const_mem + i * WORD_SIZE]
%define GLOBAL_REF(i)   mov AX, [global_mem + i * WORD_SIZE]
%define GLOBAL_SET(i)   mov [global_mem + i * WORD_SIZE], AX
%define PUSH            push AX
%define POP             pop AX
%define LABEL(l) l:
%define JMP(label)      jmp label
%define REG_CALL(label) call label
%define RETURN          ret

%macro JNT 1
   cmp AX, NULLOBJ
   je %1
%endmacro

%include "prim.asm"
%include "func.asm"
%include "arith.asm"
