%define TARGET_x86 ;;<целевая платформа (TARGET_x86 / TARGET_x86_64)>

;; глобальные макросы
%macro ENTRY 0
section .text	
run:
	ALIGN
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
       ARRAY_ADDR equ 0xFFFFFFE0

       %macro ALLOC 1
           push %1
           call alloc
           add SP, 4
       %endmacro

   %elifdef TARGET_x86_64
       USE64
       %define WORD_SIZE 8
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
       ARRAY_ADDR equ 0xFFFFFFFFFFFFFFE0

       %macro ALLOC 1
           mov rdi, %1
           call alloc
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
%define JMP(label)      jmp label
%define REG_CALL(label) call label
%define RETURN          ret

%macro JNT 1
   cmp AX, NULLOBJ
   je %1
%endmacro

%include "prim.asm"
	
