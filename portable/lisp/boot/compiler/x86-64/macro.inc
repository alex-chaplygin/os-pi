%define TARGET_x86 ;;<целевая платформа (TARGET_x86 / TARGET_x86_64)>

;; глобальные макросы
%macro ENTRY 0
run:
	ALIGN
%include "init.asm"
%endmacro
	
%define GLOBALS(i) global_mem: times (i * WORD_SIZE) db 0
%define CONSTS(i) const_mem: times (i * WORD_SIZE) db 0

;; платформо-зависимые макросы
%ifdef   TARGET_x86
       USE32
       %define WORD_SIZE 4
       %define ALIGN
       %define UNALIGN
       %define ACC eax
       %define AX eax
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
       %define ACC rax
       %define AX rax
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
%define CONST(i)        mov ACC, [const_mem + i * WORD_SIZE]
%define GLOBAL_REF(i)   mov ACC, [global_mem + i * WORD_SIZE]
%define GLOBAL_SET(i)   mov [global_mem + i * WORD_SIZE], ACC
%define PUSH            push ACC
%define POP             pop ACC
%define JMP(label)      jmp label
%define REG_CALL(label) call label
%define RETURN          ret

%macro JNT 1
   cmp ACC, NIL
   je %1
%endmacro
