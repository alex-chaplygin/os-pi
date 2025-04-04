#include <x86/console.h>
#include <portable/libc.h>
#include "../../portable/lisp/objects.h"
#include "../../portable/lisp/eval.h"

char *table[] = {
"Divide-by-zero Error 	0 (0x0) 	Fault 	#DE 	No",
"Debug 	1 (0x1) 	Fault/Trap 	#DB 	No",
"Non-maskable Interrupt 	2 (0x2) 	Interrupt 	- 	No",
"Breakpoint 	3 (0x3) 	Trap 	#BP 	No",
"Overflow 	4 (0x4) 	Trap 	#OF 	No",
"Bound Range Exceeded 	5 (0x5) 	Fault 	#BR 	No",
"Invalid Opcode 	6 (0x6) 	Fault 	#UD 	No",
"Device Not Available 	7 (0x7) 	Fault 	#NM 	No",
"Double Fault 	8 (0x8) 	Abort 	#DF 	Yes (Zero)",
"Coprocessor Segment Overrun 	9 (0x9) 	Fault 	- 	No",
"Invalid TSS 	10 (0xA) 	Fault 	#TS 	Yes",
"Segment Not Present 	11 (0xB) 	Fault 	#NP 	Yes",
"Stack-Segment Fault 	12 (0xC) 	Fault 	#SS 	Yes",
"General Protection Fault 	13 (0xD) 	Fault 	#GP 	Yes",
"Page Fault 	14 (0xE) 	Fault 	#PF 	Yes",
"Reserved 	15 (0xF) 	- 	- 	No",
"x87 Floating-Point Exception 	16 (0x10) 	Fault 	#MF 	No",
"Alignment Check 	17 (0x11) 	Fault 	#AC 	Yes",
"Machine Check 	18 (0x12) 	Abort 	#MC 	No",
"SIMD Floating-Point Exception 	19 (0x13) 	Fault 	#XM/#XF 	No",
"Virtualization Exception 	20 (0x14) 	Fault 	#VE 	No",
"Reserved 	21-29 (0x15-0x1D) 	- 	- 	No",
"Security Exception 	30 (0x1E) 	- 	#SX 	Yes",
"Reserved 	31 (0x1F) 	- 	- 	No",
"Triple Fault 	- 	- 	- 	No",
"FPU Error Interrupt",
};

/// Всего аппаратных прерываний
#define MAX_IRQ 16
/// Базовый вектор аппаратных прерываний
#define IRQ_BASE 0x20
/// Таблица Lisp обработчиков прерываний
object_t int_handlers[MAX_IRQ];

/**
 * @brief Вызывается при панике ядра
 * 
 */
void panic(){
  printf("\nPanic\n");
  while(1){
    
  }
}

/**
 * @brief Обработчик исключения
 * 
 * @param num Номер исключения
 */
void exception_handler(int num)
{
    printf(table[num]);
    panic();
}

/** 
 * Обработчик прерываний
 * 
 * @param num Номер прерывания
 */
void interrupt_handler(int num)
{
    extern object_t current_env;
    object_t env;
    object_t h = int_handlers[num - IRQ_BASE];
    if (h) {
	env = current_env;
	eval(new_pair(h, NULLOBJ), env, NULLOBJ);
	current_env = env;
    }
}

/** 
 * Установить Lisp обработчик прерываний
 * 
 * @param num Номер прерывания
 * @param func lambda функция без параметров
 */
void set_int_handler(int num, object_t func)
{
    int_handlers[num] = func;
}
