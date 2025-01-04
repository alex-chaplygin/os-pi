/**
 * @file   main.c
 * @author alex <alex@alex-Inspiron-N5040>
 * @date   Mon Oct 19 13:44:56 2020
 * 
 * @brief  Главный модуль ядра
 * 
 * 
 */

#include <objects.h>
#include <x86/x86.h>
#include <lexer.h>
#include <parser.h>
#include <eval.h>
#include <portable/libc.h>
#include <x86/console.h>
#include <x86/gdt.h>
#include <x86/idt.h>
#include <portable/keyboard.h>
#include <portable/syslib.h>
#include <bind.h>

/// Адрес начала секции .lisp
const void *_lisp_start;
/// состояние стека
extern jmp_buf repl_buf;

/// последний прочитанный токен
extern token_t token;
void graph_init();

void init_all();

/** 
 * Загрузка начального кода lisp
 *
 */
void boot_lisp()
{
    boot_load = 1;
    boot_code = (char *)&_lisp_start;
#ifdef REPL
    printf("boot = %x\n", &_lisp_start);
#endif
    if (setjmp(repl_buf) == 0) {
	do {
	    object_t o = parse();
	    if (o == NOVALUE)
		longjmp(repl_buf, 1);
	    //printf("parse: "); PRINT(o);
	    PROTECT1(o);
	    object_t res = eval(o, NULLOBJ, NULLOBJ);
	    UNPROTECT;
	    //printf("res: "); PRINT(res);
#ifdef REPL
	    PRINT(res);
#endif
	} while (token.type != END);
    }
    boot_load = 0;
    reset_buffer();
#ifdef REPL    
    print_gc_stat(1);
#endif
}

/** 
 * Точка входа в ядро
 * 
 */
void kmain(void)
{
    console_clear();
    init_memory(); // init gdt
    init_interrupts();
    init_keyboard();
    init_all();
    init_sys();
    graph_init();
    boot_lisp();
    if (setjmp(repl_buf) == 0) {
	while(1) {
#ifdef REPL	
	    printf("> ");
	    object_t o = parse();
	    if (o == NOVALUE)
		longjmp(repl_buf, 1);
	    //printf("parse: "); PRINT(o);
	    object_t res = eval(o, NULLOBJ, NULLOBJ);
	    PRINT(res);
#endif
	}
    }
    while (1);
}

