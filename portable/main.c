/**
 * @file   main.c
 * @author alex <alex@alex-Inspiron-N5040>
 * @date   Mon Oct 19 13:44:56 2020
 * 
 * @brief  Главный модуль ядра
 * 
 * 
 */

#include <x86/x86.h>
#include <lexer.h>
#include <objects.h>
#include <parser.h>
#include <eval.h>
#include <portable/libc.h>
#include <x86/console.h>
#include <x86/gdt.h>
#include <x86/idt.h>
#include <portable/keyboard.h>
#include <portable/syslib.h>

/// Адрес начала секции .lisp
const void *_lisp_start;
/// состояние стека
extern jmp_buf repl_buf;

/// последний прочитанный токен
extern token_t token;
void graph_init();

void init_all();

/** 
 * Вывод сообщения об ошибке и выход из программы
 *
 * @param str сообщение об ошибке
 */
void error(char *str, ...)
{
    if (str[0] != '\0') {
        va_list vals;
        va_start(vals, str);
        vprintf(str, vals);
        va_end(vals);
        putchar('\n');
    }
    longjmp(repl_buf, 1);
}

/** 
 * Загрузка начального кода lisp
 *
 */
void boot_lisp()
{
    boot_load = 1;
    boot_code = (char *)&_lisp_start;
    printf("boot = %x\n", &_lisp_start);
    if (setjmp(repl_buf) == 0) {
	do {
	    object_t o = parse();
	    if (o == NOVALUE)
		longjmp(repl_buf, 1);
	    //printf("parse: "); PRINT(o);
	    object_t res = eval(o, NULLOBJ, NULLOBJ);
	    //printf("res: "); PRINT(res);
	    PRINT(res);
	} while (token.type != END);
    }
    boot_load = 0;
    reset_buffer();
    print_gc_stat(1);
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
    while(1) {
	if (setjmp(repl_buf) == 0) {
	    printf("> ");
	    object_t o = parse();
	    if (o == NOVALUE)
		longjmp(repl_buf, 1);
	    //printf("parse: "); PRINT(o);
	    object_t res = eval(o, NULLOBJ, NULLOBJ);
	    PRINT(res);
	}
    }
}

