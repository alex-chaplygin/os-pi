/**
 * @file   main.c
 * @author alex <alex@alex-Inspiron-N5040>
 * @date   Mon Oct 19 13:44:56 2020
 * 
 * @brief  Главный модуль ядра
 * 
 * 
 */

#include <x86/console.h>
#include <portable/libc.h>
#include <portable/mem.h>
#include <x86/idt.h>
#include <portable/timer.h>
#include <x86/irq.h>
#include <portable/keyboard.h>
#include <x86/mouse.h>
#include <x86/gdt.h>
#include <portable/proc.h>
#include <x86/cmos.h>
#include <portable/device.h>
#include <portable/file.h>
#include <portable/syslib.h>
#include <x86/disk.h>
#include <objects.h>
#include <lexer.h>
#include <parser.h>
#include <eval.h>
#include <arith.h>
#include <symbols.h>

/// Адрес начала секции .lisp
const void *_lisp_start;

/// последний прочитанный токен
extern token_t token;

/** 
 * Загрузка начального кода lisp
 *
 */
void boot_lisp()
{
    boot_load = 1;
    boot_code = (char *)&_lisp_start;
    printf("boot = %x\n", &_lisp_start);
    do {
	object_t *o = parse();
	if (o != ERROR) {
	    object_t *res = eval(o, NULL);
	    PRINT(res);
        garbage_collect();
	}
    } while (token.type != END);
    boot_load = 0;
    flag = 0;
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
    init_eval();
    init_arith();
    init_regions();
    init_sys();
    graph_init();
    boot_lisp();
    while(1) {
	printf("> ");
	object_t *o = parse();
	//printf("parse: ");
	//print_obj(o);
	if (o != ERROR) {
	    object_t *res = eval(o, NULL);
	    printf("\n");
	    PRINT(res);
	}      
	garbage_collect();
    }
}

