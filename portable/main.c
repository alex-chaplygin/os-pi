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
#include <portable/syscall.h>
#include <x86/disk.h>
#include <objects.h>
#include <parser.h>
#include <eval.h>
#include <arith.h>
#include <symbols.h>

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
    while(1) {
	printf("> ");
	object_t *o = parse();
	//printf("parse: ");
	//print_obj(o);
	if (o != ERROR) {
	    object_t *res = eval(o, NULL);
	    printf("\n");
	    print_obj(res);
	    printf("\n");
	}      
    }
}

