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
#include <x86/disk.h>
#include <portable/file.h>
#include <x86/ide.h>

extern void test_syscall();

/** 
 * Точка входа в ядро
 * 
 */
void kmain(void)
{
  console_clear();
  init_memory();
  init_devices();
  init_timer(10);
  initProcesses();
  init_interrupts();
  init_keyboard();
  init_files();
  // запуск процесса init
  //kprint("mem = %d\n", memory_size());
  while(1) {
    print_time();
  }
	    // процесс ядра
}
