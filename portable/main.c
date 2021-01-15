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

extern void test_syscall();

void syscallCreateTest(void)
{
  kprint("Test #1: expected = 1, obtained = %i\n", sys_call(1, "abc.txt", 0, 0));
  kprint("Test #2: expected = -4, obtained = %i\n", sys_call(1, 0, 0, 0));
  sys_call(1, "2", 0, 0);
  sys_call(1, "3", 0, 0);
  sys_call(1, "4", 0, 0);
  sys_call(1, "5", 0, 0);
  sys_call(1, "6", 0, 0);
  sys_call(1, "7", 0, 0);
  sys_call(1, "8", 0, 0);
  sys_call(1, "9", 0, 0);
  sys_call(1, "10", 0, 0);
  kprint("Test #3: expected = -2, obtained = %i\n", sys_call(1, "abcd.txt", 0, 0));
}

/** 
 * Точка входа в ядро
 * 
 */
void kmain(void)
{
  init_memory();
  init_timer(10);
  init_devices();
  initProcesses();
  console_clear();
  init_interrupts();
  init_keyboard();
  init_disk();
  init_files();
  kprint("open file file.txt %i\n", open("file.txt"));
  syscallCreateTest();
  //kprint(intToStr(get_phys_mem_size()));
  // int a = 1 / 0;
 
  // for(int i = 16; i<=22;i++)
  // {
  // test_mem(i);
  // }
  
  //test_syscall();
  

  // if (mouse_check() == 0xAA) {
  //kprint("Mouse detected\n");
  //} else {
  // kprint("No mouse\n");
  // } 
  // запуск процесса init
  kprint("mem = %d\n", memory_size());
  while(1) {
    print_time();
  }
	    // процесс ядра
}
