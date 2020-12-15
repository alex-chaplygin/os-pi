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

extern void test_syscall();

/** 
 * Точка входа в ядро
 * 
 */
void kmain(void)
{
   init_memory();
   init_timer(10);
   initProcesses();
  console_clear();
   init_interrupts();
   init_keyboard();
  //kprint(intToStr(get_phys_mem_size()));
  // int a = 1 / 0;
 
  for(int i = 16; i<=22;i++)
  {
   test_mem(i);
  }
  
  //test_syscall();
  

  if (mouse_check() == 0xAA) {
    kprint("Mouse detected\n");
  } else {
    kprint("No mouse\n");
  } 
  kprint("Result a=%i and x=%d and y=%s and str=%x",19,7,"Result str",26);
  kprint("\n %x", str_hex_to_int("1ff"));
  kprint("\n %s", int_to_str_hex(228));
  //kprint("\n %d", (int)cmos_read(0x07));
  // запуск процесса init
  while(1); // процесс ядра
 }
