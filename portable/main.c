/**
 * @file   main.c
 * @author alex <alex@alex-Inspiron-N5040>
 * @date   Mon Oct 19 13:44:56 2020
 * 
 * @brief  Главный модуль ядра
 * 
 * 
 */

#include <portable/console.h>
#include <portable/libc.h>
#include <portable/mem.h>
#include <x86/idt.h>
#include <portable/timer.h>
#include <x86/irq.h>
#include <portable/keyboard.h>
#include <x86/mouse.h>
#include <x86/gdt.h>

extern void test_syscall();

/** 
 * Точка входа в ядро
 * 
 */
void kmain(void)
{
  init_memory();
  // init_interrupts();
  // init_timer(10);
  console_clear();
  //  init_keyboard();
  //kprint(intToStr(get_phys_mem_size()));
  // int a = 1 / 0;
  //kprint("qwerty\n");
  //kprint("ASDFGHJK\n");
  //kprint("Azxcvbn\n");

  //  for (int i = 0; i < 60; i++){
  //  kprint(intToStr(i));
  //  kprint("\n");
  // }
  
  //for(int i =1; i<10;i++)
  //test_mem(i);
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
  kprint("\n %x", str_to_hex("1ff"));
   while(1);
 }
