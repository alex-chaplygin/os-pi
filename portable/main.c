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
#include <x86/timer.h>
#include <x86/irq.h>
#include <x86/keyboard.h>
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
  init_interrupts(0x20, 0x28);
  idtInit();
  
  init_timer(10);
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
  
  kprint(int_to_str_hex(255));
  kprint("\n");
  
  //test_syscall();
  

  if (mouse_check() == 0xAA) {
    kprint("Mouse detected\n");
  } else {
    kprint("No mouse\n");
  }
    kprint("Result a=%i and b=%i",15,46);
   while(1);
 }
