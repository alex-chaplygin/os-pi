/**
 * @file   main.c
 * @author alex <alex@alex-Inspiron-N5040>
 * @date   Mon Oct 19 13:44:56 2020
 * 
 * @brief  Главный модуль ядра
 * 
 * 
 */

#include "console.h"
#include "idt.h"
#include "libc.h"
#include "timer.h"
#include "irq.h"
#include "keyboard.h"
#include "mouse.h"

extern void test_syscall();

/** 
 * Точка входа в ядро
 * 
 */
void kmain(void)
{
  idtInit();
  init_interrupts(0x20, 0x28);

  init_timer(10);
  console_clear();
  //  init_keyboard();
  // int a = 1 / 0;
  //kprint("qwerty\n");
  //kprint("ASDFGHJK\n");
  //kprint("Azxcvbn\n");

  //  for (int i = 0; i < 60; i++){
  //  kprint(intToStr(i));
  //  kprint("\n");
  // }
  
  //kprint(int_to_str_hex(255));

  //test_syscall();
  

  if (mouse_check() == 0xAA) {
    kprint("Mouse detected\n");
  } else {
    kprint("No mouse\n");
  }
    kprint("Result a=%i and b=%i",15,46);
   while(1);
 }
