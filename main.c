/**
 * @file   main.c
 * @author alex <alex@alex-Inspiron-N5040>
 * @date   Mon Oct 19 13:44:56 2020
 * 
 * @brief  OS main start code
 * 
 * 
 */

#include "console.h"
#include "idt.h"
#include "libc.h"
#include "timer.h"
#include "irq.h"
#include "keyboard.h"

extern void test_syscall();

/** 
 * Kernel entry point
 * 
 */
void kmain(void)
{
  init_interrupts(0x20, 0x28);
  
  //  idtInit();
  //init_timer(1000);
  console_clear();
  init_keyboard();
  // int a = 1 / 0;
  //kprint("qwerty\n");
  //kprint("ASDFGHJK\n");
  //kprint("Azxcvbn\n");

  /*for (int i = 0; i < 60; i++){
    kprint(intToStr(i));
    kprint("\n");
    }*/
  
  //kprint(int_to_str_hex(255));

  //test_syscall();
  while(1);
}
