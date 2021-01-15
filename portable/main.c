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

void syscallReadTest(void)
{
  int data[512];
  int fileId = open("file.txt");
  int i = 0;
  while(sys_call(6, fileId, &data[i], 1) > 0)
    {
      kprint("data[%i] = %x, ", i, data[i]);
      i++;
    }
  kprint("open file %i\n", fileId);
  kprint("Test #1: expected = -4, obtained = %i,\n", sys_call(6, fileId, data, -1));
  kprint("Test #2: expected = -4, obtained = %i,\n", sys_call(6, fileId, data, 513));
  kprint("Test #3: expected = -4, obtained = %i,\n", sys_call(6, -1, data, 1));
  kprint("Test #4: expected = -4, obtained = %i,\n", sys_call(6, 330, data, 1));
  kprint("Test #5: expected = -4, obtained = %i,\n", sys_call(6, fileId, 0, 1));
  //kprint("Test #6: expected = 512, obtained = %i,\n", sys_call(6, fileId, data, 512));
  kprint("Test #7: expected = 0, obtained = %i.\n", sys_call(6, fileId, data, 1));
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
  syscallReadTest();
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
