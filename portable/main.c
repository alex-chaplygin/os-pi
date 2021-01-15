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
#include <portable/syscall.h>

extern void test_syscall();

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
  kprint("open file %i\n", open("file.txt"));
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
  int id_file=open("file.txt");
  test_fstat(id_file);
  
  kprint("mem = %d\n", memory_size());
  
  while(1) {
    print_time();
  }
	    // процесс ядра
}

void test_fstat(int id){
  struct file_info info;
  //Тестирование работы функции с параметрами соотвествующими спецификации
  kprint("fstat %i\n",fstat(id,&info));
  kprint("%i %d %i \n",info.length,info.attrib,info.device_num);
  //Тестирование работы функции с не существующим идентификатором файла
  kprint("fstat %i\n",fstat(5,&info));
  //Тестирование работы функции с не существующим идентификатором файла
  kprint("fstat %i\n",fstat(-1,&info));
  //Тестирование работы функции с нулевым значением указателя структуры
  kprint("fstat %i\n",fstat(0,0));
}
