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


void syscallReadTest(void)
{
  int data[512];
  int fileId = open("file.txt");
  int i = 0;
  byte b;
  while(read(fileId, &b, 1) > 0)
    {
      kprint("%x ", b);
      i++;
      if (i == 512) break;
    }
  kprint("\nopen file %i\n", fileId);
  kprint("Test #1: expected = -4, obtained = %i,\n", read(fileId, data, -1));
  kprint("Test #2: expected = -4, obtained = %i,\n", read(fileId, data, 513));
  kprint("Test #3: expected = -4, obtained = %i,\n", read(-1, data, 1));
  kprint("Test #4: expected = -4, obtained = %i,\n", read(330, data, 1));
  kprint("Test #5: expected = -4, obtained = %i,\n", read(fileId, 0, 1));
  //kprint("Test #6: expected = 512, obtained = %i,\n", read(fileId, data, 512));
  kprint("Test #7: expected = 0, obtained = %i.\n", read(fileId, data, 1));
}


void test_set_attr() {
  /*
  Входные условия: id файла меньше 0.
  Правильный класс эквивалентности: ошибка ERROR_INVALID_PARAMETERS.
  Неправильный класс эквивалентности: возвращается 0.

  Входные условия: id файла больше NUM_FILES.
  Правильный класс эквивалентности: ошибка ERROR_INVALID_PARAMETERS.
  Неправильный класс эквивалентности: возвращается 0.

  Входные условия: по id не открыт никакой файл.
  Правильный класс эквивалентности: ошибка ERROR_INVALID_PARAMETERS.
  Неправильный класс эквивалентности: возвращается 0.

  Входные условия: id корректен, файл открыт.
  Правильный класс эквивалентности: возвращается 0, файлу присваиваются новые атрибуты.
  Неправильный класс эквивалентности: ошибка ERROR_INVALID_PARAMETERS.

  Входные условия: id корректен, файл открыт, вызов через sys_call.
  Правильный класс эквивалентности: возвращается 0, файлу присваиваются новые атрибуты.
  Неправильный класс эквивалентности: ошибка ERROR_INVALID_PARAMETERS.
  */
  int descriptor = open("file.txt");
  kprint("open file %i\n", descriptor);
  int res = set_attr(-88, ATTR_REGULAR);
  kprint("Test #1, expected: -4, actual: %d\n", res);

  res = set_attr(322, ATTR_REGULAR);
  kprint("Test #2, expected: -4, actual: %d\n", res);

  res = set_attr(228, ATTR_REGULAR);
  kprint("Test #3, expected: -4, actual: %d\n", res);

  res = set_attr(descriptor, ATTR_DEVICE);
  kprint("Test #4, expected: 0, actual: %d\n", res);
  kprint("Expected attributes: 3, actual attributes: %d\n", get_attr(descriptor));

  byte buffer[BLOCK_SIZE];
  disk_read_block(buffer, 1);

  for (int i = 0; i < FILE_RECORD_SIZE; i++) {
    kprint("0x%x ", buffer[i]);
  }

  kprint("\n");

  res = sys_call(5, descriptor, ATTR_DIRECTORY, 0);
  kprint("Test #5, expected: 0, actual: %d\n", res);
  kprint("Expected attributes: 2, actual attributes: %d\n", get_attr(descriptor));

  close(descriptor);
  descriptor = open("file.txt");
  kprint("After closing and reopening the file: %d\n", get_attr(descriptor));
  close(descriptor);
}

void test_fstat()
{
  struct file_info info;
  int id = open("file.txt");
  //Тестирование работы функции с параметрами соотвествующими спецификации
  kprint("fstat %i\n",fstat(id,&info));
  kprint("%i %d %i \n",info.length,info.attrib,info.device_num);
  //Тестирование работы функции с не существующим идентификатором файла
  kprint("fstat %i\n",fstat(5,&info));
  //Тестирование работы функции с не существующим идентификатором файла
  kprint("fstat %i\n",fstat(-1,&info));
  //Тестирование работы функции с нулевым значением указателя структуры
  kprint("fstat %i\n",fstat(0,0));
  close(id);
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
  init_files();
  
  test_set_attr();
  syscallReadTest();
  test_fstat();
  
  kprint("mem = %d\n", memory_size());
  // запуск процесса init    
  while(1) {
    print_time();
  }	    // процесс ядра
}

