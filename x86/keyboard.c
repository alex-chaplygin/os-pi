/**
 * @file   keyboard.c
 * @author Pavel <pavel@pavel-VirtualBox>
 * @date   Mon Oct 26 09:32:37 2020
 * 
 * @brief Инициализация клавиатуры  
 * 
 * 
 */
#include <x86/ports.h>
#include <portable/console.h>
#include <portable/types.h>
/** 
 * проверка подключения клавиатуры
 * 
 */
void init_keyboard() {
  uchar data; 
  write_port(0x64, 0xAA);
  data = read_port(0x60);
  if (data == 0x55)
    kprint("Кeyboard is enabled\n");
  else
    kprint("No keyboard\n");
}


