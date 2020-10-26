/**
 * @file   keyboard.c
 * @author Pavel <pavel@pavel-VirtualBox>
 * @date   Mon Oct 26 09:32:37 2020
 * 
 * @brief Инициализация клавиатуры  
 * 
 * 
 */
#include keyboard.h
/** 
 * проверка подключения клавиатуры
 * 
 */
void init_keyboard() {
  byte data; 
  write_port (0x64, 0xAB);
  read_port (0x60, data);
  if (data==0x00)
    kprint ("keyboard is enabled")
  else
    kprint ("keyboard is not enabled");
}


