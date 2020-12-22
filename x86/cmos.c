/**
 * @file   cmos.c
 * @author Pavel <pavel@pavel-VirtualBox>
 * @date   Sun Dec 13 22:13:16 2020
 * 
 * @brief  Модуль для работы с cmos
 * 
 * 
 */

#include <portable/types.h>
#include <x86/cmos.h>
#include <x86/x86.h>
#include <portable/libc.h>

/** 
 * @brief Фунция чтения регистра cmos
 * 
 * @param num_reg Номер регистра cmos
 * 
 * @return Значние регистра
 */
byte cmos_read(int num_reg)
{
  write_port(0x70, num_reg);
  return (byte)read_port(0x71);
}

/** 
 * @brief Функция записи в регистр cmos
 * 
 * @param num_reg Номер регистра cmos
 * @param val Значение для записи
 */
void cmos_write(int num_reg, byte val)
{
  write_port(0x70, num_reg);
  write_port(0x71, (int)val);
}

extern int printPtr;
extern int print_page;
extern int current_page;

/** 
 * @brief Печатает текущее время в формате чч:мм:сс
 * 
 */
void print_time()
{
  disable_interrupts();
    int ptr = printPtr;
    int pp = print_page;
    int cp = current_page;
    printPtr = 120;
    print_page = 0;
    current_page = 0;
    byte seconds = cmos_read(CMOS_SECONDS);
    byte minutes = cmos_read(CMOS_MINUTES);
    byte hours = cmos_read(CMOS_HOURS);
    if(!(cmos_read(0x0b) & 0x04))
      {
	seconds = (seconds & 0x0f) + ((seconds / 16) * 10);
	minutes = (minutes & 0x0f) + ((minutes / 16) * 10);
	hours = ((hours & 0x0f) + (((hours & 0x70) / 16) * 10)) | (hours & 0x80);
      }
    if(!(cmos_read(0x0b) & 0x02) && (hours & 0x80))
      hours = ((hours & 0x7f) + 12) % 24;
    if((int)hours < 10)
      if((int)minutes < 10)
	if((int)seconds < 10)
	  kprint("0%d:0%d:0%d", (int)hours, (int)minutes, (int)seconds);
	else
	  kprint("0%d:0%d:%d", (int)hours, (int)minutes, (int)seconds);
      else
        if((int)seconds < 10)
	  kprint("0%d:%d:0%d", (int)hours, (int)minutes, (int)seconds);
	else
	  kprint("0%d:%d:%d", (int)hours, (int)minutes, (int)seconds);
    else
      if((int)minutes < 10)
	if((int)seconds < 10)
	  kprint("%d:0%d:0%d", (int)hours, (int)minutes, (int)seconds);
	else
	  kprint("%d:0%d:%d", (int)hours, (int)minutes, (int)seconds);
      else
        if((int)seconds < 10)
	  kprint("%d:%d:0%d", (int)hours, (int)minutes, (int)seconds);
	else
	  kprint("%d:%d:%d", (int)hours, (int)minutes, (int)seconds);
    printPtr = ptr;
    print_page = pp;
    current_page = cp;
    enable_interrupts();
}

/** 
 * @brief Получает размер памяти из cmos
 * 
 * 
 * @return Размер памяти
 */
uint memory_size()
{
  uint total;
  byte lowmem, highmem;

  write_port(0x70, CMOS_LOWMEM);
  lowmem = read_port(0x71);
  write_port(0x70, CMOS_HIGHMEM);
  highmem = read_port(0x71);

  total = lowmem | highmem << 8;
  return total;
}
