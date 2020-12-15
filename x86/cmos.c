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
    kprint("%d:%d:%d", (int)cmos_read(CMOS_HOURS), (int)cmos_read(CMOS_MINUTES), (int)cmos_read(CMOS_SECONDS));
    printPtr = ptr;
    print_page = pp;
    current_page = cp;
    enable_interrupts();
}
