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
