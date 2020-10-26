/**
 * @file   syscall.c
 * @author yri066 <yri066@ubuntu>
 * @date   Mon Oct 26 09:06:06 2020
 * 
 * @brief  Таблица системных вызовов
 * 
 * 
 */

#include "console.h"

/// Тип функции для системного вызова
typedef void (* syscall_f)(int, int, int);

void sys_call1(int p1, int p2, int p3)
{
  kprint("sys_call1");
}

void sys_call2(int p1, int p2, int p3)
{
  kprint("sys_call2");
}

void sys_call3(int p1, int p2, int p3)
{
  kprint("sys_call3");
}

void sys_call4(int p1, int p2, int p3)
{
  kprint("sys_call4");
}

/// Таблица адресов системных вызовов
syscall_f sys_call_table[] = {
    sys_call1,
    sys_call2,
    sys_call3,
    sys_call4
};

/** 
 * Функция sys_call(int num, int p1, int p2, int p3) должна по номеру вызвать нужный обработчик из таблицы адресов системных вызовов
 * 
 * @param num Номер системного вызова 
 * @param param1 Параметр 1 
 * @param param2 Параметр 2
 * @param param3 Параметр 3
 */
void sys_call(int num, int param1, int param2, int param3)
{
    kprint("\nSys call ");
    kprint(" ");
    sys_call_table[num](param1, param2, param3);
 } 
