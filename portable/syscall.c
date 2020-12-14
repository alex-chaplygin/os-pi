/**
 * @file   syscall.c
 * @author yri066 <yri066@ubuntu>
 * @date   Mon Oct 26 09:06:06 2020
 * 
 * @brief  Таблица системных вызовов
 * 
 * 
 */

#include <portable/syscall.h>
#include <portable/console.h>

/// Тип функции для системного вызова
typedef int (* syscall_f)(int, int, int);
typedef int (* syscall0_f)(char*);
typedef int (* syscall1_f)(int);
typedef int (* syscall2_f)(int, int);
typedef int (* syscall3_f)(int, struct file_info);
typedef int (* syscall4_f)(int, void*, int);
typedef int (* syscall5_f)(void);

/// Структура таблицы адресов системных вызовов
struct sys_calls {
  int param_num;
  syscall_f func;
};

/// Таблица адресов системных вызовов
struct sys_calls sys_call_table[] = {
	 1, (syscall_f)open,
	 1, (syscall_f)create,
	 1, (syscall_f)close,
	 2, (syscall_f)seek,
	 2, (syscall_f)fstat,
	 2, (syscall_f)set_attr,
	 3, (syscall_f)read,
	 3, (syscall_f)write,
	 0, (syscall_f)fork,
	 1, (syscall_f)exec,
	 1, (syscall_f)exit,
	 1, (syscall_f)wait
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
    if(num == 0)
      (syscall0_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 1)
      (syscall0_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 2)
      (syscall1_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 3)
      (syscall2_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 4)
      (syscall3_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 5)
      (syscall2_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 6)
      (syscall4_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 7)
      (syscall4_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 8)
      (syscall5_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 9)
      (syscall0_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 10)
      (syscall1_f)sys_call_table[num].func(param1, param2, param3);
    if(num == 11)
      (syscall1_f)sys_call_table[num].func(param1, param2, param3);
 } 
