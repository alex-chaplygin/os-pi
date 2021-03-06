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
#include <portable/libc.h>

/// Тип функции для системного вызова
typedef int (* syscall_f)(int, int, int);
typedef int (* syscall0_f)(int);
typedef int (* syscall1_f)(int, int);
typedef int (* syscall2_f)(int, void*, int);
typedef int (* syscall3_f)(void);

/// Структура таблицы адресов системных вызовов
struct syscall {
  int param_num;		/**< кол-во параметров функции системного вызова */
  syscall_f func;		/**< функция системного вызова */
};

/// Таблица адресов системных вызовов
struct syscall sys_call_table[] = {
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
 * 
 * @return значение вызванной функции из таблицы адресов системных вызовов
 */
int sys_call(int num, int param1, int param2, int param3)
{
    if(num < 0 || num > sizeof(sys_call_table) / sizeof(struct syscall))
      	return ERROR_INVALID_PARAMETERS;
    else
      {
	if(sys_call_table[num].param_num == 0)
	  return ((syscall3_f)sys_call_table[num].func)();
	if(sys_call_table[num].param_num == 1)
	  return ((syscall0_f)sys_call_table[num].func)(param1);
	if(sys_call_table[num].param_num == 2)
	  return ((syscall1_f)sys_call_table[num].func)(param1, param2);
	if(sys_call_table[num].param_num == 3)
	  return ((syscall2_f)sys_call_table[num].func)(param1, (void*)param2, param3);
      }
 } 
