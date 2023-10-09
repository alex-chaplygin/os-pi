/**
 * @file   sys.c
 * @author alex <alex@alex-home>
 * @date   Sun Oct  8 11:43:59 2023
 * 
 * @brief  Встроенные Lisp функции от операционной системы
 * 
 */

#include <x86/x86.h>
#include <objects.h>
#include <symbols.h>
#include <eval.h>
#include <parser.h>

#define NULL 0

/** 
 * Чтение байта из порта
 *
 * @param args (номер порта)
 *
 * @return значение регистра из порта
 */
object_t *INB(object_t *args)
{
    if (args == NULL || args->type != PAIR) {
	error("INB: port\n");
	return ERROR;
    }
    int val = inb(FIRST(args)->u.value);
    return object_new(NUMBER, &val);
}

/** 
 * Чтение слова из порта
 *
 * @param args (номер порта)
 *
 * @return значение регистра из порта
 */
object_t *INW(object_t *args)
{
    if (args == NULL || args->type != PAIR) {
	error("INW: port\n");
	return ERROR;
    }
    int val = inw(FIRST(args)->u.value);
    return object_new(NUMBER, &val);
}

/** 
 * Запись байта в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t *OUTB(object_t *args)
{
    if (args == NULL || args->type != PAIR || args->u.pair->right == NULL) {
	error("OUTB: port val\n");
	return ERROR;
    }
    outb(FIRST(args)->u.value, SECOND(args)->u.value);
    return NULL;
}

/** 
 * Запись слова в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t *OUTW(object_t *args)
{
    if (args == NULL || args->type != PAIR || args->u.pair->right == NULL) {
	error("OUTW: port val\n");
	return ERROR;
    }
    outw(FIRST(args)->u.value, SECOND(args)->u.value);
    return NULL;
}

/**
 * Чтение двойного слова из порта
 *
 * @param args (номер порта)
 *
 * @return значение регистра из порта
 */
object_t *INDW(object_t *args)
{
    if (args == NULL || args->type != PAIR) {
	error("INDW: port\n");
	return ERROR;
    }
    int val = indw(FIRST(args)->u.value);
    return object_new(NUMBER, &val);
}

/** 
 * Запись двойного слова в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t *OUTDW(object_t *args)
{
    if (args == NULL || args->type != PAIR || args->u.pair->right == NULL) {
	error("OUTDW: port val\n");
	return ERROR;
    }
    outdw(FIRST(args)->u.value, SECOND(args)->u.value);
    return NULL;
}

/** 
 * Регистрация системных функций
 */
void init_sys()
{
    register_func("INB", INB);
    register_func("OUTB", OUTB);
    register_func("INW", INW);
    register_func("OUTW", OUTW);
    register_func("INDW", INDW);
    register_func("OUTDW", OUTDW);
}
