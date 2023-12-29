/**
 * @file   sys.c
 * @author alex <alex@alex-home>
 * @date   Sun Oct  8 11:43:59 2023
 * 
 * @brief  Встроенные Lisp функции от операционной системы
 * 
 */

#include <x86/x86.h>
#include <x86/console.h>
#include <portable/libc.h>
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
 * Чтение массива слов из порта
 *
 * @param args (<номер порта> <число слов>)
 *
 * @return объект массив
 */
object_t *INSW(object_t *args)
{
    if (args == NULL || args->type != PAIR) {
	error("INSW: port\n");
	return ERROR;
    }
    int port = FIRST(args)->u.value;
    int size = SECOND(args)->u.value;
    byte *buf = alloc_region(size * sizeof(short));
    inw_arr(port, size, buf);
    size <<= 1;
    array_t *a = new_empty_array(size);
    byte *b = buf;
    int val;
    for (int i = 0; i < size; i++) {
	val = *b++;
	a->data[i] = object_new(NUMBER, &val);
    }
    free_region(buf);
    return object_new(ARRAY, a);
}

/** 
 * Запись массива байт (словами) в порт
 *
 * @param args (номер_порта массив)
 *
 * @return nil
 */
object_t *OUTSW(object_t *args)
{
    if (args == NULL || args->type != PAIR || args->u.pair->right == NULL) {
	error("OUTSW: port val\n");
	return ERROR;
    }
    int port = FIRST(args)->u.value;
    array_t *a = SECOND(args)->u.arr;
    byte *buf = alloc_region(a->length);
    byte *dst = buf;
    object_t **src = a->data;
    for (int i = 0; i < a->length; i++) {
	*dst++ = (*src)->u.value;
	src++;
    }
    outw_arr(port, a->length >> 1, buf);
    return NULL;
}

/** 
 * Печать символа 
 *
 * @param args (строка, содержащая один символ)
 *
 * @return nil
 */
object_t *PUTCHAR(object_t *args)
{
    if (args == NULL || TAIL(args) != NULL) {
	error("PUTCHAR: char\n");
	return ERROR;
    }
    object_t *str = FIRST(args);
    if (str->type != STRING && str->type != NUMBER) {
	error("PUTCHAR: not string and no number\n");
	return ERROR;
    }
    if (str->type == STRING)
	putchar(*str->u.str->data);
    else
	putchar(str->u.value);
    return NULL;
}

/** 
 * Установка позиции курсора 
 *
 * @param args (две координаты x, y)
 *
 * @return nil
 */
object_t *SET_CURSOR(object_t *args)
{
    if (args == NULL || TAIL(args) == NULL || TAIL(TAIL(args)) != NULL) {
	error("SET-CURSOR: x y\n");
	return ERROR;
    }
    object_t *x = FIRST(args);
    object_t *y = SECOND(args);
    if (x->type != NUMBER || y->type != NUMBER) {
	error("SET-CURSOR: not number\n");
	return ERROR;
    }
    set_cursor(x->u.value, y->u.value);
    return NULL;
}

/** 
 * Установка цвета символа 
 *
 * @param args цвет символа
 *
 * @return nil
 */
object_t *SET_COLOR(object_t *args)
{
    if (args == NULL || TAIL(args) != NULL) {
	error("SET-COLOR: char\n");
	return ERROR;
    }
    object_t *col = FIRST(args);
    if (col->type != NUMBER) {
	error("SET-COLOR: not number\n");
	return ERROR;
    }
    int c = col->u.value;
    if (c < 0 || c > 15) {
	error("SET-COLOR: invalid color\n");
	return ERROR;
    }
    set_color(col->u.value);
    return NULL;
}

/** 
 * Установка цвета фона 
 *
 * @param args цвет фона
 *
 * @return nil
 */
object_t *SET_BACK_COLOR(object_t *args)
{
    if (args == NULL || TAIL(args) != NULL) {
	error("SET_BACK_COLOR: char\n");
	return ERROR;
    }
    object_t *col = FIRST(args);
    if (col->type != NUMBER) {
	error("SET_BACK_COLOR: not number\n");
	return ERROR;
    }
    int c = col->u.value;
    if (c < 0 || c > 15) {
	error("SET_BACK_COLOR: invalid color\n");
	return ERROR;
    }
    set_back_color(col->u.value);
    return NULL;
}

/** 
 * Спрятать курсор 
 *
 * @param args параметров нет
 *
 * @return nil 
 */
object_t *HIDE_CURSOR(object_t *args)
{
     if (args != NULL) {
	error("HIDE_CURSOR: invalid params\n");
	return ERROR;
     }
     disable_cursor();
     return NULL;
}

/** 
 * Показать курсор 
 *
 * @param args параметров нет
 *
 * @return nil 
 */
object_t *SHOW_CURSOR(object_t *args)
{
     if (args != NULL) {
	error("SHOW_CURSOR: invalid params\n");
	return ERROR;
     }
     enable_cursor(15, 0xFF);
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
    register_func("INSW", INSW);
    register_func("OUTSW", OUTSW);
    register_func("PUTCHAR", PUTCHAR);
    register_func("SET-CURSOR", SET_CURSOR);
    register_func("SET-COLOR", SET_COLOR);
    register_func("SET-BACK-COLOR", SET_BACK_COLOR);
    register_func("HIDE-CURSOR", HIDE_CURSOR);
    register_func("SHOW-CURSOR", SHOW_CURSOR);
}
