/**
 * @file   sys.c
 * @author alex <alex@alex-home>
 * @date   Sun Oct  8 11:43:59 2023
 * 
 * @brief  Встроенные Lisp функции от операционной системы
 * 
 */

#include <objects.h>
#include <x86/x86.h>
#include <x86/console.h>
#include <portable/libc.h>
#include <alloc.h>
#include <symbols.h>
#include <eval.h>
#include <parser.h>

/** 
 * Чтение байта из порта
 *
 * @param args (номер порта)
 *
 * @return значение регистра из порта
 */
object_t INB(object_t port)
{
    if (TYPE(port) != NUMBER)
	error("INB: port\n");
    int val = inb(get_value(port));
    return new_number(val);
}

/** 
 * Чтение слова из порта
 *
 * @param args (номер порта)
 *
 * @return значение регистра из порта
 */
object_t INW(object_t port)
{
    if (TYPE(port) != NUMBER)
	error("INW: port\n");
    int val = inw(get_value(port));
    return new_number(val);
}

/** 
 * Запись байта в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t OUTB(object_t port, object_t val)
{
    if (TYPE(port) != NUMBER || TYPE(val) != NUMBER)
	error("OUTB: port val\n");
    outb(get_value(port), get_value(val));
    return NULLOBJ;
}

/** 
 * Запись слова в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t OUTW(object_t port, object_t val)
{
    if (TYPE(port) != NUMBER || TYPE(val) != NUMBER)
	error("OUTW: port val\n");
    outw(get_value(port), get_value(val));
    return NULLOBJ;
}

/**
 * Чтение двойного слова из порта
 *
 * @param args (номер порта)
 *
 * @return значение регистра из порта
 */
object_t INDW(object_t port)
{
    if (!IS_NUMBER(TYPE(port)))
	error("INDW: port\n");
    int val = indw(get_value(port));
    return new_number(val);
}

/** 
 * Запись двойного слова в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t OUTDW(object_t port, object_t val)
{
    if (!IS_NUMBER(TYPE(port)) || !IS_NUMBER(TYPE(val)))
	error("OUTDW: port val\n");
    outdw(get_value(port), get_value(val));
    return NULLOBJ;
}

/**
 * Чтение массива слов из порта
 *
 * @param args (<номер порта> <число слов>)
 *
 * @return объект массив
 */
object_t INSW(object_t p, object_t s)
{
    if (TYPE(p) != NUMBER || TYPE(s) != NUMBER)
	error("INSW: port size\n");
    int port = get_value(p);
    int size = get_value(s);
    byte *buf = alloc_region(size * sizeof(short));
    inw_arr(port, size, buf);
    size <<= 1;
    array_t *a = new_empty_array(size);
    byte *b = buf;
    int val;
    for (int i = 0; i < size; i++) {
	val = *b++;
	a->data[i] = new_number(val);
    }
    free_region(buf);
    return NEW_OBJECT(ARRAY, a);
}

/** 
 * Запись массива байт (словами) в порт
 *
 * @param args (номер_порта массив)
 *
 * @return nil
 */
object_t OUTSW(object_t p, object_t arr)
{
    if (TYPE(p) != NUMBER || TYPE(arr) != ARRAY)
	error("OUTSW: port val\n");
    int port = get_value(p);
    array_t *a = GET_ARRAY(arr);
    byte *buf = alloc_region(a->length);
    byte *dst = buf;
    object_t *src = a->data;
    for (int i = 0; i < a->length; i++)
	*dst++ = get_value(*src++);
    outw_arr(port, a->length >> 1, buf);
    free_region(buf);
    return NULLOBJ;
}

/** 
 * Установка позиции курсора 
 *
 * @param args (две координаты x, y)
 *
 * @return nil
 */
object_t SET_CURSOR(object_t x, object_t y)
{
    if (TYPE(x) != NUMBER || TYPE(y) != NUMBER)
	error("SET-CURSOR: not number\n");
    set_cursor(get_value(x), get_value(y));
    return NULLOBJ;
}

/** 
 * Установка цвета символа 
 *
 * @param args цвет символа
 *
 * @return nil
 */
object_t SET_COLOR(object_t col)
{
    if (TYPE(col) != NUMBER)
	error("SET-COLOR: not number\n");
    int c = get_value(col);
    if (c < 0 || c > 15)
	error("SET-COLOR: invalid color\n");
    set_color(c);
    return NULLOBJ;
}

/** 
 * Установка цвета фона 
 *
 * @param args цвет фона
 *
 * @return nil
 */
object_t SET_BACK_COLOR(object_t col)
{
    if (TYPE(col) != NUMBER)
	error("SET_BACK_COLOR: not number\n");
    int c = get_value(col);
    if (c < 0 || c > 15)
	error("SET_BACK_COLOR: invalid color\n");
    set_back_color(c);
    return NULLOBJ;
}

/** 
 * Спрятать курсор 
 *
 * @param args параметров нет
 *
 * @return nil 
 */
object_t HIDE_CURSOR()
{
     disable_cursor();
     return NULLOBJ;
}

/** 
 * Показать курсор 
 *
 * @param args параметров нет
 *
 * @return nil 
 */
object_t SHOW_CURSOR()
{
     enable_cursor(15, 0xFF);
     return NULLOBJ;
}

/** 
 * Установить обработчик аппаратного прерывания
 *
 * @param args <номер irq> <lambda функция обработки>
 *
 * @return nil 
 */
object_t SET_INT_HANDLER(object_t irq, object_t fun)
{
    if (TYPE(irq) != NUMBER || TYPE(fun) != FUNCTION)
	error("SET-INT-HANDLER: invalid params\n");
    set_int_handler(get_value(irq), fun);
    return NULLOBJ;
}

/** 
 * Регистрация системных функций
 */
void init_sys()
{
    register_func("INB", INB, 0, 1);
    register_func("OUTB", OUTB, 0, 2);
    register_func("INW", INW, 0, 1);
    register_func("OUTW", OUTW, 0, 2);
    register_func("INDW", INDW, 0, 1);
    register_func("OUTDW", OUTDW, 0, 2);
    register_func("INSW", INSW, 0, 2);
    register_func("OUTSW", OUTSW, 0, 2);
    register_func("SET-CURSOR", SET_CURSOR, 0, 2);
    register_func("SET-COLOR", SET_COLOR, 0, 1);
    register_func("SET-BACK-COLOR", SET_BACK_COLOR, 0, 1);
    register_func("HIDE-CURSOR", HIDE_CURSOR, 0, 0);
    register_func("SHOW-CURSOR", SHOW_CURSOR, 0, 0);
    register_func("SET-INT-HANDLER", SET_INT_HANDLER, 0, 2);
}
