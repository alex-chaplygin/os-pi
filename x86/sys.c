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
object_t INB(object_t args)
{
    if (args == NULLOBJ || TYPE(args) != PAIR)
	error("INB: port\n");
    int val = inb(get_value(FIRST(args)));
    return new_number(val);
}

/** 
 * Чтение слова из порта
 *
 * @param args (номер порта)
 *
 * @return значение регистра из порта
 */
object_t INW(object_t args)
{
    if (args == NULLOBJ || TYPE(args) != PAIR)
	error("INW: port\n");
    int val = inw(get_value(FIRST(args)));
    return new_number(val);
}

/** 
 * Запись байта в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t OUTB(object_t args)
{
    if (args == NULLOBJ || TYPE(args) != PAIR || GET_PAIR(args)->right == NULLOBJ)
	error("OUTB: port val\n");
    outb(get_value(FIRST(args)), get_value(SECOND(args)));
    return NULLOBJ;
}

/** 
 * Запись слова в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t OUTW(object_t args)
{
    if (args == NULLOBJ || TYPE(args) != PAIR || GET_PAIR(args)->right == NULLOBJ)
	error("OUTW: port val\n");
    outw(get_value(FIRST(args)), get_value(SECOND(args)));
    return NULLOBJ;
}

/**
 * Чтение двойного слова из порта
 *
 * @param args (номер порта)
 *
 * @return значение регистра из порта
 */
object_t INDW(object_t args)
{
    if (args == NULLOBJ || TYPE(args) != PAIR)
	error("INDW: port\n");
    int val = indw(get_value(FIRST(args)));
    return new_number(val);
}

/** 
 * Запись двойного слова в порт
 *
 * @param args (номер_порта значение)
 *
 * @return nil
 */
object_t OUTDW(object_t args)
{
    if (args == NULLOBJ || TYPE(args) != PAIR || GET_PAIR(args)->right == NULLOBJ)
	error("OUTDW: port val\n");
    outdw(get_value(FIRST(args)), get_value(SECOND(args)));
    return NULLOBJ;
}

/**
 * Чтение массива слов из порта
 *
 * @param args (<номер порта> <число слов>)
 *
 * @return объект массив
 */
object_t INSW(object_t args)
{
    if (args == NULLOBJ || TYPE(args) != PAIR)
	error("INSW: port\n");
    int port = get_value(FIRST(args));
    int size = get_value(SECOND(args));
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
object_t OUTSW(object_t args)
{
    if (args == NULLOBJ || TYPE(args) != PAIR || GET_PAIR(args)->right == NULLOBJ)
	error("OUTSW: port val\n");
    int port = get_value(FIRST(args));
    array_t *a = GET_ARRAY(SECOND(args));
    byte *buf = alloc_region(a->length);
    byte *dst = buf;
    object_t *src = a->data;
    for (int i = 0; i < a->length; i++)
	*dst++ = get_value(*src++);
    outw_arr(port, a->length >> 1, buf);
    return NULLOBJ;
}

/** 
 * Печать символа 
 *
 * @param args (строка, содержащая один символ)
 *
 * @return nil
 */
object_t PUTCHAR(object_t args)
{
    if (args == NULLOBJ || TAIL(args) != NULLOBJ)
	error("PUTCHAR: char\n");
    object_t str = FIRST(args);
    if (TYPE(str) != STRING && TYPE(str) != NUMBER)
	error("PUTCHAR: not string and no number\n");
    if (TYPE(str) == STRING)
	putchar(*GET_STRING(str)->data);
    else
	putchar(get_value(str));
    return NULLOBJ;
}

/** 
 * Установка позиции курсора 
 *
 * @param args (две координаты x, y)
 *
 * @return nil
 */
object_t SET_CURSOR(object_t args)
{
    if (args == NULLOBJ || TAIL(args) == NULLOBJ || TAIL(TAIL(args)) != NULLOBJ)
	error("SET-CURSOR: x y\n");
    object_t x = FIRST(args);
    object_t y = SECOND(args);
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
object_t SET_COLOR(object_t args)
{
    if (args == NULLOBJ || TAIL(args) != NULLOBJ)
	error("SET-COLOR: char\n");
    object_t col = FIRST(args);
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
object_t SET_BACK_COLOR(object_t args)
{
    if (args == NULLOBJ || TAIL(args) != NULLOBJ)
	error("SET_BACK_COLOR: char\n");
    object_t col = FIRST(args);
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
object_t HIDE_CURSOR(object_t args)
{
     if (args != NULLOBJ)
	error("HIDE_CURSOR: invalid params\n");
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
object_t SHOW_CURSOR(object_t args)
{
     if (args != NULLOBJ)
	error("SHOW_CURSOR: invalid params\n");
     enable_cursor(15, 0xFF);
     return NULLOBJ;
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
