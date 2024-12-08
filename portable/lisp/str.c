#include <stdio.h>
#include <string.h>
#include "objects.h"
#include "alloc.h"
#include "symbols.h"
#include "eval.h"
#include "parser.h"
#include "str.h"

char *itoa(int num, char *str, int rad);

// ()

/**
 * Создание символа на основе строки
 * @param list (строка)
 * @return созданный символ
 */
object_t intern(object_t list)
{
    if (list == NULLOBJ)
        error("intern: no arguments");
    if (FIRST(list) == NULLOBJ || TYPE(FIRST(list)) != STRING)
        error("intern: not string in params");
    if (TAIL(list) != NULLOBJ)
        error("intern: too many parameters");
    char *str = GET_STRING(FIRST(list))->data;
    symbol_t *sym = find_symbol(str);
    if (sym == NULL)
	error("intern: empty string");
    return NEW_OBJECT(SYMBOL, sym);
}

/** 
 * Проверка списка на то, что все элементы типа STRING
 *
 * @param list - список
 *
 * @return 1 - все типа STRING, 0 - нет
 */
int is_params_string(object_t list)
{
    if (list == NULLOBJ)
	return 1;
    if (TYPE(FIRST(list)) != STRING)
        return 0;
    return is_params_string(TAIL(list));
}

/**
 * Возвращает длину строки
 * @param list (строка)
 * @return длина строки
**/
object_t string_size(object_t list)
{
    if (list == NULLOBJ)
	error("string-size: no arguments");
    if (TAIL(list) != NULLOBJ)
	error("string-size: too many arguments");
    if (is_params_string(list) == 0)
	error("string-size: not string in params");
    return new_number(GET_STRING(FIRST(list))->length);
}

/**
 * Получает символ по индексу
 * @param list (строка индекс)
 * @return объект символа
**/
object_t str_char(object_t list)
{
    if (list == NULLOBJ)
	error("str-char: no arguments");
    if (TAIL(list) == NULLOBJ)
	error("str-char: not all arguments");
    if (TYPE(SECOND(list)) != NUMBER)
    	error("str-char: not number in params");
    if (TAIL(TAIL(list)) != NULLOBJ)
	error("str-size: too many arguments");
    object_t str = FIRST(list);
    int ind = get_value(SECOND(list));
    if (TYPE(str) != STRING)
    	error("str-char: not string in params");
    if (ind >= GET_STRING(str)->length || ind < 0)
	error("str-char: invalid index");
    int c = GET_STRING(str)->data[ind];
    return NEW_CHAR(c);
}

/**
 * Объединение 2-х строк
 * @param list (строка_1 строка_2)
 * @return объединившая строка
 */
object_t concat(object_t list)
{
    if (list == NULLOBJ)
        error("concat: no arguments");
    if (FIRST(list) == NULLOBJ || is_params_string(list) != 1)
        error("concat: not string in params");
    object_t temp_o = list;
    int len = GET_STRING(FIRST(temp_o))->length;
    while ((temp_o = TAIL(temp_o)) != NULLOBJ)
        len += GET_STRING(FIRST(temp_o))->length;
    char *res = alloc_region(len + 1);
    strcpy(res, GET_STRING(FIRST(list))->data);
    temp_o = list;
    while ((temp_o = TAIL(temp_o)) != NULLOBJ)
        strcat(res, GET_STRING(FIRST(temp_o))->data);
    object_t new_o = NEW_STRING(res);
    free_region(res);
    return new_o;
}

/**
 * Получение имени символа
 * @param list (символ)
 * @return имя символа
 */
object_t symbol_name(object_t list)
{
    if (list == NULLOBJ)
        error("symbol-name: no arguments");
    if (FIRST(list) == NULLOBJ)
        return NEW_STRING("NIL");
    if (TYPE(FIRST(list)) != SYMBOL)
        error("symbol-name: not symbol in params");
    if (TAIL(list) != NULLOBJ)
        error("symbol-name: too many parameters");
    char *str = GET_SYMBOL(FIRST(list))->str;
    return NEW_STRING(str);
}

/** 
 * Получение подстроки из строки
 *
 * @param list (строка начальный_индекс конечный_индекс(не включается))
 *
 * @return объект-строка
 */
object_t subseq(object_t list)
{
    if (list == NULLOBJ)
        error("subseq: no arguments");
    if (TAIL(list) == NULLOBJ || TAIL(TAIL(list)) == NULLOBJ)
	error("subseq: not all arguments");
    if (TAIL(TAIL(TAIL(list))) != NULLOBJ)
	error("subseq: too many arguments");
    if (TYPE(FIRST(list)) != STRING || TYPE(SECOND(list)) != NUMBER || TYPE(THIRD(list)) != NUMBER)
        error("subseq: invalid args");
    object_t string = FIRST(list);
    int start_ind = get_value(SECOND(list));
    int end_ind = get_value(THIRD(list));

    if(start_ind < 0 || end_ind < 0)
	error("subseq: index can not be negative");
    if(end_ind - start_ind < 0 || GET_STRING(string)->length <= start_ind || end_ind > GET_STRING(string)->length)
	error("subseq: invalid index");
    char *res = alloc_region(end_ind - start_ind + 1);
    int curr_ind = 0;
    for(int i = start_ind; i < end_ind; i++)
	res[curr_ind++] = GET_STRING(string)->data[i];
    res[curr_ind] = 0;
    return NEW_STRING(res);
}

/**
 * Перевод целочисленного числа в строку
 * @param list (число)
 * @return строка из числа
 */
object_t int_to_str(object_t list)
{
    if (list == NULLOBJ)
	error("inttostr: no args");
    if (TAIL(list) != NULLOBJ)
	error("inttostr: many args");
    if (TYPE(FIRST(list)) != NUMBER)
	error("inttostr: invalid arg");
    char str[MAX_ITOA_STR];
    char *s = itoa(get_value(FIRST(list)), str, 10);    
    return NEW_STRING(s);
}

/** 
 * Определение длины списка
 *
 * @param args список
 *
 * @return длина
 */
int list_length(object_t args)
{
    int c = 0;
    while (args != NULLOBJ) {
	args = TAIL(args);
	c++;
    }
    return c;
}

/**
 * Создаёт символ по коду
 * @param list (индекс)
 * @return код символа - число
**/
object_t code_char(object_t list)
{
    if (list_length(list) != 1)
	error("code-char: invalid arguments number");
    object_t o = FIRST(list);
    if (!IS_NUMBER(o))
    	error("code-char: not number in params");
    int code = get_value(o);
    return NEW_CHAR(code);
}

/**
 * Получает код символа
 * @param list (символ)
 * @return код символа - число
**/
object_t char_code(object_t list)
{
    if (list_length(list) != 1)
	error("char-code: invalid arguments number");
    object_t o = FIRST(list);
    if (TYPE(o) != CHAR)
    	error("char-code: not char");
    return new_number(GET_CHAR(o));
}

/** 
 * Создание строки заднной длины с заполнение символом:
 *
 * @param args (размер символ)
 *
 * @return созданная строка
 */
object_t make_string(object_t args)
{
    if (list_length(args) != 2)
	error("make-string: invalid arguments count");
    object_t co = FIRST(args);
    object_t ch = SECOND(args);
    if (!IS_NUMBER(co))
	error("make-string: invalid size");
    if (TYPE(ch) != CHAR)
	error("make-string: invalid char");
    int count = get_value(co);
    char c = GET_CHAR(ch);
    char *buf = alloc_region(count + 1);
    for (int i = 0; i < count; i++)
	buf[i] = c;
    object_t str = NEW_STRING(buf);
    free_region(buf);
    return str;
}

/** 
 * Замена символа в строке
 *
 * @param args (строка индекс символ)
 *
 * @return символ
 */
object_t sets(object_t args)
{
    if (list_length(args) != 3)
	error("sets: invalid arguments count");
    object_t a1 = FIRST(args);
    if (TYPE(a1) != STRING)
	error("sets: not string");
    string_t *s = GET_STRING(a1);
    object_t a2 = SECOND(args);
    if (!IS_NUMBER(a2))
	error("sets: not number in index");
    int idx = get_value(a2);
    if (idx < 0 || idx >= s->length)
	error("sets: index out if bounds");
    object_t ch = THIRD(args);
    if (TYPE(ch) != CHAR)
	error("sets: not char");
    s->data[idx] = GET_CHAR(ch);
    return ch;
}

/** 
 * Печать объекта
 *
 * @param args <объект>
 *
 * @return nil 
 */
object_t print_object(object_t args)
{
     if (args == NULLOBJ)
	error("PRINT: invalid params\n");
     PRINT(FIRST(args));
     return NULLOBJ;
}

/** 
 * Печать символа 
 *
 * @param args (объект символ)
 *
 * @return nil
 */
object_t PUTCHAR(object_t args)
{
    if (args == NULLOBJ || TAIL(args) != NULLOBJ)
	error("PUTCHAR: char");
    object_t c = FIRST(args);
    if (TYPE(c) != CHAR) 
	error("PUTCHAR: not a char");
    putchar(GET_CHAR(c));
    return NULLOBJ;
}

void init_strings()
{
    register_func("INTERN", intern);
    register_func("CONCAT" ,concat);
    register_func("SYMBOL-NAME", symbol_name);
    register_func("STRING-SIZE", string_size);
    register_func("CHAR", str_char);
    register_func("SUBSEQ", subseq);
    register_func("MAKE-STRING", make_string);
    register_func("SETS", sets);
    register_func("INTTOSTR", int_to_str);
    register_func("CODE-CHAR",code_char);
    register_func("CHAR-CODE",char_code);
    register_func("PRINT", print_object);
    register_func("PUTCHAR", PUTCHAR);
}
