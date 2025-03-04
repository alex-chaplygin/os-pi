#include <stdio.h>
#include <string.h>
#include "objects.h"
#include "alloc.h"
#include "symbols.h"
#include "eval.h"
#include "parser.h"
#include "str.h"
/// Счетчик для gensym
int gensym_counter = 0;
char *itoa(int num, char *str, int rad);

// ()

/**
 * Создание символа на основе строки
 * @param arg - аргумент строка
 * @return созданный символ
 */
object_t intern(object_t arg)
{
    if (TYPE(arg) != STRING)
	error("intern: argument is not a string");
    char *str = GET_STRING(arg)->data;
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
object_t string_size(object_t str)
{
    if (TYPE(str) != STRING)
	error("string_size: argument (str) is not a string");
    
    return new_number(GET_STRING(str)->length);
}

/**
 * Получает символ по индексу
 * @param str строка
 * @param index индекс
 * @return объект символа
**/
object_t str_char(object_t str, object_t index)
{
    if (TYPE(str) != STRING)
    	error("str-char: first argument (str) is not a string");
    
    if (TYPE(index) != NUMBER)
	error("str-char: second argument (index) is not a number");
    
    int ind = get_value(index);
  
    if (ind >= GET_STRING(str)->length || ind < 0)
	error("str-char: invalid index");
    
    int c = GET_STRING(str)->data[ind];
    return NEW_CHAR(c);
}

/**
 * Объединение 2-х строк
 * @param list (строка_1 строка_2 ...)
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
 * @param symbol (символ)
 * @return имя символа
 */
object_t symbol_name(object_t symbol)
{
    if (symbol == NULLOBJ)
	return NEW_STRING("NIL");
    
    if (TYPE(symbol) != SYMBOL)
        error("symbol-name: argument (symbol) is not a symbol");
    
    char *str = GET_SYMBOL(symbol)->str;
    return NEW_STRING(str);
}

object_t function(object_t param);
/**
 * Получение функционального объекта из символа
 * Может быть пользовательской функцией или встроенным примитивом
 * @param list (символ)
 * @return функциональный объект
 */
object_t symbol_function(object_t list)
{
    return function(list);
}

/** 
 * Получение подстроки из строки
 *
 * @param str строка 
 * @param start_index начальный_индекс
 * @param end_index конечный_индекс (не включается)
 *
 * @return объект-строка
 */
object_t subseq(object_t str, object_t start_index, object_t end_index)
{
    if (TYPE(str) != STRING)
	error("subseq: first argument (str) is not a string");

    if (TYPE(start_index) != NUMBER)
	error("subseq: second argument (start_index) is not a number");

    if (TYPE(end_index) != NUMBER)
	error("subseq: third argument (end_index) is not a number");

    int start_ind = get_value(start_index);
    int end_ind = get_value(end_index);

    if (start_ind < 0 || end_ind < 0)
    	error("subseq: index can not be negative");
    if (end_ind - start_ind < 0 || GET_STRING(str)->length < start_ind || end_ind > GET_STRING(str)->length)
    	error("subseq: invalid index");
    char *res = alloc_region(end_ind - start_ind + 1);
    int curr_ind = 0;
    for(int i = start_ind; i < end_ind; i++)
    	res[curr_ind++] = GET_STRING(str)->data[i];
    res[curr_ind] = 0;
    return NEW_STRING(res);
}

/**
 * Перевод целочисленного числа в строку
 * @param number (число)
 * @return строка из числа
 */
object_t int_to_str(object_t number)
{
    if (TYPE(number) != NUMBER)
	error("inttostr: argument (number) is not a number");
    
    char str[MAX_ITOA_STR];
    char *s = itoa(get_value(number), str, 10);    
    return NEW_STRING(s);
}

/**
 * Создаёт символ по коду
 * @param code (код)
 * @return символ по коду
**/
object_t code_char(object_t code)
{
    if (!IS_NUMBER(code))
    	error("code-char: argument (code) is not a number");
    int res = get_value(code);
    return NEW_CHAR(res);
}

/**
 * Получает код символа
 * @param char_obj (символ)
 * @return код символа - число
**/
object_t char_code(object_t char_obj)
{
    if (TYPE(char_obj) != CHAR)
    	error("char-code: argument (char_obj) is not a char");
    return new_number(GET_CHAR(char_obj));
}

/** 
 * Создание строки заднной длины с заполнение символом:
 *
 * @param size размер
 * @param char_obj символ для заполнения
 *
 * @return созданная строка
 */
object_t make_string(object_t size, object_t char_obj)
{
    if (!IS_NUMBER(size))
	error("make-string: first argument (size) is not a number");
    if (TYPE(char_obj) != CHAR)
	error("make-string: second argument (char_obj) is not a char");
    int count = get_value(size);
    char c = GET_CHAR(char_obj);
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
 * @param str строка
 * @param index индекс
 * @param char_obj символ
 *
 * @return символ
 */
object_t sets(object_t str, object_t index, object_t char_obj)
{
    if (TYPE(str) != STRING)
	error("sets: first argument (str)  is not a string");
    string_t *s = GET_STRING(str);
    if (!IS_NUMBER(index))
	error("sets: second argument (index) is not a number");
    int idx = get_value(index);
    if (idx < 0 || idx >= s->length)
	error("sets: index out if bounds");
    if (TYPE(char_obj) != CHAR)
	error("sets: third argument (char_obj) is not a char");
    s->data[idx] = GET_CHAR(char_obj);
    return char_obj;
}

/** 
 * Печать объекта
 *
 * @param obj (объект)
 *
 * @return nil 
 */
object_t print_object(object_t obj)
{
     if (obj == NULLOBJ)
	error("PRINT: invalid params\n");
     PRINT(obj);
     return NULLOBJ;
}

/** 
 * Печать символа 
 *
 * @param char_obj (символ)
 *
 * @return nil
 */
object_t PUTCHAR(object_t char_obj)
{
    if (TYPE(char_obj) != CHAR) 
	error("PUTCHAR: argument (char_obj)  is not a char");
    putchar(GET_CHAR(char_obj));
    return NULLOBJ;
}

/** 
 * Генерация уникального символа
 *
 * @param args нет
 *
 * @return новый уникальный символ
 */
object_t gensym()
{
    char str[MAX_ITOA_STR];
    char *s = itoa(++gensym_counter, str + 1, 10);    
    *--s = 'G';
    return NEW_SYMBOL(s);
}

void init_strings()
{
    register_func("INTERN", intern, 0, 1);
    register_func("CONCAT" ,concat, 1, 0);
    register_func("SYMBOL-NAME", symbol_name, 0, 1);
    register_func("SYMBOL-FUNCTION", symbol_function, 0, 1);
    register_func("STRING-SIZE", string_size, 0, 1);
    register_func("CHAR", str_char, 0, 1);
    register_func("SUBSEQ", subseq, 0, 3);
    register_func("MAKE-STRING", make_string, 0, 2);
    register_func("SETS", sets, 0, 3);
    register_func("INTTOSTR", int_to_str, 0, 1);
    register_func("CODE-CHAR",code_char, 0, 1);
    register_func("CHAR-CODE",char_code, 0, 1);
    register_func("PRINT", print_object, 1, 0);
    register_func("PUTCHAR", PUTCHAR, 0, 1);
    register_func("GENSYM", gensym, 0, 0);
}
