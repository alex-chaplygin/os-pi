#include <stdio.h>
#include <string.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "parser.h"

/**
 * Создание символа на основе строки
 * @param list (строка)
 * @return созданный символ
 */
object_t *intern(object_t *list)
{
    if (list == NULL) {
        error("intern: no arguments\n");
        return ERROR;
    }
    if (FIRST(list) == NULL || FIRST(list)->type != STRING) {
        error("intern: not string in params\n");
        return ERROR;
    }
    if(TAIL(list) != NULL) {
        error("intern: too many parameters\n");
        return ERROR;
    }
    char *str = FIRST(list)->u.str->data;
    symbol_t *sym = new_symbol(str);
    if (sym == NULL) {
	error("intern: empty string\n");
	return ERROR;
    }
    return object_new(SYMBOL, sym);
}

/** 
 * Проверка списка на то, что все элементы типа STRING
 *
 * @param list - список
 *
 * @return 1 - все типа STRING, 0 - нет
 */
int is_params_string(object_t *list)
{
    if (list == NULL)
	return 1;
    if (FIRST(list)->type != STRING)
        return 0;
    return is_params_string(TAIL(list));
}

/**
 * Объединение 2-х строк
 * @param list (строка_1 строка_2)
 * @return объединившая строка
 */
object_t *concat(object_t *list)
{
    if (list == NULL) {
        error("concat: no arguments\n");
        return ERROR;
    }
    if (FIRST(list) == NULL || is_params_string(list) != 1) {
        error("concat: not string in params\n");
        return ERROR;
    }
    object_t *temp_o = list;
    int len = FIRST(temp_o)->u.str->length;
    while ((temp_o = TAIL(temp_o)) != NULL)
        len += FIRST(temp_o)->u.str->length;
    char *res = alloc_region(len + 1);
    strcpy(res, FIRST(list)->u.str->data);
    temp_o = list;
    while ((temp_o = TAIL(temp_o)) != NULL)
        strcat(res, FIRST(temp_o)->u.str->data);
    object_t *new_o = object_new(STRING, res);
    free_region(res);
    return new_o;
}

/**
 * Получение имени символа
 * @param list (символ)
 * @return имя символа
 */
object_t *symbol_name(object_t *list)
{
    if (list == NULL) {
        error("symbol-name: no arguments\n");
        return ERROR;
    }
    if (FIRST(list) == NULL)
	return object_new(STRING, "NIL");
    if (FIRST(list)->type != SYMBOL) {
        error("symbol-name: not symbol in params\n");
        return ERROR;
    }
    if(TAIL(list) != NULL) {
        error("symbol-name: too many parameters\n");
        return ERROR;
    }
    char *str = FIRST(list)->u.symbol->str;
    return object_new(STRING, str);
}

void init_strings()
{
    register_func("INTERN", intern);
    register_func("CONCAT" ,concat);
    register_func("SYMBOL-NAME", symbol_name);
}
