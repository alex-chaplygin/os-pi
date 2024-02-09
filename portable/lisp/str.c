#include <stdio.h>
#include <string.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "parser.h"

char *itoa(int num, char *str, int rad);

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
    if (TAIL(list) != NULL) {
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
 * Возвращает длину строки
 * @param list (строка)
 * @return длина строки
**/
object_t *string_size(object_t *list)
{
    if (list == NULL) {
	error("string-size: no arguments\n");
	return ERROR;
    }
    if (TAIL(list) != NULL) {
	error("string-size: too many arguments\n");
	return ERROR;
    }
    if (is_params_string(list) == 0) {
	error("string-size: not string in params\n");
	return ERROR;
    }
    return object_new(NUMBER, &(FIRST(list)->u.str->length)); 
}

/**
 * Получает символ по индексу
 * @param list (строка индекс)
 * @return код символа - число
**/
object_t *str_char(object_t *list)
{
    if (list == NULL) {
	error("str-char: no arguments\n");
	return ERROR;
    }
    if (TAIL(list) == NULL) {
	error("str-char: not all arguments\n");
	return ERROR;
    }
    if (SECOND(list)->type != NUMBER) {
    	error("str-char: not number in params\n");
    	return ERROR;
    }
    if (TAIL(TAIL(list)) != NULL) {
	error("str-size: too many arguments\n");
	return ERROR;
    }
    
    object_t* str = FIRST(list);
    int ind = SECOND(list)->u.value;

    if (str->type != STRING) {
    	error("str-char: not string in params\n");
    	return ERROR;
    }
    if (ind >= str->u.str->length || ind < 0) {
	error("str-char: invalid index\n");
	return ERROR;
    }
    int c = str->u.str->data[ind];
    return object_new(NUMBER, &c);
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

/** 
 * Получение подстроки из строки
 *
 * @param list (строка начальный_индекс конечный_индекс(не включается))
 *
 * @return объект-строка
 */
object_t *subseq(object_t *list)
{
    if (list == NULL) {
        error("subseq: no arguments\n");
        return ERROR;
    }
    if (TAIL(list) == NULL || TAIL(TAIL(list)) == NULL) {
	error("subseq: not all arguments\n");
	return ERROR;
    }
    if (TAIL(TAIL(TAIL(list))) != NULL) {
	error("subseq: too many arguments\n");
	return ERROR;
    }
    if (FIRST(list)->type != STRING || SECOND(list)->type != NUMBER || THIRD(list)->type != NUMBER) {
        error("subseq: invalid args\n");
        return ERROR;
    }
    object_t *string = FIRST(list);
    int start_ind = SECOND(list)->u.value;
    int end_ind = THIRD(list)->u.value;

    if(start_ind < 0 || end_ind < 0){
	error("subseq: index can not be negative\n");
	return ERROR;
    }
    if(end_ind - start_ind < 0 || string->u.str->length <= start_ind || end_ind > string->u.str->length ){
	error("subseq: invalid index\n");
        return ERROR;
    }
    char *res = alloc_region(end_ind - start_ind + 1);
    int curr_ind = 0;    
    for(int i = start_ind; i < end_ind; i++)
	res[curr_ind++] = string->u.str->data[i];
    res[curr_ind] = 0;
    return object_new(STRING, res);
}

/**
 * Перевод целочисленного числа в строку
 * @param list (число)
 * @return строка из числа
 */
object_t *int_to_str(object_t *list)
{
    if (list == NULL) {
	error("inttostr: no args\n");
	return ERROR;
    }
    if (TAIL(list) != NULL) {
	error("inttostr: many args\n");
	return ERROR;
    }
    if (FIRST(list)->type != NUMBER) {
	error("inttostr: invalid arg\n");
	return ERROR;
    }    
    char str[14];
    char *s = itoa(FIRST(list)->u.value, str, 10);    
    return object_new(STRING, s);
}

/**
 * Создаёт символ-строку по коду
 * @param list (индекс)
 * @return код символа - число
**/
object_t *code_char(object_t *list)
{
    if (list == NULL) {
	error("code-char: no arguments\n");
	return ERROR;
    }
    if (TAIL(list) != NULL) {
	error("code-char: many arguments\n");
	return ERROR;
    }
    if (FIRST(list)->type != NUMBER) {
    	error("code-char: not number in params\n");
    	return ERROR;
    }
    int code = FIRST(list)->u.value;
    char* str = alloc_region(2);
    *str = code;
    *(str + 1) = 0;
    return object_new(STRING, str);
}

void init_strings()
{
    register_func("INTERN", intern);
    register_func("CONCAT" ,concat);
    register_func("SYMBOL-NAME", symbol_name);
    register_func("STRING-SIZE", string_size);
    register_func("CHAR", str_char);
    register_func("SUBSEQ", subseq);
    register_func("INTTOSTR", int_to_str);
    register_func("CODE-CHAR",code_char);
}
