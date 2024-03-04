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
    if (FIRST(list) == NULL || TYPE(FIRST(list)) != STRING) {
        error("intern: not string in params\n");
        return ERROR;
    }
    if (TAIL(list) != NULL) {
        error("intern: too many parameters\n");
        return ERROR;
    }
    char *str = GET_STRING(FIRST(list))->data;
    symbol_t *sym = new_symbol(str);
    if (sym == NULL) {
	error("intern: empty string\n");
	return ERROR;
    }
    return NEW_OBJECT(SYMBOL, sym);
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
    if (TYPE(FIRST(list)) != STRING)
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
    return new_number(GET_STRING(FIRST(list))->length);
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
    if (TYPE(SECOND(list)) != NUMBER) {
    	error("str-char: not number in params\n");
    	return ERROR;
    }
    if (TAIL(TAIL(list)) != NULL) {
	error("str-size: too many arguments\n");
	return ERROR;
    }
    
    object_t* str = FIRST(list);
    int ind = get_value(SECOND(list));

    if (TYPE(str) != STRING) {
    	error("str-char: not string in params\n");
    	return ERROR;
    }
    if (ind >= GET_STRING(str)->length || ind < 0) {
	error("str-char: invalid index\n");
	return ERROR;
    }
    int c = GET_STRING(str)->data[ind];
    return new_number(c);
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
    int len = GET_STRING(FIRST(temp_o))->length;
    while ((temp_o = TAIL(temp_o)) != NULL)
        len += GET_STRING(FIRST(temp_o))->length;
    char *res = alloc_region(len + 1);
    strcpy(res, GET_STRING(FIRST(list))->data);
    temp_o = list;
    while ((temp_o = TAIL(temp_o)) != NULL)
        strcat(res, GET_STRING(FIRST(temp_o))->data);
    object_t *new_o = NEW_OBJECT(STRING, new_string(res));
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
    return NEW_OBJECT(STRING, new_string("NIL"));
    if (TYPE(FIRST(list)) != SYMBOL) {
        error("symbol-name: not symbol in params\n");
        return ERROR;
    }
    if(TAIL(list) != NULL) {
        error("symbol-name: too many parameters\n");
        return ERROR;
    }
    char *str = GET_SYMBOL(FIRST(list))->str;
    return NEW_OBJECT(STRING, new_string(str));
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
    if (TYPE(FIRST(list)) != STRING || TYPE(SECOND(list)) != NUMBER || TYPE(THIRD(list)) != NUMBER) {
        error("subseq: invalid args\n");
        return ERROR;
    }
    object_t *string = FIRST(list);
    int start_ind = get_value(SECOND(list));
    int end_ind = get_value(THIRD(list));

    if(start_ind < 0 || end_ind < 0){
	error("subseq: index can not be negative\n");
	return ERROR;
    }
    if(end_ind - start_ind < 0 || GET_STRING(string)->length <= start_ind || end_ind > GET_STRING(string)->length ){
	error("subseq: invalid index\n");
        return ERROR;
    }
    char *res = alloc_region(end_ind - start_ind + 1);
    int curr_ind = 0;    
    for(int i = start_ind; i < end_ind; i++)
	res[curr_ind++] = GET_STRING(string)->data[i];
    res[curr_ind] = 0;
    return NEW_OBJECT(STRING, new_string(res));
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
    if (TYPE(FIRST(list)) != NUMBER) {
	error("inttostr: invalid arg\n");
	return ERROR;
    }    
    char str[14];
    char *s = itoa(get_value(FIRST(list)), str, 10);    
    return NEW_OBJECT(STRING, new_string(s));
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
    if (TYPE(FIRST(list)) != NUMBER) {
    	error("code-char: not number in params\n");
    	return ERROR;
    }
    int code = get_value(FIRST(list));
    char* str = alloc_region(2);
    *str = code;
    *(str + 1) = 0;
    return NEW_OBJECT(STRING, new_string(str));
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
