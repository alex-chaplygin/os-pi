#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "parser.h"

/**
 * Создание пустого массива заданной длины
 * 
 * @param list <размер>
 * 
 * @return объект массива
*/
object_t make_array(object_t list)
{
    array_t *new_arr = new_empty_array(get_value(FIRST(list)));
    object_t new_obj = NEW_OBJECT(ARRAY, new_arr);
    return new_obj;
}

/**
 * Присвоение значения элементу массива
 * 
 * @param list <объект массив> <индекс> <объект значение>
 * 
 * @return объект изменённого массива
*/
object_t seta(object_t list)
{
    if (list == NULLOBJ || TAIL(list) == NULLOBJ || TAIL(TAIL(list)) == NULLOBJ) {
	error("seta: invalid arguments");
	return ERROR;
    }
    if (TAIL(TAIL(TAIL(list))) != NULLOBJ)
    {
        error("seta: many args");
	    return ERROR;
    }
    object_t arr_o = FIRST(list);
    if (TYPE(arr_o) != ARRAY) {
	error("seta: not array");
	return ERROR;
    }	
    int index = get_value(SECOND(list));
    if (index >= GET_ARRAY(arr_o)->length || index < 0) {
	error("seta: index out of range");
	return ERROR;
    }
    object_t obj = THIRD(list);
    GET_ARRAY(arr_o)->data[index] = obj;
    return arr_o;
}

/**
 * Чтение элемента массива
 * 
 * @param list <массив> <индекс>
 * 
 * @return объект по указанному индексу массива
*/
object_t aref(object_t list)
{
    if (list == NULLOBJ || TAIL(list) == NULLOBJ) {
        error("aref: invalid arguments");
        return ERROR;
    }
    object_t arr_o = FIRST(list);
    if (TYPE(arr_o) != ARRAY) {
	error("aref: not an array");
	return ERROR;
    }
    object_t index_obj = SECOND(list);
    if (TYPE(index_obj) != NUMBER && TYPE(index_obj) != BIGNUMBER) {
        error("aref: index should be a number");
        return ERROR;
    }    
    int index = get_value(index_obj);
    if (index >= GET_ARRAY(arr_o)->length || index < 0) {
	error("aref: index out of range");
	return ERROR;
    }
    return GET_ARRAY(arr_o)->data[index];
}

void init_arrays()
{
    register_func("MAKE-ARRAY", make_array);
    register_func("SETA", seta);
    register_func("AREF", aref);
}
