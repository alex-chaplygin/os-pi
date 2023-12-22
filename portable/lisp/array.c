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
object_t *make_array(object_t *list)
{
    array_t *new_arr = new_empty_array(FIRST(list)->u.value);
    object_t *new_obj = object_new(ARRAY, new_arr);
    return new_obj;
}

/**
 * Присвоение значения элементу массива
 * 
 * @param list <объект массив> <индекс> <объект значение>
 * 
 * @return объект изменённого массива
*/
object_t *seta(object_t *list)
{
    if (list == NULL || TAIL(list) == NULL || TAIL(TAIL(list)) == NULL) {
	error("seta: invalid arguments");
	return ERROR;
    }
    if (TAIL(TAIL(TAIL(list))) != NULL)
    {
        error("seta: many args");
	    return ERROR;
    }
    object_t *arr_o = FIRST(list);
    if (arr_o->type != ARRAY){
	error("seta: not array");
	return ERROR;
    }	
    int index = SECOND(list)->u.value;
    if (index >= arr_o->u.arr->length || index < 0) {
	error("seta: index out of range");
	return ERROR;
    }
    object_t *obj = THIRD(list);
    arr_o->u.arr->data[index] = obj;
    return arr_o;
}

/**
 * Чтение элемента массива
 * 
 * @param list <массив> <индекс>
 * 
 * @return объект по указанному индексу массива
*/
object_t *aref(object_t *list)
{
    if (list == NULL || TAIL(list) == NULL) {
        error("aref: invalid arguments");
        return ERROR;
    }
    object_t *arr_o = FIRST(list);
    if (arr_o->type != ARRAY) {
	error("aref: not an array");
	return ERROR;
    }
    object_t *index_obj = SECOND(list);
    if (index_obj->type != NUMBER) {
        error("aref: index should be a number");
        return ERROR;
    }    
    int index = index_obj->u.value;
    if (index >= arr_o->u.arr->length || index < 0) {
	error("aref: index out of range");
	return ERROR;
    }
    return arr_o->u.arr->data[index];
}

void init_arrays()
{
    register_func("MAKE-ARRAY", make_array);
    register_func("SETA", seta);
    register_func("AREF", aref);
}
