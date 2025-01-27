#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "parser.h"

/**
 * Создание пустого массива заданной длины
 * 
 * @param size <размер>
 * 
 * @return объект массива
*/
object_t make_array(object_t size)
{
    if (TYPE(size) != NUMBER && TYPE(size) != BIGNUMBER)
	error("make_array: not numder");
    array_t *new_arr = new_empty_array(get_value(size));
    object_t new_obj = NEW_OBJECT(ARRAY, new_arr);
    return new_obj;
}

/**
 * Присвоение значения элементу массива
 * 
 * @param list <объект массив> index_obj <индекс> value <объект значение>
 * 
 * @return объект изменённого массива
*/
object_t seta(object_t  arr_o, object_t index_obj, object_t value)
{
    if (TYPE(arr_o) != ARRAY)
	error("seta: not array");
    if (TYPE(index_obj) != NUMBER && TYPE(index_obj) != BIGNUMBER)
	error("seta: index not numder");
    int index = get_value(index_obj);
    if (index >= GET_ARRAY(arr_o)->length || index < 0)
	error("seta: index out of range");
    GET_ARRAY(arr_o)->data[index] = value;
    return arr_o;
}

/**
 * Чтение элемента массива
 * 
 * @param list <массив> index_obj <индекс>
 * 
 * @return объект по указанному индексу массива
*/
object_t aref(object_t  arr_o, object_t index_obj)
{
    if (TYPE(arr_o) != ARRAY)
	error("aref: not an array");
     if (TYPE(index_obj) != NUMBER && TYPE(index_obj) != BIGNUMBER)
	error("seta: index not numder");
    int index = get_value(index_obj);
    if (index >= GET_ARRAY(arr_o)->length || index < 0)
	error("aref: index out of range");
    return GET_ARRAY(arr_o)->data[index];  
}

/**
 * Размер массива
 * 
 * @param list <массив>
 * 
 * @return число элементов
*/
object_t array_size(object_t arr_o)
{
    if (TYPE(arr_o) != ARRAY)
        error("array_size: first element is not an array");
    
    return new_number(GET_ARRAY(arr_o)->length);   
}


void init_arrays()
{
    register_func("MAKE-ARRAY", make_array, 0, 1);
    register_func("ARRAY-SIZE", array_size, 0, 1);
    register_func("SETA", seta, 0, 3);
    register_func("AREF", aref, 0, 2);
}
