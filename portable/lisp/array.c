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
    if (TYPE(arr_o) != ARRAY && TYPE(arr_o) != BYTE_ARRAY)
	error("seta: not array");
    if (TYPE(index_obj) != NUMBER && TYPE(index_obj) != BIGNUMBER)
	error("seta: index not numder");
    int index = get_value(index_obj);
    if (index >= GET_ARRAY(arr_o)->length || index < 0)
	error("seta: index out of range");
    if (TYPE(arr_o) == BYTE_ARRAY) {
	if (!IS_NUMBER(value))
	    error("seta: byte array value not number");
	((unsigned char *)GET_ARRAY(arr_o)->data)[index] = get_value(value);
    } else
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
    if (TYPE(arr_o) != ARRAY && TYPE(arr_o) != BYTE_ARRAY)
	error("aref: not an array");
     if (TYPE(index_obj) != NUMBER && TYPE(index_obj) != BIGNUMBER)
	error("aref: index not numder");
    int index = get_value(index_obj);
    if (index >= GET_ARRAY(arr_o)->length || index < 0)
	error("aref: index out of range");
    if (TYPE(arr_o) == BYTE_ARRAY)
	return new_number(((unsigned char *)GET_ARRAY(arr_o)->data)[index]);
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
    if (TYPE(arr_o) != ARRAY && TYPE(arr_o) != BYTE_ARRAY)
        error("array_size: first element is not an array");
    
    return new_number(GET_ARRAY(arr_o)->length);   
}

/**
 * Создание массива байт заданной длины (заполнен нулями)
 *
 * @param size <размер в байтах>
 *
 * @return объект массива байт
*/
object_t alloc_bytes(object_t size)
{
    if (!IS_NUMBER(size))
	error("alloc: not number");
    array_t *new_arr = new_byte_array(get_value(size));
    return NEW_OBJECT(BYTE_ARRAY, new_arr);
}

/**
 * Адрес данных массива байт
 *
 * @param arr_o <массив байт>
 *
 * @return адрес в памяти (число)
*/
object_t array_addr(object_t arr_o)
{
    if (TYPE(arr_o) != BYTE_ARRAY)
	error("array-addr: not a byte array");
    return new_bignumber((object_t)GET_ARRAY(arr_o)->data);
}

void init_arrays()
{
    register_func("MAKE-ARRAY", make_array, 0, 1);
    register_func("ARRAY-SIZE", array_size, 0, 1);
    register_func("SETA", seta, 0, 3);
    register_func("AREF", aref, 0, 2);
    register_func("ALLOC", alloc_bytes, 0, 1);
    register_func("ARRAY-ADDR", array_addr, 0, 1);
}
