#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "array.h"

/**
 * Создание пустого массива заданной длины
 * 
 * @param list <имя массива> <размер>
 * 
 * @return объект имя массива
*/
object_t *make_array(object_t *list)
{
    array_t *new_arr = new_empty_array(SECOND(list)->u.value);
    object_t *new_obj = object_new(ARRAY, new_arr);
    object_t *new_sym = object_new(SYMBOL, FIRST(list)->u.symbol->str);
    new_sym->u.symbol->value = new_obj;
    // return new_obj;
    return SECOND(list);
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
    object_t *arr_o = FIRST(list);
    int index = SECOND(list)->u.value;
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
    object_t *arr_o = FIRST(list);
    int index = SECOND(list)->u.value;
    return arr_o->u.arr->data[index];
}

void init_arrays()
{
    register_func("MAKE-ARRAY", make_array);
    register_func("SETA", seta);
    register_func("AREF", aref);
}