#include <stdio.h>
#include <setjmp.h>
#include "test.h"
#include "objects.h"
#include "alloc.h"
#include "symbols.h"
#include "array.h"
#include "parser.h"

jmp_buf jmp_env;
/// текущее окружение
object_t current_env = NULLOBJ;
/// окружение функции
object_t func_env = NULLOBJ;

void error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
}

/**
 * Проверка создания массивов:
 * Создаём массив из 10 элементов и проверяем, что все элементы являются NULLOBJ
 */
void test_make_array()
{
    printf("test_make_array: ");
    int length = 10;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);
    PRINT(list);
    PRINT(arr);
    ASSERT(TYPE(arr), ARRAY);
    for (int i = 0; i < length; i++)
        ASSERT(GET_ARRAY(arr)->data[i], NULLOBJ);
}

/**
 * Проверка создания массива с отрицательной длиной
 * Ожидается, что вызов завершится ошибкой
 */
void test_make_array_negative_length()
{
    printf("test_make_array_negative_length: ");
    int length = -5; // Отрицательная длина массива
    object_t list = new_pair(new_number(length), NULLOBJ);
    if (setjmp(jmp_env) == 0) {
	object_t arr = make_array(list); // Попытка создать массив
	FAIL; // Если не вызвана ошибка, тест провален
    } else 
	OK; // Если вызвана ошибка, тест успешен
}

/**
 * Тест для make_array: создание массива с нулевой длиной
 */
void test_make_array_zero_length()
{
    printf("test_make_array_zero_length: ");
    int length = 0;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);
    ASSERT(TYPE(arr), ARRAY);
    ASSERT(GET_ARRAY(arr)->length, length);
}

/**
 * Тест для make_array: создание массива с некорректным аргументом
 */
void test_make_array_invalid_argument()
{
    printf("test_make_array_invalid_argument: ");
    object_t list = new_pair(NEW_SYMBOL("invalid"), NULLOBJ);
    if (setjmp(jmp_env) == 0) {
        object_t arr = make_array(list);
        FAIL;
    } else
        OK;
}

/**
 * Тест для make_array: создание массива с лишними аргументами
 */
void test_make_array_many_arguments()
{
    printf("test_make_array_many_arguments: ");
    object_t list = new_pair(new_number(5), new_pair(new_number(10), NULLOBJ));
    if (setjmp(jmp_env) == 0) {
        object_t arr = make_array(list);
        FAIL;
    } else
        OK;
}

/**
 * Проверка присваивания значения массиву:
 * Создаём массив на 5 элементов и присваиваем разные типы объектов в 0, 2, 4 и 7 индексы
*/
void test_seta()
{
    printf("test_seta: ");
    int length = 5;
    int num1 = 1337;
    int num2 = 42;
    int num3 = 0;
    int num4 = 2;
    int num5 = 4;
    int num6 = 7;
    object_t obj1 = new_number(num1);
    object_t obj2 = NEW_SYMBOL("ABCDEF");
    object_t obj3 = NEW_STRING("qwerty");
    object_t obj4 = new_number(num2);
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);
    object_t cmd1 = new_pair(arr, new_pair(new_number(num3), new_pair(obj1, NULLOBJ)));
    object_t cmd2 = new_pair(arr, new_pair(new_number(num4), new_pair(obj2, NULLOBJ)));
    object_t cmd3 = new_pair(arr, new_pair(new_number(num5), new_pair(obj3, NULLOBJ)));
    object_t cmd4 = new_pair(arr, new_pair(new_number(num6), new_pair(obj4, NULLOBJ)));
    seta(cmd1);
    seta(cmd2);
    seta(cmd3);
    ASSERT(GET_ARRAY(arr)->data[num3], obj1);
    ASSERT(GET_ARRAY(arr)->data[num4], obj2);
    ASSERT(GET_ARRAY(arr)->data[num5], obj3);
    if (setjmp(jmp_env) == 0) {
        seta(cmd4);
        FAIL;
    } else
        OK;
}

/**
 * Проверка присваивания значения объекту и NULL
*/
void test_seta_invalid_arguments()
{
    printf("test_seta_invalid_arguments: ");

    int num = 42;
    object_t obj = new_number(num);
    object_t result;
    if (setjmp(jmp_env) == 0) {
        result = seta(new_pair(obj, NULLOBJ));
        FAIL;
    } else
        OK;
    
    if (setjmp(jmp_env) == 0) {
        result = seta(NULLOBJ);
        FAIL;
    } else
        OK;
}

/**
* Проверка присваивания значения с лишними аргументами
*/
void test_seta_many_args()
{
    printf("test_seta_many_args: ");

    int num1 = 42;
    int num2 = 42;
    int num3 = 42;
    int num4 = 42;
    object_t obj1 = new_number(num1);
    object_t obj2 = new_number(num2);
    object_t obj3 = new_number(num3);
    object_t obj4 = new_number(num4);
    object_t list = new_pair(obj1, new_pair(obj2, new_pair(obj3, new_pair(obj4, NULLOBJ))));
    if (setjmp(jmp_env) == 0) {
        object_t result = seta(list);   
        FAIL;
    } else
        OK;
}

/**
 * Проверка присваивания значения массиву неверной структуры
*/
void test_seta_not_array()
{
    printf("test_seta_not_array: ");

    int num = 42;
    int index = 0;
    object_t obj = new_number(num);
    object_t value = new_number(num);
    object_t list = new_pair(obj, new_pair(new_number(index), new_pair(value, NULLOBJ)));
    if (setjmp(jmp_env) == 0) {
        object_t result = seta(list);
        FAIL;
    } else
        OK;
}

/**
 * Тест для seta: обновление существующего элемента
 */
void test_seta_update_value()
{
    printf("test_seta_update_value: ");
    int length = 3;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);

    object_t old_value = NEW_SYMBOL("OLD");
    object_t new_value = NEW_SYMBOL("NEW");

    // Установка первоначального значения
    seta(new_pair(arr, new_pair(new_number(1), new_pair(old_value, NULLOBJ))));
    ASSERT(GET_ARRAY(arr)->data[1], old_value);

    // Обновление значения
    seta(new_pair(arr, new_pair(new_number(1), new_pair(new_value, NULLOBJ))));
    ASSERT(GET_ARRAY(arr)->data[1], new_value);
}

/**
 * Тест для seta: индекс вне диапазона
 */
void test_seta_out_of_bounds()
{
    printf("test_seta_out_of_bounds: ");
    int length = 3;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);

    object_t value = new_number(42);

    // Индекс больше длины массива
    if (setjmp(jmp_env) == 0) {
        seta(new_pair(arr, new_pair(new_number(10), new_pair(value, NULLOBJ))));
        FAIL;
    } else
        OK;

    // Отрицательный индекс
    if (setjmp(jmp_env) == 0) {
        seta(new_pair(arr, new_pair(new_number(-1), new_pair(value, NULLOBJ))));
        FAIL;
    } else
        OK;
}

/**
 * Тестирование чтени элемента массива
 * Создать массив на 3 элемента 
 * Присвоить во 2 ячейку число 4 
 * Проверить число во 2 ячейке (aref arr 2)
 * Проверить 0 элемент массива
 */
void test_aref()
{
    printf("test_aref: ");
    int length = 3;
    int num = 4;
    int idx = 2;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);
    object_t obj = new_number(num);
    GET_ARRAY(arr)->data[2] = obj;
    object_t elem = aref(new_pair(arr, new_pair(new_number(idx), NULLOBJ)));
    ASSERT(get_value(elem), num);
    idx = 0;
    elem = aref(new_pair(arr, new_pair(new_number(idx), NULLOBJ)));
    ASSERT(elem, NULLOBJ);
}

/**
 * Тестирование чтения элемента массива по неверному индексу
 * Индекс передается как параметр index
 */
void test_aref_invalid_index(int index)
{
    printf("test_aref_invalid_index: ");
    int length = 3;
    int num = 4;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);
    object_t obj = new_number(num);
    GET_ARRAY(arr)->data[2] = obj;
    if (setjmp(jmp_env) == 0) {
        object_t elem = aref(new_pair(arr, new_pair(new_number(index), NULLOBJ)));
        FAIL;
    } else
        OK;
}

/**
 * Тестирование обращения по индексу к объекту, не являющемуся массивом
 */
void test_aref_invalid_array()
{
    printf("test_aref_invalid_array: ");
    int idx = 2;
    object_t obj = new_number(idx);
    if (setjmp(jmp_env) == 0) {
        object_t elem = aref(new_pair(obj, new_pair(new_number(idx), NULLOBJ)));
        FAIL;
    } else
        OK;
}

/**
 * Тестирование чтения элемента массива без параметров
 */
void test_aref_no_args()
{
    printf("test_aref_no_args: ");
    if (setjmp(jmp_env) == 0) {
        object_t elem = aref(NULLOBJ);
        FAIL;
    } else
        OK;
}

/**
 * Тестирование чтения элемента массива с одним параметром (только массив без индекса)
 */
void test_aref_only_array()
{
    printf("test_aref_only_array: ");
    int length = 3;
    object_t list = new_pair(new_number(length), NULLOBJ);
    if (setjmp(jmp_env) == 0) {
        object_t elem = aref(list);    
        FAIL;
    } else
        OK;
}

/**
 * Тестирование чтения элемента массива по неверному типу индекса
 */
void test_aref_invalid_index_type()
{
    printf("test_aref_invalid_index_type: ");
    int length = 3;
    int num = 4;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);
    object_t obj = new_number(num);
    GET_ARRAY(arr)->data[2] = obj;
    if (setjmp(jmp_env) == 0) {
        object_t elem = aref(new_pair(arr, new_pair(NEW_OBJECT(PAIR, new_pair(NULLOBJ, NULLOBJ)), NULLOBJ)));
        FAIL;
    } else
        OK;
}

/**
 * Тест для aref: чтение без указания индекса
 */
void test_aref_no_index()
{
    printf("test_aref_no_index: ");
    int length = 3;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);

    if (setjmp(jmp_env) == 0) {
        object_t elem = aref(new_pair(arr, NULLOBJ));
        FAIL;
    } else
        OK;
}

/**
 * Тест для aref: чтение элементов с граничными индексами
 */
void test_aref_boundary_indices()
{
    printf("test_aref_boundary_indices: ");
    int length = 5;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);

    object_t value_start = new_number(1);
    object_t value_end = new_number(2);

    // Устанавливаем значения
    GET_ARRAY(arr)->data[0] = value_start;
    GET_ARRAY(arr)->data[length - 1] = value_end;

    // Проверяем первый элемент
    object_t elem = aref(new_pair(arr, new_pair(new_number(0), NULLOBJ)));
    ASSERT(elem, value_start);

    // Проверяем последний элемент
    elem = aref(new_pair(arr, new_pair(new_number(length - 1), NULLOBJ)));
    ASSERT(elem, value_end);
}

/**
 * Тест для aref: лишние аргументы
 */
void test_aref_extra_arguments()
{
    printf("test_aref_extra_arguments: ");
    int length = 3;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);

    if (setjmp(jmp_env) == 0) {
	object_t index = new_number(1);
	object_t extra_arg = new_number(10);
	object_t pair = new_pair(arr, new_pair(index, new_pair(extra_arg, NULLOBJ)));
	aref(pair);
	FAIL;
    } else
	OK;
}

/**
 * Тест для array_size: проверка массива длиной 10.
 */
void test_correct_array_size()
{
  printf("test_correct_array_size: ");
  int length = 10;
  object_t list = new_pair(new_number(length), NULLOBJ);
  object_t arr = make_array(list);
  
  object_t result = array_size(new_pair(arr, NULLOBJ));
  ASSERT(get_value(result), length);
}

/**
 * Тест для array_size: проверка пустого массива
 */
void test_array_size_empty()
{
    printf("test_array_size_empty: ");
    int length = 0;
    object_t list = new_pair(new_number(length), NULLOBJ); 
    object_t arr = make_array(list); 
    object_t result = array_size(new_pair(arr, NULLOBJ)); 
    
    ASSERT(get_value(result), length); 
}

/**
 * Тест для array_size: некорректный аргумент
 */
void test_array_size_invalid_input()
{
    printf("test_array_size_invalid_input: ");
    object_t non_array_obj = new_number(100);
    
    if (setjmp(jmp_env) == 0) {
        object_t result = array_size(new_pair(non_array_obj, NULLOBJ)); 
        FAIL; 
    } else 
        OK; 
    
}

/**
 * Тест для array_size: передача NULL объекта в качестве входных данных
 */
void test_array_size_null()
{
    printf("test_array_size_null: ");
    
    if (setjmp(jmp_env) == 0) {
        object_t result = array_size(NULLOBJ); 
        FAIL; 
    } else 
        OK; 
    
}

/**
 * Тест для array_size: лишние аргументы
 */
void test_array_size_extra_arguments()
{
    printf("test_array_size_extra_arguments: ");
    int length = 3;
    object_t list = new_pair(new_number(length), NULLOBJ);
    object_t arr = make_array(list);

    if (setjmp(jmp_env) == 0) {
	object_t extra_arg = new_number(10);
	object_t pair = new_pair(arr, new_pair(extra_arg, NULLOBJ));
	array_size(pair);
	FAIL;
    } else
	OK;
}

int main()
{
    printf("------------test_arrays---------\n");
    init_regions();
    init_objects();
    test_make_array();
    test_make_array_negative_length();
    test_make_array_zero_length();
    test_make_array_invalid_argument();
    test_seta();
    test_seta_invalid_arguments();
    test_seta_not_array();
    test_seta_many_args();
    test_seta_update_value();
    test_seta_out_of_bounds();
    test_aref();
    test_aref_invalid_index(10);
    test_aref_invalid_index(-1);
    test_aref_invalid_index_type();
    test_aref_no_args();
    test_aref_only_array();
    test_aref_invalid_array();
    test_aref_no_index();
    test_aref_boundary_indices();
    test_aref_extra_arguments();
    test_correct_array_size();
    test_array_size_empty();
    test_array_size_invalid_input();
    test_array_size_null();
    test_array_size_extra_arguments();
    return 0;
}
