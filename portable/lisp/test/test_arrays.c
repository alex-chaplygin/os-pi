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
 * Проверка создания массива с отрицательной длиной
 * Ожидается, что вызов завершится ошибкой
 */
void test_make_array_negative_length()
{
    printf("test_make_array_negative_length: ");
    int size = -5; // Отрицательная длина массива
    if (setjmp(jmp_env) == 0) {
	object_t arr = make_array(new_number(size)); // Попытка создать массив
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
    int size = 0;
    object_t arr = make_array(new_number(size));
    ASSERT(TYPE(arr), ARRAY);
    ASSERT(GET_ARRAY(arr)->length, size);
}

/**
 * Тест для make_array: создание массива с некорректным аргументом
 */
void test_make_array_invalid_argument()
{
    printf("test_make_array_invalid_argument: ");
    object_t size = NEW_SYMBOL("invalid");
    if (setjmp(jmp_env) == 0) {
        object_t arr = make_array(size);
        FAIL;
    } else
        OK;
}

/**
 * Проверка присваивания значения объекту c index NULLOBJ
*/
void test_seta_invalid_arguments()
{
    printf("test_seta_invalid_arguments: ");
    object_t arr_o = make_array(new_number(10)); 
    int num = 42;
    object_t obj = new_number(num);
    object_t result;
    if (setjmp(jmp_env) == 0) {
        result = seta(arr_o, NULLOBJ, obj);
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
    object_t arr_o = make_array(new_number(10)); 
    int num = 42;
    int index = 0;
    object_t obj = new_number(num);
    object_t value = new_number(num);
    if (setjmp(jmp_env) == 0) {
        object_t result = seta(arr_o, NULLOBJ, value);
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
    object_t arr_o = make_array(new_number(10));
    seta(arr_o, new_number(0), new_number(10));
    seta(arr_o, new_number(0), new_number(1));
    ASSERT(get_value(aref(arr_o, 0)), 1)
}

/**
 * Тест для seta: индекс вне диапазона
 */
void test_seta_out_of_bounds()
{
   printf("test_seta_out_of_bounds: ");
    object_t arr_o = make_array(new_number(5)); 
    object_t value = new_number(42); 

    if (setjmp(jmp_env) == 0) {
        object_t result = seta(arr_o, new_number(-1), value); 
        FAIL; 
    } else {
        OK; 
    }

    if (setjmp(jmp_env) == 0) {
        object_t result = seta(arr_o, new_number(5), value); 
        FAIL; 
    } else {
        OK; 
    }
}

/**
 * Тестирование чтени элемента массива
 * Создать массив на 3 элемента 
 * Присвоить во 2 ячейку число 4 
 * Проверить число во 2 ячейке (aref arr 2)
 */
void test_aref()
{
    printf("test_aref: ");
    object_t arr_o = make_array(new_number(3));
    seta(arr_o, new_number(1), new_number(4));
    ASSERT(get_value(aref(arr_o, new_number(1))), 4);
}


/**
 * Тест для array_size: проверка массива длиной 10.
 */
void test_correct_array_size()
{
  printf("test_correct_array_size: ");
  object_t arr_o = make_array(new_number(10));
  object_t result = array_size(arr_o);
  ASSERT(get_value(result), 10);
}


/**
 * Тест для array_size: некорректный аргумент
 */
void test_array_size_invalid_input()
{
    printf("test_array_size_invalid_input: ");
    object_t non_array_obj = new_number(100);
    
    if (setjmp(jmp_env) == 0) {
        object_t result = array_size(non_array_obj); 
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

int main()
{
    printf("------------test_arrays---------\n");
    init_regions();
    init_objects();
    test_make_array_negative_length();
    test_make_array_zero_length();
    test_make_array_invalid_argument();
    test_seta_invalid_arguments();
    test_seta_not_array();
    test_seta_update_value();
    test_seta_out_of_bounds();
    test_aref();
    test_correct_array_size();
    test_array_size_invalid_input();
    test_array_size_null();
    return 0;
}
