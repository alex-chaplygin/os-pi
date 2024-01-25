#include <stdio.h>
#include "test.h"
#include "objects.h"
#include "symbols.h"
#include "array.h"
#include "parser.h"

void error(char *str, ...)
{
  printf("%s", str);
}

/**
 * Проверка создания массивов:
 * Создаём массив из 10 элементов и проверяем, что все элементы являются NULL
*/
void test_make_array()
{
    printf("test_make_array: ");
    int length = 10;
    object_t *list = new_pair(object_new(NUMBER, &length), NULL);
    object_t *arr = make_array(list);
    PRINT(list);
    PRINT(arr);
    ASSERT(arr->type, ARRAY);
    for (int i = 0; i < length; i++)
        ASSERT(arr->u.arr->data[i], NULL);
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
    object_t *obj1 = object_new(NUMBER, &num1);
    object_t *obj2 = object_new(SYMBOL, "ABCDEF");
    object_t *obj3 = object_new(STRING, "qwerty");
    object_t *obj4 = object_new(NUMBER, &num2);
    object_t *list = new_pair(object_new(NUMBER, &length), NULL);
    object_t *arr = make_array(list);
    object_t *cmd1 = new_pair(arr, new_pair(object_new(NUMBER, &num3), new_pair(obj1, NULL)));
    object_t *cmd2 = new_pair(arr, new_pair(object_new(NUMBER, &num4), new_pair(obj2, NULL)));
    object_t *cmd3 = new_pair(arr, new_pair(object_new(NUMBER, &num5), new_pair(obj3, NULL)));
    object_t *cmd4 = new_pair(arr, new_pair(object_new(NUMBER, &num6), new_pair(obj4, NULL)));
    object_t *cmd5 = new_pair(arr, new_pair(object_new(NUMBER, &num1), new_pair(obj4, new_pair(object_new(num4, &num1), NULL))));
    seta(cmd1);
    seta(cmd2);
    seta(cmd3);
    object_t *o = seta(cmd4);
    ASSERT(arr->u.arr->data[num3], obj1);
    ASSERT(arr->u.arr->data[num4], obj2);
    ASSERT(arr->u.arr->data[num5], obj3);
    ASSERT(o, ERROR);
    object_t *o2 = seta(cmd5);
    ASSERT(o2, ERROR);
}

/**
 * Проверка присваивания значения объекту и NULL
*/
void test_seta_invalid_arguments() {
    printf("test_seta_invalid_arguments: ");

    int num = 42;
    object_t *obj = object_new(NUMBER, &num);
    object_t *result = seta(new_pair(obj, NULL));
    ASSERT(result, ERROR);
    
    result = seta(NULL);
    ASSERT(result, ERROR);

}

/**
 * Проверка присваивания значения массиву неверной структуры
*/
void test_seta_not_array() {
    printf("test_seta_not_array: ");

    int num = 42;
    int index = 0;
    object_t *obj = object_new(NUMBER, &num);
    object_t *value = object_new(NUMBER, &num);
    object_t *list = new_pair(obj, new_pair(object_new(NUMBER, &index), new_pair(value, NULL)));

    object_t *result = seta(list);
    ASSERT(result, ERROR);
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
    object_t *list = new_pair(object_new(NUMBER, &length), NULL);
    object_t *arr = make_array(list);
    object_t *obj = object_new(NUMBER, &num);
    arr->u.arr->data[2] = obj;
    object_t *elem = aref(new_pair(arr, new_pair(object_new(NUMBER, &idx), NULL)));
    ASSERT(elem->u.value, num);
    idx = 0;
    elem = aref(new_pair(arr, new_pair(object_new(NUMBER, &idx), NULL)));
    ASSERT(elem, NULL);
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
    object_t *list = new_pair(object_new(NUMBER, &length), NULL);
    object_t *arr = make_array(list);
    object_t *obj = object_new(NUMBER, &num);
    arr->u.arr->data[2] = obj;

    object_t *elem = aref(new_pair(arr, new_pair(object_new(NUMBER, &index), NULL)));
    ASSERT(elem, ERROR);
}

/**
 * Тестирование обращения по индексу к объекту, не являющемуся массивом
 */
void test_aref_invalid_array()
{
    printf("test_aref_invalid_array: ");
    int idx = 2;
    object_t *obj = object_new(NUMBER, &idx);
    object_t *elem = aref(new_pair(obj, new_pair(object_new(NUMBER, &idx), NULL)));
    ASSERT(elem, ERROR);
}

/**
 * Тестирование чтения элемента массива без параметров
 */
void test_aref_no_args()
{
    printf("test_aref_no_args: ");
    object_t *elem = aref(NULL);
    ASSERT(elem, ERROR);
}

/**
 * Тестирование чтения элемента массива с одним параметром (только массив без индекса)
 */
void test_aref_only_array()
{
    printf("test_aref_only_array: ");
    int length = 3;
    object_t *list = new_pair(object_new(NUMBER, &length), NULL);
    object_t *elem = aref(list);    
    ASSERT(elem, ERROR);
}

/**
 * Тестирование чтения элемента массива по неверному типу индекса
 */
void test_aref_invalid_index_type()
{
    printf("test_aref_invalid_index_type: ");
    int length = 3;
    int num = 4;
    object_t *list = new_pair(object_new(NUMBER, &length), NULL);
    object_t *arr = make_array(list);
    object_t *obj = object_new(NUMBER, &num);
    arr->u.arr->data[2] = obj;

    object_t *elem = aref(new_pair(arr, new_pair(object_new(PAIR, new_pair(NULL, NULL)), NULL)));
    ASSERT(elem, ERROR);
}

int main()
{
    printf("------------test_arrays---------\n");
    init_regions();
    test_make_array();
    test_seta();
    test_seta_invalid_arguments();
    test_seta_not_array();
    test_aref();
    test_aref_invalid_index(10);
    test_aref_invalid_index(-1);
    test_aref_invalid_index_type();
    test_aref_no_args();
    test_aref_only_array();
    test_aref_invalid_array();
    return 0;
}
