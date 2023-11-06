#include <stdio.h>
#include <string.h>
#include "test.h"
#include "objects.h"
#include "symbols.h"
#include "parser.h"

object_t *intern(object_t *list);
object_t *concat(object_t *list);
object_t *symbol_name(object_t *list);

void error(char *str)
{
  printf("%s", str);
}

/**
 * Тест создания символа на основе строки
 */
void test_intern()
{
    printf("test_intern: ");
    object_t *obj = new_pair(object_new(STRING, "ABC"), NULL);
    object_t *res = intern(obj);
    ASSERT(res->type, SYMBOL);
    ASSERT(strcmp(res->u.symbol->str, "ABC"), 0);
}

/**
 * Тест создания символа на основе строки
 * Ошибка: некорректный тип параметра
 */
void test_intern_incorrect_type()
{
    printf("test_intern_incorrect_type: ");
    object_t *obj = new_pair(object_new(SYMBOL, "ABC"), NULL);
    object_t *res = intern(obj);
    ASSERT(res, ERROR);
}

/**
 * Тест создания символа на основе строки
 * Ошибка: лишний параметр
 */
void test_intern_two_param()
{
    printf("test_intern_two_param: ");
    object_t *obj = new_pair(object_new(STRING, "ABC"), new_pair(object_new(STRING, "ABC"), NULL));
    object_t *res = intern(obj);
    ASSERT(res, ERROR);
}

/**
 * Тест создания символа на основе строки
 * Ошибка: NULL в параметре-списке
 */
void test_intern_null_first_param()
{
    printf("test_intern_null_first_param: ");
    object_t *obj = new_pair(NULL, NULL);
    object_t *res = intern(obj);
    ASSERT(res, ERROR);
}

/**
 * Тест создания символа на основе строки
 * Ошибка: без параметра
 */
void test_intern_no_params()
{
    printf("test_intern_no_params: ");
    object_t *res = intern(NULL);
    ASSERT(res, ERROR);
}

/**
 * Тест функции объединения строк
 * Передаём одну строку
 */
void test_concat_one_str()
{
    printf("test_concat_one_str: ");
    object_t *obj = new_pair(object_new(STRING, "ab "), NULL);
    object_t *res = concat(obj);
    ASSERT(res->type, STRING);
    ASSERT(strcmp(res->u.str->data, "ab "), 0);
}

/**
 * Тест функции объединения строк
 * Передаём две строки
 */
void test_concat_two_str()
{
    printf("test_concat_two_str: ");
    object_t *obj = new_pair(object_new(STRING, "ab "), new_pair(object_new(STRING, "cd"), NULL));
    object_t *res = concat(obj);
    ASSERT(res->type, STRING);
    ASSERT(strcmp(res->u.str->data, "ab cd"), 0);
}

/**
 * Тест функции объединения строк
 * Передаём три строки
 */
void test_concat_three_str()
{
    printf("test_concat_three_str: ");
    object_t *obj = new_pair(object_new(STRING, ""), new_pair(object_new(STRING, "cd"), new_pair(object_new(STRING, "_ef"), NULL)));
    object_t *res = concat(obj);
    ASSERT(res->type, STRING);
    ASSERT(strcmp(res->u.str->data, "cd_ef"), 0);
}

/**
 * Тест функции объединения строк
 * Ошибка: некорректный тип параметра
 */
void test_concat_incorrect_type()
{
    printf("test_concat_incorrect_type: ");
    object_t *obj = new_pair(object_new(SYMBOL, "ab "), new_pair(object_new(STRING, "cd"), NULL));
    object_t *res = concat(obj);
    ASSERT(res, ERROR);
}

/**
 * Тест функции объединения строк
 * Ошибка: NULL в параметре-списке
 */
void test_concat_null_first_param()
{
    printf("test_concat_null_first_param: ");
    object_t *obj = new_pair(NULL, NULL);
    object_t *res = concat(obj);
    ASSERT(res, ERROR);
}

/**
 * Тест функции объединения строк
 * Ошибка: без параметра
 */
void test_concat_no_params()
{
    printf("test_concat_no_params: ");
    object_t *res = concat(NULL);
    ASSERT(res, ERROR);
}

/**
 * Тест функции получения имени символа
 */
void test_symbol_name()
{
    printf("test_symbol_name: ");
    object_t *obj = new_pair(object_new(SYMBOL, "abcd"), NULL);
    object_t *res = symbol_name(obj);
    ASSERT(res->type, STRING);
    ASSERT(strcmp(res->u.str->data, "abcd"), 0);
}

/**
 * Тест функции получения имени символа
 * Ошибка: некорректный тип параметра
 */
void test_symbol_name_incorrect_type()
{
    printf("test_symbol_name_incorrect_type: ");
    object_t *obj = new_pair(object_new(STRING, "abcd"), NULL);
    object_t *res = symbol_name(obj);
    ASSERT(res, ERROR);
}

/**
 * Тест функции получения имени символа
 * Ошибка: лишний параметр
 */
void test_symbol_name_many_params()
{
    printf("test_symbol_name_many_params: ");
    object_t *obj = new_pair(object_new(SYMBOL, "abcd"), new_pair(object_new(SYMBOL, "ef"), NULL));
    object_t *res = symbol_name(obj);
    ASSERT(res, ERROR);
}

/**
 * Тест функции получения имени символа
 * Ошибка: NULL в параметре-списке
 */
void test_symbol_name_null_first_param()
{
    printf("test_symbol_name_null_first_param: ");
    object_t *obj = new_pair(NULL, NULL);
    object_t *res = symbol_name(obj);
    ASSERT(res, ERROR);
}

/**
 * Тест функции получения имени символа
 * Ошибка: без параметра
 */
void test_symbol_name_no_params()
{
    printf("test_symbol_name_no_params: ");
    object_t *res = symbol_name(NULL);
    ASSERT(res, ERROR);
}

int main()
{
    printf("------------test_str_int---------\n");
    init_regions();
    test_intern();
    test_intern_incorrect_type();
    test_intern_two_param();
    test_intern_null_first_param();
    test_intern_no_params();
    test_concat_one_str();
    test_concat_two_str();
    test_concat_three_str();
    test_concat_incorrect_type();
    test_concat_null_first_param();
    test_concat_no_params();
    test_symbol_name();
    test_symbol_name_incorrect_type();
    test_symbol_name_many_params();
    test_symbol_name_null_first_param();
    test_symbol_name_no_params();
    return 0;
}
