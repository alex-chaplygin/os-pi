#include <stdio.h>
#include <string.h>
#include "test.h"
#include "objects.h"
#include "symbols.h"
#include "parser.h"

object_t *intern(object_t *list);
object_t *concat(object_t *list);
object_t *symbol_name(object_t *list);
object_t *string_size(object_t *list);
object_t *str_char(object_t *list);
object_t *code_char(object_t *list);
object_t *subseq(object_t *list);

void error(char *str, ...)
{
  printf("%s", str);
}

char *itoa(int num, char *str, int rad)
{
    int i = 15;
    int neg = 0;
    str[i - 1] = 0;
    char *p = &str[i - 1];
    if (num == 0)
	    *--p = '0';
    if (num < 0) {
        neg = 1;
        num *= -1;
    }
    while (num > 0) {
        int currchar = num % rad;
        p--;
        *p = '0' + currchar;
        num = num / rad;
    }
    if (neg)
        *--p = '-';
    return p;
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
 * Ошибка: без параметра
 */
void test_intern_no_params()
{
    printf("test_intern_no_params: ");
    object_t *res = intern(NULL);
    ASSERT(res, ERROR);
}

/**
 * Тест создания символа на основе пустой строки
 * Ошибка: пустая строка
 */
void test_intern_empty_string()
{
    printf("test_intern_empty_string: ");
    object_t *obj = new_pair(object_new(STRING, ""), NULL);
    object_t *res = intern(obj);
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
    ASSERT(res->type, STRING);
    ASSERT(strcmp(res->u.str->data, "NIL"), 0);
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

/**
 * Тест функции получения длинны строки
 */
void test_string_size() {
    printf("test_string_size: ");
    char *str = "Hello";
    object_t *string_obj = object_new(STRING, str);
    object_t *params = new_pair(string_obj, NULL);
    object_t *res = string_size(params);
    ASSERT(res->type, NUMBER);
    ASSERT(res->u.value, strlen(str));
}

/**
 * Тест функции получения длинны строки
 * Ошибка: без параметра
 */
void test_string_size_no_arguments() {
    printf("test_string_size_no_arguments: ");
    object_t *result = string_size(NULL);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения длинны строки
 * Ошибка: несколько аргументов
 */
void test_string_size_too_many_arguments() {
    printf("test_string_size_too_many_arguments: ");
    int num = 5;
    object_t *number_obj = object_new(NUMBER, &num);
    object_t *params = new_pair(number_obj, new_pair(number_obj, NULL));
    object_t *result = string_size(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения длинны строки
 * Ошибка: аргумент не строка
 */
void test_string_size_not_string() {
    printf("test_string_size_not_string: ");
    int num = 5;
    object_t *number_obj = object_new(NUMBER, &num);
    object_t *params = new_pair(number_obj, NULL);
    object_t *result = string_size(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения символа по индексу в строке
 * Получение 2 символа строки
 */
void test_str_char() {
    printf("test_str_char: ");
    char *str = "Hello";
    int ind = 1;
    object_t *string_obj = object_new(STRING, str);
    object_t *index_obj = object_new(NUMBER, &ind);
    object_t *params = new_pair(string_obj, new_pair(index_obj, NULL));
    object_t *result = str_char(params);
    ASSERT(result->type, NUMBER);
    ASSERT(result->u.value, 'e');
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: без параметра
 */
void test_str_char_null() {
    printf("test_str_char_null: ");
    object_t *result = str_char(NULL);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: не хватает одного параметра
 */
void test_str_char_not_all_arguments() {
    printf("test_str_char_not_all_arguments: ");
    char *str = "Hello";
    object_t *string_obj = object_new(STRING, str);
    object_t *params = new_pair(string_obj, NULL);
    object_t *result = str_char(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: второй аргумент не число
 */
void test_str_char_second_not_number() {
    printf("test_str_char_second_not_number: ");
    char *str = "Hello";
    object_t *string_obj = object_new(STRING, str);
    object_t *not_number = object_new(STRING, "abc");
    object_t *params = new_pair(string_obj, new_pair(not_number, NULL));
    object_t *result = str_char(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: аргументов больше чем 2
 */
void test_str_char_too_many_arguments() {
    printf("test_str_char_too_many_arguments: ");
    char *str = "Hello";
    int ind = 0;
    object_t *string_obj = object_new(STRING, str);
    object_t *index_obj = object_new(NUMBER, &ind);
    object_t *extra_arg = object_new(NUMBER, &ind);
    object_t *params = new_pair(string_obj, new_pair(index_obj, new_pair(extra_arg, NULL)));
    object_t *result = str_char(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: первый аргумент не строка
 */
void test_str_char_first_not_string() {
    printf("test_str_char_first_not_string: ");
    int ind = 0;
    object_t *not_string = object_new(NUMBER, &ind);
    object_t *index_obj = object_new(NUMBER, &ind);
    object_t *params = new_pair(not_string, new_pair(index_obj, NULL));
    object_t *result = str_char(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: неверный индекс символа
 */
void test_str_char_invalid_index() {
    printf("test_str_char_invalid_index: ");
    char *str = "Hello";
    int ind = 10;
    object_t *string_obj = object_new(STRING, str);
    object_t *index_obj = object_new(NUMBER, &ind);
    object_t *params = new_pair(string_obj, new_pair(index_obj, NULL));
    object_t *result = str_char(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции создания символа-строки по коду
 * Получение 1го символа строки
 */
void test_code_char() {
    printf("test_code_char: ");
    int ind = 110; // 110 - код символа 'n'

    object_t *params = new_pair(object_new(NUMBER, &ind), NULL);
    object_t *res = code_char(params);

    ASSERT(res->type, STRING);
    ASSERT(res->u.str->data[0], 'n');
}

/**
 * Тест функции создания символа-строки по коду
 * Ошибка: нет аргументов
 */
void test_code_char_no_arguments() {
    printf("test_code_char_no_arguments: ");
    object_t *result = code_char(NULL);
    ASSERT(result, ERROR);
}

/**
 * Тест функции создания символа-строки по коду
 * Ошибка: слишком много аргументов
 */
void test_code_char_too_many_arguments() {
    printf("test_code_char_too_many_arguments: ");
    int num = 2;
    object_t *number_obj = object_new(NUMBER, &num);
    object_t *params = new_pair(number_obj, new_pair(number_obj, NULL));
    object_t *result = code_char(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции создания символа-строки по коду
 * Ошибка: передано не число
 */
void test_code_char_not_number() {
    printf("test_code_char_not_number: ");
    char *str = "Hello";
    object_t *not_number = object_new(STRING, str);
    object_t *params = new_pair(not_number, NULL); 
    object_t *result = code_char(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения подстроки из строки
 * Ошибка: без параметров
 */
void test_subseq_no_arguments() {
    printf("test_subseq_no_arguments: ");
    object_t *result = subseq(NULL);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения подстроки из строки
 * Ошибка: не хватает аргументов
 */
void test_subseq_not_all_arguments() {
    printf("test_subseq_not_all_arguments: ");
    char *str = "Hello";
    object_t *string_obj = object_new(STRING, str);
    object_t *params = new_pair(string_obj, NULL);
    object_t *result = subseq(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения подстроки из строки
 * Ошибка: аргументов больше чем требуется
 */
void test_subseq_too_many_arguments() {
    printf("test_subseq_too_many_arguments: ");
    char *str = "Hello";
    int start = 1;
    int end = 4;
    int extra = 5;
    object_t *string_obj = object_new(STRING, str);
    object_t *start_index = object_new(NUMBER, &start);
    object_t *end_index = object_new(NUMBER, &end);
    object_t *extra_arg = object_new(NUMBER, &extra);
    object_t *params = new_pair(string_obj, new_pair(start_index, new_pair(end_index, new_pair(extra_arg, NULL))));
    object_t *result = subseq(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения подстроки из строки
 * Ошибка: неверный аргумент
 */
void test_subseq_invalid_args() {
    printf("test_subseq_invalid_args: ");
    int temp = 123;
    int start = 1;
    int end = 4;
    object_t *not_string = object_new(NUMBER, &temp);
    object_t *start_index = object_new(NUMBER, &start);
    object_t *end_index = object_new(NUMBER, &end);
    object_t *params = new_pair(not_string, new_pair(start_index, new_pair(end_index, NULL)));
    object_t *result = subseq(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения подстроки из строки
 * Ошибка: неверный индекс
 */
void test_subseq_negative_index() {
    printf("test_subseq_negative_index: ");
    char *str = "Hello";
    int start = -1;
    int end = 4;
    object_t *string_obj = object_new(STRING, &str);
    object_t *start_index = object_new(NUMBER, &start);
    object_t *end_index = object_new(NUMBER, &end);
    object_t *params = new_pair(string_obj, new_pair(start_index, new_pair(end_index, NULL)));
    object_t *result = subseq(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения подстроки из строки
 * Ошибка: индекс за границами длины строки
 */
void test_subseq_invalid_index_range() {
    printf("test_subseq_invalid_index_range: ");
    char *str = "Hello";
    int start = 1;
    int end = 10;
    object_t *string_obj = object_new(STRING, str);
    object_t *start_index = object_new(NUMBER, &start);
    object_t *end_index = object_new(NUMBER, &end);
    object_t *params = new_pair(string_obj, new_pair(start_index, new_pair(end_index, NULL)));
    object_t *result = subseq(params);
    ASSERT(result, ERROR);
}

/**
 * Тест функции получения подстроки из строки
 * Получение требуемой подстроки
 */
void test_subseq() {
    printf("test_subseq: ");
    char *str = "Hello";
    int start = 1;
    int end = 3;
    object_t *string_obj = object_new(STRING, str);
    object_t *start_index = object_new(NUMBER, &start);
    object_t *end_index = object_new(NUMBER, &end);
    object_t *params = new_pair(string_obj, new_pair(start_index, new_pair(end_index, NULL)));
    object_t *result = subseq(params);
    ASSERT(result->type, STRING);
    ASSERT(strcmp(result->u.str->data, "el"), 0);
}

int main()
{
    printf("------------test_str_int---------\n");
    init_regions();
    test_intern();
    test_intern_incorrect_type();
    test_intern_two_param();
    test_intern_no_params();
    test_intern_empty_string();
    test_concat_one_str();
    test_concat_two_str();
    test_concat_three_str();
    test_concat_incorrect_type();
    test_concat_no_params();
    test_symbol_name();
    test_symbol_name_incorrect_type();
    test_symbol_name_many_params();
    test_symbol_name_null_first_param();
    test_symbol_name_no_params();
    test_string_size();
    test_string_size_no_arguments();
    test_string_size_too_many_arguments();
    test_string_size_not_string();
    test_str_char();
    test_str_char_null();
    test_str_char_not_all_arguments();
    test_str_char_second_not_number();
    test_str_char_too_many_arguments();
    test_str_char_first_not_string();
    test_str_char_invalid_index();
    test_code_char();
    test_code_char_no_arguments();
    test_code_char_too_many_arguments();
    test_code_char_not_number();
    test_subseq_no_arguments();
    test_subseq_not_all_arguments();
    test_subseq_too_many_arguments();
    test_subseq_invalid_args();
    test_subseq_negative_index();
    test_subseq_invalid_index_range();
    test_subseq();
    return 0;
}
