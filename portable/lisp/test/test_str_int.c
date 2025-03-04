#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include "test.h"
#include "objects.h"
#include "alloc.h"
#include "symbols.h"
#include "parser.h"
#include "str.h"

object_t intern(object_t arg);
object_t concat(object_t list);
object_t symbol_name(object_t symbol);
object_t string_size(object_t str);
object_t str_char(object_t str, object_t index);
object_t code_char(object_t code);
object_t subseq(object_t str, object_t start_index, object_t end_index);
object_t int_to_str(object_t number);

/// текущее окружение
object_t current_env = NULLOBJ;
/// окружение функции
object_t func_env = NULLOBJ;
jmp_buf jmp_env;

void error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
}

object_t function(object_t param)
{
    return param;
}

int list_length(object_t args)
{
    int c = 0;
    while (args != NULLOBJ) {
	args = TAIL(args);
	c++;
    }
    return c;
}

char *itoa(int num, char *str, int rad)
{
    int i = MAX_ITOA_STR;
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
    object_t str_obj = NEW_STRING("ABC");
    object_t res = intern(str_obj);
    ASSERT(TYPE(res), SYMBOL);
    ASSERT(GET_STRING(res)->length, 0);
}

/**
 * Тест создания символа на основе строки
 * Ошибка: некорректный тип параметра
 */
void test_intern_incorrect_type()
{
    printf("test_intern_incorrect_type: ");
    object_t str_obj = NEW_SYMBOL("ABC");
    if (setjmp(jmp_env) == 0) {
        object_t res = intern(str_obj);
        FAIL;
    } else 
        OK;
}

/**
 * Тест создания символа на основе пустой строки
 * Ошибка: пустая строка
 */
void test_intern_empty_string()
{
    printf("test_intern_empty_string: ");
    object_t obj = NEW_STRING("");
    if (setjmp(jmp_env) == 0) {
        object_t res = intern(obj);
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции объединения строк
 * Передаём одну строку
 */
void test_concat_one_str()
{
    printf("test_concat_one_str: ");
    object_t obj = new_pair(NEW_STRING("ab "), NULLOBJ); 
    object_t res = concat(obj); 
    ASSERT(TYPE(res), STRING); 
    ASSERT(strcmp(GET_STRING(res)->data, "ab "), 0);
}


/**
 * Тест функции объединения строк
 * Передаём две строки
 */
void test_concat_two_str()
{
    printf("test_concat_two_str: ");
    object_t obj = new_pair(NEW_STRING("ab "), new_pair(NEW_STRING("cd"), NULLOBJ)); 
    object_t res = concat(obj); 
    ASSERT(TYPE(res), STRING); 
    ASSERT(strcmp(GET_STRING(res)->data, "ab cd"), 0);
}

/**
 * Тест функции объединения строк
 * Передаём три строки
 */
void test_concat_three_str()
{
    printf("test_concat_three_str: ");
    object_t obj = new_pair(NEW_STRING(""), new_pair(NEW_STRING("cd"), new_pair(NEW_STRING("_ef"), NULLOBJ))); 
    object_t res = concat(obj); 
    ASSERT(TYPE(res), STRING); 
    ASSERT(strcmp(GET_STRING(res)->data, "cd_ef"), 0);
}

/**
 * Тест функции объединения строк
 * Ошибка: некорректный тип параметра
 */
void test_concat_incorrect_type()
{
    printf("test_concat_incorrect_type: ");
    object_t obj = new_pair(NEW_SYMBOL("ab "), new_pair(NEW_STRING("cd"), NULLOBJ)); 
    if (setjmp(jmp_env) == 0) {
        object_t res = concat(obj); 
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции объединения строк
 * Ошибка: без параметра
 */
void test_concat_no_params()
{
    printf("test_concat_no_params: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = concat(NULLOBJ); 
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции получения имени символа
 */
void test_symbol_name()
{
    printf("test_symbol_name: ");
    object_t obj = NEW_SYMBOL("abcd"); 
    object_t res = symbol_name(obj); 
    ASSERT(TYPE(res), STRING);
    ASSERT(strcmp(GET_STRING(res)->data, "abcd"), 0);
}

/**
 * Тест функции получения имени символа
 * Ошибка: некорректный тип параметра
 */
void test_symbol_name_incorrect_type()
{
    printf("test_symbol_name_incorrect_type: ");
    object_t obj = NEW_STRING("abcd"); 
    if (setjmp(jmp_env) == 0) {
        object_t res = symbol_name(obj); 
        FAIL;
    } else 
        OK;
}


/**
 * Тест функции получения имени символа
 * Ошибка: NULL в параметре-списке
 */
void test_symbol_name_null()
{
    printf("test_symbol_name_null_first_param: ");
    object_t res = symbol_name(NULLOBJ); 
    ASSERT(TYPE(res), STRING); 
    ASSERT(strcmp(GET_STRING(res)->data, "NIL"), 0);
}


/**
 * Тест функции получения длины строки
 */
void test_string_size()
{
    printf("test_string_size: ");
    char *str = "Hello";
    object_t string_obj = NEW_STRING(str);
    object_t res = string_size(string_obj);
    ASSERT(TYPE(res), NUMBER);
    ASSERT(get_value(res), strlen(str));
}

/**
 * Тест функции получения длины строки
 * Ошибка: без параметра
 */
void test_string_size_null()
{
    printf("test_string_size_null: ");
    if (setjmp(jmp_env) == 0) {
        string_size(NULLOBJ);
        FAIL;
    } else 
        OK;
}


/**
 * Тест функции получения длины строки
 * Ошибка: аргумент не строка
 */
void test_string_size_not_string()
{
    printf("test_string_size_not_string: ");
    object_t number_obj = new_number(5);
    if (setjmp(jmp_env) == 0) {
        string_size(number_obj);
        FAIL;
    } else 
        OK;
}


/**
 * Тест функции получения символа по индексу в строке
 * Получение 2 символа строки
 */
void test_str_char()
{
    printf("test_str_char: ");
    char *str = "Hello";
    object_t string_obj = NEW_STRING(str);
    object_t index_obj = new_number(1);
    object_t result = str_char(string_obj, index_obj);
    ASSERT(TYPE(result), CHAR);
    ASSERT(GET_CHAR(result), 'e');
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: без параметра
 */
void test_str_char_null()
{
    printf("test_str_char_null: ");
    if (setjmp(jmp_env) == 0) {
        object_t result = str_char(NULLOBJ, NULLOBJ);
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: не хватает одного параметра
 */
void test_str_char_not_all_arguments()
{
    printf("test_str_char_not_all_arguments: ");
    char *str = "Hello";
    object_t string_obj = NEW_STRING(str);
    if (setjmp(jmp_env) == 0) {
        object_t result = str_char(string_obj, NULLOBJ);
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: второй аргумент не число
 */
void test_str_char_second_not_number()
{
    printf("test_str_char_second_not_number: ");
    char *str = "Hello";
    object_t string_obj = NEW_STRING(str);
    object_t not_number = NEW_STRING("abc");
    if (setjmp(jmp_env) == 0) {
        object_t result = str_char(string_obj, not_number);
        FAIL;
    } else 
        OK;
}


/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: первый аргумент не строка
 */
void test_str_char_first_not_string()
{
    printf("test_str_char_first_not_string: ");
    object_t not_string = new_number(0);
    object_t index_obj = new_number(0);
    if (setjmp(jmp_env) == 0) {
        object_t result = str_char(not_string, index_obj);
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции получения символа по индексу в строке
 * Ошибка: неверный индекс символа
 */
void test_str_char_invalid_index()
{
    printf("test_str_char_invalid_index: ");
    char *str = "Hello";
    object_t string_obj = NEW_STRING(str);
    object_t index_obj = new_number(10);
    if (setjmp(jmp_env) == 0) {
        object_t result = str_char(string_obj, index_obj);
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции создания символа-строки по коду
 * Получение 1го символа строки
 */
void test_code_char()
{
    printf("test_code_char: ");
    object_t num_obj = new_number(110); 
    object_t res = code_char(num_obj); 
    ASSERT(TYPE(res), CHAR); 
    ASSERT(GET_CHAR(res), 'n');
}

/**
 * Тест функции создания символа-строки по коду
 * Ошибка: передан null
 */
void test_code_char_null()
{
    printf("test_code_char_null: ");
    if (setjmp(jmp_env) == 0) {
	object_t result = code_char(NULLOBJ); 
	FAIL;
    } else 
	OK;
}

/**
 * Тест функции создания символа-строки по коду
 * Ошибка: передано не число
 */
void test_code_char_not_number()
{
    printf("test_code_char_not_number: ");
    object_t not_number = NEW_STRING("Hello"); 
    if (setjmp(jmp_env) == 0) {
        object_t result = code_char(not_number); 
        FAIL;
    } else 
        OK;
}



/**
 * Тест функции получения подстроки из строки
 * Ошибка: неверный аргумент
 */
void test_subseq_invalid_args()
{
    printf("test_subseq_invalid_args: ");
    object_t not_string = new_number(123); 
    object_t start_index = new_number(1); 
    object_t end_index = new_number(4); 
    if (setjmp(jmp_env) == 0) {
        object_t result = subseq(not_string, start_index, end_index); 
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции получения подстроки из строки
 * Ошибка: неверный индекс
 */
void test_subseq_negative_index()
{
    printf("test_subseq_negative_index: ");
    object_t string_obj = NEW_STRING("Hello"); 
    object_t start_index = new_number(-1); 
    object_t end_index = new_number(4);  
    if (setjmp(jmp_env) == 0) {
        object_t result = subseq(string_obj, start_index, end_index); 
        FAIL;
    } else 
        OK;
}
/**
 * Тест функции получения подстроки из строки
 * Ошибка: неверный конечный индекс
 */
void test_subseq_negative_end_index()
{
    printf("test_subseq_negative_end_index: ");
    object_t string_obj = NEW_STRING("Hello");
    object_t start_index = new_number(0);
    object_t end_index = new_number(-1);
    if (setjmp(jmp_env) == 0) {
        object_t result = subseq(string_obj, start_index, end_index); 
        FAIL;
    } else 
        OK;
}
/**
 * Тест функции получения подстроки из строки
 * Ошибка: индекс за границами длины строки
 */
void test_subseq_invalid_index_range()
{
    printf("test_subseq_invalid_index_range: ");
    object_t string_obj = NEW_STRING("Hello"); 
    object_t start_index = new_number(1); 
    object_t end_index = new_number(10); 
    if (setjmp(jmp_env) == 0) {
        object_t result = subseq(string_obj, start_index, end_index);  
        FAIL;
    } else 
        OK;
}
/**
 * Тест функции получения подстроки из строки
 * Ошибка: начальный индекс больше конечного
 */
void test_subseq_start_greater_than_end()
{
    printf("test_subseq_start_greater_than_end: ");
    object_t string_obj = NEW_STRING("Hello");
    object_t start_index = new_number(4);
    object_t end_index = new_number(2);
    if (setjmp(jmp_env) == 0) {
        object_t result = subseq(string_obj, start_index, end_index);
        FAIL;
    } else 
        OK;
}
/**
 * Тест функции получения подстроки из строки
 * На вход подаётся пустая строка
 */
void test_subseq_empty_input()
{
    printf("test_subseq_empty_input: ");
    object_t string_obj = NEW_STRING("");
    object_t start_index = new_number(0);
    object_t end_index = new_number(0);
    object_t result = subseq(string_obj, start_index, end_index);
    ASSERT(TYPE(result), STRING);
    ASSERT(strcmp(GET_STRING(result)->data, ""), 0);
}
/**
 * Тест функции получения подстроки из строки
 * Получение требуемой подстроки
 */
void test_subseq()
{
    printf("test_subseq: ");
    object_t string_obj = NEW_STRING("Hello"); 
    object_t start_index = new_number(1); 
    object_t end_index = new_number(3);
    object_t result = subseq(string_obj, start_index, end_index);  
    ASSERT(TYPE(result), STRING); 
    ASSERT(strcmp(GET_STRING(result)->data, "el"), 0);
}
/**
 * Тест функции получения подстроки из строки
 * Получение всей строки
 */
void test_subseq_full_string()
{
    printf("test_subseq_full_string: ");
    object_t string_obj = NEW_STRING("Hello, World!");
    object_t start_index = new_number(0);
    object_t end_index = new_number(13); 
    object_t result = subseq(string_obj, start_index, end_index); 
    ASSERT(TYPE(result), STRING);
    ASSERT(strcmp(GET_STRING(result)->data, "Hello, World!"), 0);
}

/**
 * Тест функции получения подстроки из строки
 * Получение пустой подстриоки, если start_index == end_index
 */
 void test_subseq_empty_substring()
{
    printf("test_subseq_empty_substring: ");
    object_t string_obj = NEW_STRING("Hello");
    object_t start_index = new_number(2);
    object_t end_index = new_number(2);
    object_t result = subseq(string_obj, start_index, end_index);
    ASSERT(TYPE(result), STRING);
    ASSERT(strcmp(GET_STRING(result)->data, ""), 0);
}

/**
 * Тест функции перевода целочисленного числа в строку
 * Ошибка: передается null 
 */
void test_int_to_str_null()
{
    printf("test_int_to_str_null: ");
    if (setjmp(jmp_env) == 0) {
        object_t result = int_to_str(NULLOBJ); 
        FAIL;
    } else 
        OK;
}


/**
 * Тест функции перевода целочисленного числа в строку
 * Ошибка: неверный параметр
 */
void test_int_to_str_invalid_arg()
{
    printf("test_int_to_str_invalid_arg: ");
    object_t string_obj = NEW_STRING("Number"); 
    if (setjmp(jmp_env) == 0) {
        object_t result = int_to_str(string_obj); 
        FAIL;
    } else 
        OK;
}

/**
 * Тест функции перевода целочисленного числа в строку
 * Перевод положительного числа в строку
 */
void test_int_to_str_positive()
{
    printf("test_int_to_str_positive: ");
    object_t number_obj = new_number(123); 
    object_t result = int_to_str(number_obj); 
    ASSERT(TYPE(result), STRING);
    ASSERT(strcmp(GET_STRING(result)->data, "123"), 0);
}

/**
 * Тест функции перевода целочисленного числа в строку
 * Перевод отрицательного числа в строку
 */
void test_int_to_str_negative()
{
    printf("test_int_to_str_negative: ");
    object_t number_obj = new_number(-123);
    object_t result = int_to_str(number_obj); 
    ASSERT(TYPE(result), STRING); 
    ASSERT(strcmp(GET_STRING(result)->data, "-123"), 0);
}

int main()
{
    printf("------------test_str_int---------\n");
    init_regions();
    init_objects();
    
    test_intern();
    test_intern_incorrect_type();
    test_intern_empty_string();
    
    test_concat_one_str();
    test_concat_two_str();
    test_concat_three_str();
    test_concat_incorrect_type();
    test_concat_no_params();
    
    test_symbol_name();
    test_symbol_name_incorrect_type();
    test_symbol_name_null();
    
    test_string_size();
    test_string_size_null();
    test_string_size_not_string();
    
    test_str_char();
    test_str_char_null();
    test_str_char_second_not_number();
    test_str_char_first_not_string();
    test_str_char_invalid_index();
    
    test_code_char();
    test_code_char_null();
    test_code_char_not_number();
    
    test_subseq_invalid_args();
    test_subseq_negative_index();
    test_subseq_negative_end_index();
    test_subseq_invalid_index_range();
    test_subseq_start_greater_than_end();
    test_subseq_empty_input();
    test_subseq();
    test_subseq_full_string();
    test_subseq_empty_substring();
    
    test_int_to_str_null();
    test_int_to_str_invalid_arg();
    test_int_to_str_positive();
    test_int_to_str_negative();
    
    return 0;
}
