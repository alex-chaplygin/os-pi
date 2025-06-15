#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test.h"
#include "objects.h"
#include "symbols.h"

extern symbol_t *hash_table[HASH_SIZE];

void str_copy (char *str1, char *str2);
unsigned int hash(char *str);

symbol_t s;

void bind_static(object_t symbol)
{
}

symbol_t *new_symbol(char *str)
{
    symbol_t *symbol = (symbol_t*)malloc(sizeof(symbol_t));
    memset(symbol, 0, sizeof(symbol_t));
    strcpy(symbol->str, str);
    return symbol;
}

void test_find_symbol(char *str, char *expected_symbol)
{
    printf("test_find_symbol: ");
    symbol_t *result = find_symbol(str);
    ASSERT(strcmp(result->str, expected_symbol), 0);
}

/** 
 * Тест пустой строки
 */
void test_find_symbol_empty_string()
{
    printf("test_find_symbol_empty_string: ");
    symbol_t *result = find_symbol("");
    ASSERT(result, NULL);
}

/** 
 * Тест строки недопустимой длины
 */
void test_find_symbol_invalid_string_length()
{
    printf("test_find_symbol_invalid_string_length: ");
    char str[MAX_SYM_STR + 2];
    for (int i = 0; i < MAX_SYM_STR + 1; i++)
        str[i] = 'a';
    str[MAX_SYM_STR + 1] = '\0';
    symbol_t *result = find_symbol(str);
    ASSERT(result, NULL);
}

/**
 * Тест строки максимальной длины
 */
void test_find_symbol_max_string_length()
{
    printf("test_find_symbol_max_string_length: ");
     char str[MAX_SYM_STR + 1];
    for (int i = 0; i < MAX_SYM_STR; i++)
        str[i] = 'a';
    str[MAX_SYM_STR] = '\0';
    symbol_t *result = find_symbol(str);
    ASSERT(result, NULL);
}

/** 
 * Тестирование символов с обинаковым хеш значением
 */
void test_same_hash()
{
    printf("test_same_hash:");
    char str1[] = "PJ";
    char str2[] = "452";
    printf("%d\n", hash(str1));
    printf("%d\n", hash(str2));
    ASSERT((find_symbol(str1) != find_symbol(str2)), 1);
}

/**
 * Создать символ "f" с помощью find_symbol
 * Получить символ "f" из таблицы с помощью find_symbol_get
 * Проверить что созданный и полученный символы совпадают
 */
void test_check_symbol()
{
    printf("test_check_symbol: ");
    symbol_t *s1 = check_symbol("f");
    ASSERT(s1, NULL);
    symbol_t *s2 = find_symbol("f");
    symbol_t *s3 = check_symbol("f");
    ASSERT(s2, s3);
}

/** 
 * Тест пустой строки
 */
void test_check_symbol_empty_string()
{
    printf("test_check_symbol_empty_string: ");
    symbol_t *result = check_symbol("");
    ASSERT(result, NULL);
}

/** 
 * Тест строки недопустимой длины
 */
void test_check_symbol_invalid_string_length()
{
    printf("test_check_symbol_invalid_string_length: ");
    char str[MAX_SYM_STR + 2];
    for (int i = 0; i < MAX_SYM_STR + 1; i++)
        str[i] = 'a';
    str[MAX_SYM_STR + 1] = '\0';
    symbol_t *result = check_symbol(str);
    ASSERT(result, NULL);
}

/**
 * Тест строки максимальной длины
 */
void test_check_symbol_max_string_length()
{
    printf("test_check_symbol_max_string_length: ");
    char str[MAX_SYM_STR + 1];
    for (int i = 0; i < MAX_SYM_STR; i++)
        str[i] = 'a';
    str[MAX_SYM_STR] = '\0';
    symbol_t *result = check_symbol(str);
    ASSERT(result, NULL);
}

object_t test(object_t list)
{
    return NULLOBJ;
}

/**
 * зарегистрировать функцию и проверить указатель
 */
void test_register_func()
{
    s.next = NULL;
    printf("test_register_func: ");
    register_func("TEST", test, 0, 1);
    strcpy(find_symbol("TEST")->str,"TEST");
    ASSERT(find_symbol("TEST")->func, test); 
}

/** 
 * Тестирование символов с обинаковым хеш значением
 */
void test_same_hash_three_symbols()
{
    printf("test_same_hash_three_symbols:");
    char str1[] = "PJ";
    char str2[] = "452";
    char str3[] = "\xe4\x44\x8a";
    /*    for (int i = 0; i < 256; i++ ){
        for (int j = 0; j < 256; j++ ){
            for (int k = 0; k < 256; k++){
                str3[0] = i;
                str3[1] = j;
                str3[2] = k;
                str3[3] = 0;    
                if (hash(str3) == hash(str2)){
                    printf("str:");
                    printf("%d %s %d %s %d %x %x %x \n", hash(str1), str1, hash(str2), str2, hash(str2), str3[0], str3[1], str3[2]);
                }
            }
        }
    }*/    
    printf("%d\n", hash(str1));
    printf("%d\n", hash(str2));
    printf("%d\n", hash(str3));
    find_symbol(str1);
    find_symbol(str2);
    find_symbol(str3);
    ASSERT(((find_symbol(str1) != find_symbol(str2)) || (find_symbol(str1) != find_symbol(str3)) || (find_symbol(str2) != find_symbol(str3))), 1);
}

/** 
 * создать цепочку сиволов с одинаковым хэшем  LET -> ARR -> FIELD
 * удалить символ из середины цепочки
 * удалить символ из начала цепочки
 * проверить что данных символов больше нет в цепочке
 */
void test_remove_hash()
{
    printf("test_remove_hash: ");
    symbol_t *s1 = find_symbol("LET");
    symbol_t *s2 = find_symbol("ARR");
    symbol_t *s3 = find_symbol("FIELD");
    ASSERT((int)hash_table[s3->hash_index] != 0, 1)
    hash_remove(s1);
    hash_remove(s2);
    ASSERT((int)hash_table[s3->hash_index] != 0, 1)
    
    for (symbol_t *cur = hash_table[s3->hash_index]; cur != NULL; cur = cur->next) {
       if (s1 == cur)
           FAIL;
       if (s2 == cur)
           FAIL;
    }
}

/*
find_symbol
|условие               |правильный класс                    |неправильный класс  |
|входная строка        |1) длина строки <= 80 && > 0        |2) длина строки = 0 |
|                      |                                    |3) длина строки > 80|
|кол-во вызовов функции|4) два вызова с одинаковыми строками|                    |
|                      |5) два вызова с разными строками    |                    |

check_symbol
|условие               |правильный класс                    |неправильный класс  |
|входная строка        |6) длина строки <= 80 && > 0        |7) длина строки = 0 |
|                      |                                    |8) длина строки > 80|
|вызов функции         |9) символ есть в таблице            |                    |
|                      |10) символа нет в таблице           |                    |
*/
int main()
{
    printf("--------------test symbols---------------------\n");
    test_find_symbol("a", "a"); // 1, 4
    test_find_symbol("ab", "ab"); // 1, 4
    test_find_symbol_empty_string(); // 2
    test_find_symbol_invalid_string_length(); // 3
    test_find_symbol_max_string_length(); // граничный тест строки максимально допустимой длины
    test_check_symbol_empty_string(); // 7
    test_check_symbol_invalid_string_length(); // 8
    test_check_symbol_max_string_length(); // граничный тест строки максимально допустимой длины
    test_register_func();
    test_check_symbol(); // 6, 9, 10
    test_register_func();
    test_same_hash();
    test_same_hash_three_symbols();
    test_remove_hash();
}
