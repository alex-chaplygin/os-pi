#include <stdio.h>
#include <string.h>
#include "test.h"
#include "objects.h"
#include "symbols.h"

void str_copy (char *str1, char *str2);
int compare_str(char *str1, char *str2);

symbol_t s;

symbol_t *new_symbol(char *str)
{
    return &s;
}

void test_compare_str(char *str, char *str2, int res)
{
    printf("test_compare_str: ");
    ASSERT(res, compare_str(str, str2));
}

void test_find_same_symbol()
{
    symbol_t *i = find_symbol("f");
    symbol_t *k = find_symbol("f");
    printf("test_find_same_symbol: ");
    ASSERT(i, k);
}

object_t *test(object_t *list)
{
    return NULL;
}

/**
 * зарегистрировать функцию и проверить указатель
 */
void test_register_func()
{
    s.next = NULL;
    printf("test_register_func: ");
    register_func("TEST", test);
    strcpy(find_symbol("TEST")->str,"TEST");
    ASSERT(find_symbol("TEST")->func, test); 
}

int main()
{
    printf("--------------test symbols---------------------\n");
    test_compare_str("abc", "abc", 1);
    test_compare_str("abc", "abc1", 0);
    test_find_same_symbol();
    test_register_func();
}
