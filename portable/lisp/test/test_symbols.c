#include <stdio.h>
#include "test.h"
#include "symbols.h"

void str_copy (char *str1, char *str2);
int compare_str(char *str1, char *str2);


void test_str_copy()
{
    char a[] = "fdfd";
    char b[] = "sdadas";
    printf("test_str_copy");
    str_copy(a, b);
    ASSERT(1, compare_str("fdfd", a))
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
   ASSERT(i, k)
}



int main()
{
  test_str_copy();
  test_compare_str("abc", "abc", 1);
  test_compare_str("abc", "abc1", 0);
  test_find_same_symbol();
}
