void test_str_copy()
{
    char a[] = "fdfd";
    char b[] = "sdadas";
    str_copy(a, b);
    ASSERT(1, compare_str("fdfd", a))
}

void test_compare_str(char *str, char *str2, int res)
{
  ASSERT(res, compare_str(str, str2));
}

int main()
{
  test_str_copy();
  test_compare_str("abc", "abc", 1);
  test_compare_str("abc", "abc1", 0); 
}
