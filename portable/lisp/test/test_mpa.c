#include <stdio.h>
#include "mpa.h"
#include "test.h"

void test_bignum_sum(const char* num1, const char* num2, const char* expected_result, int r)
{
    printf("test_bignum_sum: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    int res = bignum_sum(bignum1, bignum2);
    ASSERT(res, r);
    if (r >= 0) {
	ASSERT(bignum1->size, result_bignum->size);
	ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    }
}


int main()
{
    test_bignum_sum("10220100434343002222", "1022010043434300222", "11242110477777302444", 0);
    test_bignum_sum("0", "0", "0", 0);
    test_bignum_sum("10220100434343002222", "0", "10220100434343002222", 0);
    test_bignum_sum("0", "10220100434343002222", "10220100434343002222", 0);

    /*
      Условия   Правильные классы    Неправильные классы
      n1 > 0        n1 > 0
      n1 = 0        n1 = 0
      n1 < 0        n1 < 0
      n2 > 0        n2 > 0
      n2 = 0        n2 = 0
      n2 < 0        n2 < 0
      n1 содержит     n1 = "432"       n1 = "3443b3"
      только цифры
      n2 содержит     n2 = "4444"      n2 = "12b"
      только цифры
     
      Тесты      
*/
    test_bignum_sum("-10", "10", "0", 0);
    test_bignum_sum("20", "-10", "10", 0);
    test_bignum_sum("2a0", "10", "", -2);
    test_bignum_sum("20", "10z", "", -2);
    return 0;
}
