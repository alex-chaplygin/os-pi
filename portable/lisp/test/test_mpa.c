#include <stdio.h>
#include "mpa.h"
#include "test.h"

void test_bignum_sum(const char* num1, const char* num2, const char* expected_result)
{
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    int res = bignum_sum(bignum1, bignum2);
    ASSERT(res, 0);
    

    ASSERT(bignum1->size, result_bignum->size);

    ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
}


int main()
{
    test_bignum_sum("10220100434343002222", "1022010043434300222", "11242110477777302444");
    test_bignum_sum("0", "0", "0");
    test_bignum_sum("10220100434343002222", "0", "10220100434343002222");
    test_bignum_sum("0", "10220100434343002222", "10220100434343002222");

    return 0;
}