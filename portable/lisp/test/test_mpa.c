#include "mpa.h"

int main()
{
    bignum_t bignum1 = new_bignum_from_str("10220100434343002222");
    bignum_t bignum2 = new_bignum_from_str("1022010043434300222");
    bignum_t bignum3 = new_bignum_from_str("0");

    printf("One num: ");
    print_bignum(bignum1);
    printf("\n");

    printf("Two num: ");
    print_bignum(bignum2);
    printf("\n");

    printf("Zero: ");
    print_bignum(bignum3);
    printf("\n");

    int result_sub = bignum_sub(bignum1, bignum2);
    if (result_sub == 0) 
    {
        printf("Вычитание: ");
        print_bignum(bignum1);
        printf("\n");
    }
    else 
    {
        printf("Ошибка при вычитании.\n");
    }

    return 0;
}