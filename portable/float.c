#include <stdio.h>
#include "mpa.h"
#include "float.h"

/**
 * @brief Печать вещественного числа
 *  sign(1) exp(8) mant(23)
 * 
 * @param n Входное число
 */
void print_double_num(double num)
{
    long long n = *(long long *)&num;
    int s = n >> 63;
    int e = ((n >> MANTISSA_BITS) & 0x7ff) - DOUBLE_EXP_BIAS;
    unsigned long long m = n & 0xfffffffffffff;
    bignum_t sum = bignum_from_int(1);
    bignum_t bit = new_bignum_from_str("0.5");
    bignum_t five = new_bignum_from_str("0.5");
    bignum_t degree;
    if (s != 0)
        putchar('-');
    if (e == -DOUBLE_EXP_BIAS) {
        printf("0.000000");
        return;
    } else if (e == (0x7ff - DOUBLE_EXP_BIAS) && m == 0) {
        printf("inf");
        return;
    } else if (e == (0x7ff - DOUBLE_EXP_BIAS) && m != 0) {
        printf("nan");
        return;
    }
    if (e >= 0) {
        degree = bignum_from_int(1);
        bignum_t two = bignum_from_int(2);
        for(int i = 0; i < e; i++) {
            bignum_mult(degree, two);
        }
        free_bignum(two);
    } else {
        degree = bignum_from_int(1);
        for (int i = 0; i < -e; i++)
            bignum_mult(degree, five);
    }
    for (int i = 0; i < MANTISSA_BITS; ++i) {
        if ((m >> (MANTISSA_BITS - i - 1) & 1) == 1)
            bignum_sum(sum, bit);
        bignum_mult(bit, five);
    }
    bignum_mult(sum, degree);
    round_bignum(sum, 15);
    print_bignum(sum);
    free_bignum(sum);
    free_bignum(bit);
    free_bignum(five);
    free_bignum(degree);
}
