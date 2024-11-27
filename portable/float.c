#include <stdio.h>
#include "mpa.h"
#include "float.h"

/**
 * @brief Печать вещественного числа
 *  sign(1) exp(8) mant(23)
 * 
 * @param n Входное число
 */
void print_float_num(float num)
{
    int n = *(int *)&num;
    int s = n >> 31; // знак
    int e = ((n >> MANTISSA_BITS) & 0xff) - 127; // порядок
    int m = n & 0x7fffff; // мантисса
    bignum_t sum = bignum_from_int(1); //сумма значений всех битов
    bignum_t bit = new_bignum_from_str("0.5"); // значение бита мантиссы
    bignum_t five = new_bignum_from_str("0.5");
    bignum_t degree;
    if (e >= 0)
	degree = bignum_from_int(1 << e);
    else {
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
    if (s != 0)
	putchar('-');
    round_bignum(sum, 6); // округление до 6 знаков после .
    print_bignum(sum);
    free_bignum(sum);
    free_bignum(bit);
    free_bignum(five);
    free_bignum(degree);
}
