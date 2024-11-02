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
    char str[MANTISSA_BITS + 2] = "100000000000000000000000";
    bignum_t sum;  //сумма значений всех битов
    bignum_t bit;  // значение бита мантиссы
    bignum_t power = new_bignum_from_str("1"); // степень 5
    bignum_t five = new_bignum_from_str("5");
    sum = new_bignum_from_str(str); 
    bit = new_bignum_from_str(str);
    //printf("bit = ");
    //print_bignum(bit);
    //printf("\ntwo = ");
    //print_bignum(two);
    //printf("\n");
    for (int i = 0; i < MANTISSA_BITS; ++i) {
	bignum_mult(power, five);	
	if ((m >> (MANTISSA_BITS - i - 1) & 1) == 1) {
	    for (int j = 0; j < i + 2; ++j)
		bit->data[bit->size - i - 2 + j] = 0;
	    for (int j = 0; j < power->size; ++j)
		bit->data[bit->size - i - 2 + j] = power->data[j];
	    bignum_sum(sum, bit);
	}
    }
    print_bignum(sum);
    free_bignum(sum);
    free_bignum(bit);
    free_bignum(power);
    free_bignum(five);
}
