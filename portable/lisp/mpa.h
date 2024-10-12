#ifndef BIGNUM_H
#define BIGNUM_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BIGNUM_SIZE 100
#define MAX_BIGNUMS 100

/// Структура большого числа
typedef struct bign
{
    int size; /// Число десятичных цифр числа
    int sign; /// Знак числа
    char data[MAX_BIGNUM_SIZE]; /// Цифры числа, начиная с младшей цифры
    int exponent;
} *bignum_t;

bignum_t new_bignum();
bignum_t new_bignum_from_str(const char* str);
void set_digit(bignum_t bignum, int pos, char val);
void set_exp(bignum_t bignum, int exponent);
void set_sign(bignum_t bignum, int sign);
void print_num(bignum_t bignum);
void print_bignum(bignum_t num);
void bignum_from_int(bignum_t n, int num);
int bignum_sum(bignum_t n1, bignum_t n2);
int bignum_sub(bignum_t n1, bignum_t n2);
int bignum_mult(bignum_t n1, bignum_t n2);

#endif 
