#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpa.h"

static struct bign bignums[MAX_BIGNUMS];
static int last_bignum = 0;

/**
 * @brief Инициализация структуры вещественного числа
 *
 * @param Входное число size
 *
 * @return struct bignum_t bignum
 */
bignum_t new_bignum()
{
    if (last_bignum >= MAX_BIGNUMS) {
        printf("MAX bignum");
        return NULL;
    }
    return &bignums[last_bignum++];
}

/** 
 * Создать большое число из строки
 *
 * @param str строка числа
 *
 * @return объект большое число
 */
bignum_t new_bignum_from_str(const char *str)
{
    bignum_t bignum = new_bignum();
    int size = strlen(str);
    if (str[0] == '-') {
	bignum->sign = -1;
        str++;
        size--;     
    } else
	bignum->sign = 1;
    for (int i = 0; i < size; i++)
        bignum->data[i] = str[size - i - 1] - '0';
    bignum->size = size;
    bignum->exponent = 0;
    return bignum;    
}

/**
 * @brief Изменение значения цифры вещественного числа
 *
 * @param Входная стуктура bignum_t
 * @param Входное число pos - позиция цифры
 * @param Входное число val - значение цифры
 *
 */
void set_digit(bignum_t bignum, int pos, char val)
{
    bignum->data[pos] = val;
}

/**
 * @brief Изменение позиции плавающей точки вещественного числа
 *
 * @param Входная стуктура bignum_t
 * @param Входное число exponent - позиция плавающей точки
 *
 */
void set_exp(bignum_t bignum, int exponent)
{
    bignum->exponent = exponent;
}

/**
 * @brief Изменение знака вещественного числа
 *
 * @param Входная стуктура bignum_t
 * @param Входное число sign - знак числа
 *
 */
void set_sign(bignum_t bignum, int sign)
{
    bignum->sign = sign;
}

/**
 * @brief Печать структуры вещественного числа
 * 
 * @param bignum - входное число
 * 
 */
void print_num(bignum_t bignum)
{
    if (bignum->sign == -1) 
        putchar('-');
    int i = bignum->size - 1;
    while (bignum->data[i] == 0 && i >= 0)
        i--;
    if (i == -1)
        printf("0");
    else
        while (i >= 0) {
            printf("%d", bignum->data[i]);
            i--;
        }
}



/**
 * @brief конвертирует число int в большое число
 * 
 * @param n - большое число 
 * @param num -целое число 
 */
void bignum_from_int(bignum_t n, int num)
{
    if (num < 0) {
         n->sign = 1;
         num = -num;
    }
    for(int i = 0; i < n->size; i++) {   
        n->data[i] = num % 10;
        num = num / 10;
        if (num == 0)
            break;
    }
}

/**
 * @brief Сложение больших чисел. Перезаписывает 1-е большое число, прибавляя к нему 2-е
 * 
 * @param n1 - большое 1-е число 
 * @param n2 - большое 2-е число 
 */
int bignum_sum(bignum_t n1, bignum_t n2) 
{
    if (n1->size < n2->size)
        n1->size = n2->size;
    int carry = 0;
    for (int i = 0; i < n2->size; i++)
    {
        int sum = n1->data[i] + n2->data[i] + carry;
        n1->data[i] = sum % 10;
        carry = sum / 10;
    }
    if (carry)
	n1->data[n1->size++] = 1;    
    return 0;
}

/**
 * @brief Умножение больших чисел. Перезаписывает 1-е большое число, умножая его на 2-е
 * 
 * @param n1 - большое 1-е число
 * @param n2 - большое 2-е число
 */
int bignum_mult(bignum_t n1, bignum_t n2)
{
    bignum_t n3 = new_bignum(); //промежуточная сумма
    bignum_t n4 = new_bignum(); //частичное произведение
    int carry = 0;
    
    for(int i = 0; i < n2->size; i++)
    {	
	for(int k = 0; k < n4->size; k++)
	    n4->data[k] = 0;
	for(int j = 0; j < n1->size; j++) {
	    int mult = n1->data[j] * n2->data[i] + carry;
	    n4->data[j + i] = mult % 10;
	    carry = mult / 10;
	}
	n4->size = n1->size + i;
	if (carry){
	    n4->data[n4->size++] = carry;
	    carry = 0;
	}
	bignum_sum(n3, n4);
    }
    n1->size = n3->size;
    for(int i = 0; i < n3->size; i++){
	n1->data[i] = n3->data[i];
    }
    return 0;
}

/**
 * @brief перезаписывает 1-е большое число, вычитая из него 2-е
 * 
 * @param n1 - большое 1-е число 
 * @param n2 - большое 2-е число 
 */
int bignum_sub(bignum_t n1, bignum_t n2)
{
    if (n1->size < n2->size ||
        (n1->size == n2->size && n1->data[n1->size - 1] < n2->data[n2->size - 1])) {
        printf("Args error: n1 < n2\n");
        return -1;
    }
    int borrow = 0;
    for (int i = 0; i < n1->size; i++) {
        int sub = n1->data[i] - (i < n2->size ? n2->data[i] : 0) - borrow;
        if (sub < 0) {
            sub += 10;
            borrow = 1;
        } else
            borrow = 0;
        n1->data[i] = sub;
    }
    while (n1->size > 0 && n1->data[n1->size - 1] == 0)
        n1->size--;
    if (n1->size == 0) {
        n1->size = 1;
        n1->data[0] = 0;
    }
    return 0;
}
