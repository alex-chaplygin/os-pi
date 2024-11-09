#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpa.h"
#include "objects.h"
#include "lexer.h"
#include "parser.h"

/// Все большие числа
static struct bign bignums[MAX_BIGNUMS];
/// Индекс последнего большого числа 
static int last_bignum = 0;
/// Список свободных больших чисел
bignum_t free_bignums = NULL;

/**
 * @brief Инициализация структуры большого числа
 *
 * @return struct bignum_t bignum
 */
bignum_t new_bignum()
{
    bignum_t number;
    if (last_bignum >= MAX_BIGNUMS) {
	if (free_bignums == NULL)
	    error("Error: out of memory: bignumbers");
	number = free_bignums;
	free_bignums = free_bignums -> next;
    } else
	number = &bignums[last_bignum++];
    return number;
}

/**
 * Освобождение памяти большого числа
 *
 * @param o объект для освобождения
 */
void free_bignum(bignum_t o)
{
    if (o == NULL)
	error("free_bignumber: null pointer: obj");
    if (o->free)
	return;
    o->next = free_bignums;
    free_bignums = o;
    o->free = 1;
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
    char c;
    bignum->exponent = 0;
    if (str[0] == '-') {
	bignum->sign = -1;
        str++;
        size--;     
    } else
	bignum->sign = 1;
    for (int i = 0; i < size; i++)
    {
	c = str[size - i - 1];
	if (isdigit(c))
	    bignum->data[i] = c - '0';
	else{
	    if (c == '.')
		bignum->exponent = -i;
	    else
		error("new_bignum_from_str: not digit");}
    }
    bignum->size = size;
    return bignum;    
}

/**
 * @brief конвертирует число int в большое число
 * 
 * @param n - большое число 
 * @param num -целое число
 *
 * @return объект большое число 
 */
bignum_t bignum_from_int(int num)
{
    bignum_t bignum = new_bignum();
    int size = 0;
    if (num < 0) {
         bignum->sign = -1;
         num = -num;
    } else if (num == 0)
	size = 1;
    while (num != 0) {
	bignum->data[size] = num % 10;
	num = num / 10;
	size++;
    }
    bignum->size = size;
    return bignum;
}

/**
 * @brief Печать структуры вещественного числа
 * 
 * @param bignum - входное число
 * 
 */
void print_bignum(bignum_t bignum)
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
 * @brief Сложение больших чисел. Перезаписывает 1-е большое число, прибавляя к нему 2-е
 * 
 * @param n1 - большое 1-е число 
 * @param n2 - большое 2-е число 
 */
void bignum_sum(bignum_t n1, bignum_t n2) 
{
    if (n1->size < n2->size)
        n1->size = n2->size;
    else if (n2->size < n1->size)
	n2->size = n1->size;
    if (n1->sign != n2->sign) {
	n1->sign = 1;
	n2->sign = 1;
	bignum_sub(n1, n2);
	return;
    } 
    int carry = 0;
    for (int i = 0; i < n2->size; i++)
    {
        int sum = n1->data[i] + n2->data[i] + carry;
        n1->data[i] = sum % 10;
        carry = sum / 10;
    }
    if (carry)
    n1->data[n1->size++] = 1;
}

/**
 * @brief Умножение больших чисел. Перезаписывает 1-е большое число, умножая его на 2-е
 * 
 * @param n1 - большое 1-е число
 * @param n2 - большое 2-е число
 */
void bignum_mult(bignum_t n1, bignum_t n2)
{
    bignum_t n3 = new_bignum(); //промежуточная сумма
    bignum_t n4 = new_bignum(); //частичное произведение
    int carry = 0;
    
    for(int i = 0; i < n2->size; i++) {	
	for(int k = 0; k < n4->size; k++)
	    n4->data[k] = 0;
	for(int j = 0; j < n1->size; j++) {
	    int mult = n1->data[j] * n2->data[i] + carry;
	    n4->data[j + i] = mult % 10;
	    carry = mult / 10;
	}
	n4->size = n1->size + i;
	if (carry) {
	    n4->data[n4->size++] = carry;
	    carry = 0;
	}
	bignum_sum(n3, n4);
    }
    if ((n1->size == 1 && n1->data[0] == 0) ||
	(n2->size == 1 && n2->data[0] == 0)) // проверка на то, является ли n1 или n2 нулём
	n1->size = 1;
    else
	n1->size = n3->size;
    for(int i = 0; i < n3->size; i++)
	n1->data[i] = n3->data[i];
    free_bignum(n3);
    free_bignum(n4);
}

/**
 * @brief перезаписывает 1-е большое число, вычитая из него 2-е
 * 
 * @param n1 - большое 1-е число 
 * @param n2 - большое 2-е число 
 */
void bignum_sub(bignum_t n1, bignum_t n2)
{
    /*if (n1->size < n2->size ||
        (n1->size == n2->size && n1->data[n1->size - 1] < n2->data[n2->size - 1])) {
	n1->size = n2->size;
	n1->sign = -1;
	}*/
    if (bignum_compare(n1, n2) == -1){
	bignum_t temp = new_bignum();
	temp = n1;
	n1 = n2;
	n2 = temp;
	n1->sign = -1;
	free_bignum(temp);
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
}

/**
 * @brief сравнивает два больших числа
 * 
 * @param n1 - большое 1-е число 
 * @param n2 - большое 2-е число 
 */
int bignum_compare(bignum_t n1, bignum_t n2)
{
    if (n1->size != n2->size)
        return (n1->size > n2->size) ? 1 : -1;
    for (int i = n1->size - 1; i >= 0; i--)
        if (n1->data[i] != n2->data[i])
            return (n1->data[i] > n2->data[i]) ? 1 : -1;
    return 0;
}

/**
 * @brief деление больших чисел, перезаписывает 1-е число, деля его на 2-е
 * 
 * @param n1 - большое 1-е число 
 * @param n2 - большое 2-е число 
 */
void bignum_div(bignum_t n1, bignum_t n2)
{
    if (n2->size == 1 && n2->data[0] == 0)
        error("bignum_div: division by zero");
    if (bignum_compare(n1, n2) == -1) {
        n1->size = 1;
	n1->data[0] = 0;
        return;
    }
    bignum_t temp = new_bignum();
    bignum_t one = new_bignum();
    temp->size = n1->size;
    one->size = 1;
    one->data[0] = 1;
    for (int i = 0; i < n1->size; i++) {
        temp->data[i] = n1->data[i];
        n1->data[i] = 0;
    }
    n1->size = 1;
    while (bignum_compare(temp, n2) >= 0) {
        bignum_sub(temp, n2);
        bignum_sum(n1, one);
    }
    free_bignum(temp);
    free_bignum(one);
}
