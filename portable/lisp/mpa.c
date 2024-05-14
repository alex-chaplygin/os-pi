#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BIGNUM_SIZE 100
#define MAX_BIGNUMS 100

/// Структура вещественного числа, запсисанного в виде символов
typedef struct bign
{
    int size; // количество цифр в числе
    int sign; // знак числа
    char data[MAX_BIGNUM_SIZE]; //указатель на массив цифр числа от младшей к старшей 
    int exponent; // позиция плавающей точки, начиная справа
} *bignum_t;

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
    if (bignum->sign == 1) 
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
 * @brief перезаписывает 1-е большое число, прибавляя к нему 2-е
 * 
 * @param n1 - большое 1-е число 
 * @param n2 - большое 2-е число 
 */
int bignum_sum(bignum_t n1, bignum_t n2) 
{
    if (n1->size < n2->size) {
        printf("Args error");
        return -1;
    }
    int carry = 0;
    for (int i = 0; i < n2->size; i++) {
        int sum = n1->data[i] + n2->data[i] + carry;
        n1->data[i] = sum % 10;
        carry = sum >= 10;
    }
    if (carry) {
        printf("Индекс за пределами массива!\n");
        return -1;
    }
    return 0;
}

int main()
{
    bignum_t bignum1 = new_bignum_from_str("-10220100434343002222");
    bignum_t bignum2 = new_bignum_from_str("10220100434343002222");
    bignum_t bignum3 = new_bignum_from_str("0");

    if (bignum1 && bignum2 && bignum3) {
	printf("One num: ");
	print_num(bignum1);
	printf("\n");

	printf("Two num: ");
	print_num(bignum2);
	printf("\n");

	printf("Zero: ");
	print_num(bignum3);
    }
    return 0;
}
