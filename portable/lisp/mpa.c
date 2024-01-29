/******************************************************************************

                            Online C Compiler.
                Code, Compile, Run and Debug C program online.
Write your code in this editor and press "Run" button to compile and execute it.

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>


/// Структура вещественного числа, запсисанного в виде символов
typedef struct bign
{
    int size; // количество цифр в числе
    int sign; // знак числа
    char *data; //указатель на массив цифр числа от младшей к старшей 
    int exponent; // позиция плавающей точки, начиная справа
} *bignum_t;

/**
 * @brief Инициализация структуры вещественного числа
 *
 * @param Входное число size
 *
 * @return struct bignum_t bignum
 */
bignum_t new_bignum(int size)
{
    bignum_t bignum = (bignum_t)malloc(sizeof(struct bign));
    bignum->size = size;
    bignum->sign = 0;
    bignum->data = (char *)malloc(size);
    for(int i = 0; i < size; i++) 
        bignum->data[i] = 0;
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
    /*for(int i = 0; i < bignum->size - bignum->exponent; i++) 
        if (bignum->data[i]!=-1)
            printf("%d", bignum->data[i]);
    if (bignum->exponent != 0) 
        putchar('.');
    for(int i = bignum->size - bignum->exponent; i < bignum->size; i++)
        if (bignum->data[i]!=-1)
            printf("%d", bignum->data[i]); 
    printf("\n");*/
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
    //float a = 1.0000012345f;
    bignum_t n1 = new_bignum(40);
    bignum_t n2 = new_bignum(4);
    //bignum_t n3 = new_bignum(20);
   /* set_digit(n, 1, 9);
    set_digit(n, 4, 1);
    set_digit(n, 17, 7);
    set_exp(n, 12);
    set_sign(n, -1);*/
    bignum_from_int(n1, 15);
    bignum_from_int(n2, 2865);
    //print_float_num(a);
    //printf("\n%f", a);
    //printf("%d %d %d\n",n->sign, n->size, n->exponent);
    //for(int i=0; i<n.size; i++) printf("%d ", n.data[i]);
    print_num(n1);
    printf("\n");
    print_num(n2);
    printf("\n");
    if (bignum_sum(n1, n2) == 0) {
        printf("Сумма:");
        print_num(n1);
    }
    //print_num(n3);

    return 0;
}