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
    char size; // количество цифр в числе
    char sign; // знак числа
    char* data; //указатель на массив цифр числа
    char exponent; // позиция плавающей точки, начиная справа
} *bignum_t;

/**
 * @brief Инициализация структуры вещественного числа
 *
 * @param Входное число size
 *
 * @return struct bignum_t bignum
 */
bignum_t new_bignum(char size)
{
    bignum_t bignum = (bignum_t) malloc(sizeof(struct bign));
    bignum->size = size;
    bignum->sign = 0;
    bignum->data = (char* )malloc(size * sizeof(char));
    for(int i = 0; i < size; i++) bignum->data[i] = 0;
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
void set_digit(bignum_t bignum, char pos, char val)
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
void set_exp(bignum_t bignum, char exponent)
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
void set_sign(bignum_t bignum, char sign)
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
    if (bignum->sign == -1) putchar('-');
    for(int i = 0; i < bignum->size - bignum->exponent; i++) printf("%d", bignum->data[i]);
    if (bignum->exponent != 0) putchar('.');
    for(int i = bignum->size - bignum->exponent; i < bignum->size; i++) printf("%d", bignum->data[i]);
}

/**
 * @brief Печать вещественного числа (Старая версия)
 *  sign(1) exp(8) mant(23)
 * 
 * @param n Входное число
 */
void print_float_num(float num)
{
    int n = *(int *)&num;
    int s = n >> 31; // знак
    int e = ((n >> 23) & 0xff) - 127; // порядок
    int m = n & 0x7fffff; // мантисса
    int num_bits = 23 - e; // число используемых (значащих) бит в мантиссе
    printf("%d\n",num_bits);
    int int_part = (m + (1 << 23)) >> num_bits; // целая часть числа
    int float_part = (m + (1 << 23)) & (1 << num_bits) - 1; // вещественная часть в степенях 2
    //printf("%d\n",float_part);
    long long float_res = 0; // полученная вещественная часть * 10^num_bits
    long long power2 = 5;
    int n1=0;
    printf("S = %d E = %d num = %d, float = %x\n", s, e, int_part, float_part);
    while (float_res < 1000000000000000000 && float_part != 0) {
        printf("power2 = %lld n = %d %lld\n", power2, float_part, float_res);
        if(float_res==0) n1++;
	    float_res += power2 * ((float_part >> (num_bits - 1)) & 1);
	    float_res *= 10;
	    power2 = power2 * 5;
	    float_part &= (1 << num_bits - 1) - 1;
	    //printf("%d\n",(1 << num_bits - 1) - 1);
	    num_bits--;
    }
    printf("float_res = %lld\n", float_res);
    if (s == -1){
        printf("-");
    }
    
    printf("%d.%lld", int_part, float_res);
    printf("\n%d",n1);
    
}
int main()
{
    //float a = 1.0000012345f;
    bignum_t n = new_bignum(20); // {sign: -1, data:[1,0,0,1,2], exp:2}
    set_digit(n, 1, 9);
    set_digit(n, 4, 1);
    set_digit(n, 17, 7);
    set_exp(n, 12);
    set_sign(n, -1);
    //print_float_num(a);
    //printf("\n%f", a);
    printf("%d %d %d\n",n->sign, n->size, n->exponent);
    //for(int i=0; i<n.size; i++) printf("%d ", n.data[i]);
    print_num(n);
    

    return 0;
}
