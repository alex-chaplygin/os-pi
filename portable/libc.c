/**
 * @file libc.c
 * @brief Реализация функций стандартной библиотеки
 * @version 0.1
 * @date 2020-10-26
 * 
 * @copyright Copyright (c) 2020
 * 
 */

#include <portable/libc.h>
#include <x86/console.h>
#include <mpa.h>
#include "float.h"

char outbuf[50]; // память для возвращаемого значения

/**
 * @brief Копирует блок памяти
 * 
 * @param destptr Адрес куда
 * @param srcptr Адрес откуда
 * @param size Количество копируемых байт
 */

void memcpy(void* destptr, const void* srcptr, unsigned int size)
{
    const unsigned char *source = (const unsigned char *)srcptr;
    unsigned char *destination = (unsigned char *)destptr;
    unsigned int i;

    for (i = 0; i < size / 4; ++i) {
	*((int *)destination) = *((const int *)source);
	destination += 4;
	source += 4;
    }
    for (i = 0; i < size % 4; ++i)
	*destination++ = *source++;
}

/**
 * @brief Переводит положительное 
// целое число в шестнадцатиричную строку типа 15 = f
 * 
 * @param num Положительное беззнаковое целое число от 0 до 4294967295
 * @return char* Указатель на первый символ строки
 */
void print_hex_num(unsigned int num)
{
    char hex[10];
    int i = 0;
    
    if (num == 0) { // если число равно 0, то выводит 0
        hex[0] = '0';
        i++;
    } else {
        while (num != 0) {
            if (num % 16 < 10)
                hex[i] = num % 16 + '0';
            else
                hex[i] = num % 16 - 10 + 'A';
            num = num / 16; 
            i++; 
        }
    }
    hex[i] = '\0'; // добавление нулевого символа в конце массива
    for (int j = i - 1; j >= 0; j--)
        putchar(hex[j]); // печать шестнадцатеричного числа в обратном порядке
}

/**
 * @brief Заполняет нулями указанную область памяти.
 * 
 * @param buffer указатель на область памяти для очистки.
 * @param offset смещение.
 * @param count количество байтов, которые необходимо установить в 0.
 */
void clear_buffer(byte* buffer, int offset, int count)
{
    for (int i = 0; i < count; i++)
        buffer[offset + i] = 0;
}

void memset(void *buf, int val, int count)
{
    byte *p = (byte *)buf;
    for (int i = 0; i < count; i++)
	*p++ = (byte)val;
}

/** 
 * @brief Переводит шестнадцатиричное число в строке в целое беззнаковое число 
 * 
 * @param s Строка с hex-числом
 * 
 * @return hex-число
 */
unsigned int str_hex_to_int(char* s)
{
    char hexdigitschar[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

    int hexdigitsint[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };

    int number = 0, digit = 0;

    int tmp;

    for (int i = 0; s[i] != '\0'; i++)
        digit++;

    for (int i = 0; s[i] != '\0'; i++)
        for (int j = 0; j < 16; j++)
            if (s[i] == hexdigitschar[j])
            {
                tmp = 1;
                for (int k = 0; k < digit; k++)
                    if (k != digit - 1)
                        tmp *= 16;

                number += hexdigitsint[j] * tmp;
                digit--;
                break;
            }

    return number;
}

/**
 * @brief Перевод числа в строку
 *
 * @param Входное число num
 * @param Входной массив символов str
 *
 * @return char p
 */
char *itoa(int num, char *str, int rad)
{
    int i = 14;
    int neg = 0;
    str[i - 1] = 0;
    char *p = &str[i - 1];
    if (num == 0)
	    *--p = '0';
    if (num < 0) {
        neg = 1;
        num *= -1;
    }
    while (num > 0) {
        int currchar = num % rad;
        p--;
        *p = '0' + currchar;
        num = num / rad;
    }
    if (neg)
        *--p = '-';
    return p;
}

/**
 * @brief Печать целого числа
 * 
 * @param n Входное число
 */
void print_num(int num)
{
    char a[14];
    char *s = itoa(num, a, 10);
    printf(s);
}

/**
 * @brief Посимвольный анализ строки для печати типа printf - вывод значения многих переменных
 * %i - вывод целого числа
 * %d - вывод десятичного числа
 * %x - вывод шестнадцатеричного числа
 * %s - вывод строки 
 * @param *format указатель на форматную строку
 * @param args вариативные аргументы
 *
 */
void vprintf(char *format, va_list args)
{
    for (int i = 0; format[i] != '\0'; i++) {
        char symbol = format[i];
        char next_symbol = format[i + 1];
        if (symbol == '%' && next_symbol == 'i') {
            print_num(va_arg(args, int));
            i++;
        } else if (symbol == '%' && next_symbol == 'd') {
            print_num(va_arg(args, int));
            i++;
        } else if (symbol == '%' && next_symbol == 'f') {
            print_double_num(va_arg(args, double));
            i++;
        } else if (symbol == '%' && next_symbol == 'x') {
            print_hex_num(va_arg(args, unsigned int));
            i++;
        } else if (symbol == '%' && next_symbol == 's') {
            vprintf(va_arg(args, char *), args);
            i++;
        } else if (symbol == '%' && next_symbol == 'c') {
            putchar(va_arg(args, int));
            i++;
        } else
            putchar(symbol);
    }
}

void printf(char *str, ...)
{
    va_list args;
    va_start(args, str);
    vprintf(str, args);
    va_end(args);
}

void puts(char *str)
{
    printf("%s", str);
}

/**
 * @brief Сравнивает строки
 * @param str_one первая строка
 * @param str_two вторая строка
 * @return если строки равны, возвращает 0 и -1 если не равны
 * 
 */
int strcmp(char *str1, char *str2)
{
    while (*str1 && *str2)
    {
        if (*str1++ != *str2++)
	    return 1;
    }
    
    return *str1 != *str2;
}

// копирование строки str1 в строку str2
void strcpy(char *str1, char *str2)
{
    //int i = 0; //"abc" ['a', 'b', 'c', 0]
    while (*str2)
        *str1++ = *str2++;
    *str1 = 0;
}

// длина строки
int strlen(char *str)
{
    int c = 0;
    while (*str++)
	c++;
    return c;
}

/**
 * @brief Соединение строк: к строке str1 добавляется str2
 * @param str1 первая строка "ab"
 * @param str2 вторая строка "cd"
 * 
 */
void strcat(char *str1, char *str2)
{
    while (*str1 != 0)
	str1++;
    while (*str2 != 0)
	*str1++ = *str2++;
    *str1 = 0;
}

/** 
 * Перевод символа в верхний регистр
 */ 
int toupper(int ch)
{
    if (ch >= 'a' && ch <= 'z')
        return ch - 32;
    return ch;
}

// проверка символа на цифру
int isdigit(int c)
{
    return c >= '0' && c <= '9';
}

// проверка символа на букву
int isalpha(int c)
{
    return c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z';
}

