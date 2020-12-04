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

/**
 * @brief Копирует блок памяти
 * 
 * @param destptr Адрес откуда
 * @param srcptr Адрес куда
 * @param num Количество копируемых байт
 */
void memcpy(unsigned char* destptr, unsigned char* srcptr, int num ){
    for (int i = 0; i < num; i++)
    {
        destptr[i] = srcptr[i];
    }    
}

/**
 * @brief Переводит положительное 
// целое число в шестнадцатиричную строку типа 15 = f
 * 
 * @param num Положительное беззнаковое целое число от 0 до 4294967295
 * @return char* Указатель на первый символ строки
 */
char * int_to_str_hex(unsigned int num) {
    if ((num < 0) || (num > 4294967295)) return 0; // Проверка на исключение

    int numLength = 1;
    int numLengthBuffer = num;
    while (numLengthBuffer > 9) { // Вычисление символьной длины int числа
        numLength++;
        numLengthBuffer /= 10;
    }
    
    char * outbuf = malloc(numLength); // Выделение памяти для возвращаемого значения
    int base = 16; // Возвращаемая система счисления
    int i = 12;
    int j = 0;

    do {
        outbuf[i] = "0123456789abcdef" [num % base];
        i--;
        num = num / base;
    } while (num > 0);

    while (++i < 13) {
        outbuf[j++] = outbuf[i];
    }

    outbuf[j] = '\0'; // Добавление нуль терминального символа строки в конец char массива
    return outbuf;
}

/**
 * @brief Переводит int в нуль-терминируемую "строку"
 * 
 * @param n Входное число
 * @return char* Указатель на первый символ строки
 */
char* int_to_str(int n)
{
    char* c;
    int negative = 0;//идентификация отрицательного числа

    if (n < 0)
    {
        c = (char*)malloc(11 * sizeof(char));
        negative = 1;
    }
    else
        c = (char*)malloc(10 * sizeof(char));

    for (int i = 0; i < 11; i++)
    {
        c[i] = 0;
    }

    int v = 0;//количество цифр в числе n
    //разбиваем на отдельные символы число n

    if (n < 0)
    {
        n = n * (-1);
    }

    while (n > 9)
    {
        c[v++] = (n % 10) + '0';
        n = n / 10;
    }

    if (negative == 1)
    {
        c[v++] = n + '0';
        c[v++] = '-';
        c[v] = '\0';
    }
    else
    {
        c[v++] = n + '0';
        c[v] = '\0';
    }
    char t;
    //инвертируем массив символов
    for (int i = 0; i < v / 2; i++)
    {
        t = c[i];
        c[i] = c[v - 1 - i];
        c[v - 1 - i] = t;
    }
    v = 0;

    //while (c[v] != '\0')
     //   printf("%c", c[v++]);

    //free(c);
    return c;
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
