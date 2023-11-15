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
 * @brief Переводит int в нуль-терминируемую "строку"
 * 
 * @param n Входное число
 * @return char* Указатель на первый символ строки
 */
char* int_to_str(int n)
{
    char* c = outbuf;
    int negative = 0;//идентификация отрицательного числа

    if (n < 0)
        negative = 1;

    for (int i = 0; i < 11; i++)
        c[i] = 0;

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
 * @brief Заполняет нулями указанную область памяти.
 * 
 * @param buffer указатель на область памяти для очистки.
 * @param offset смещение.
 * @param count количество байтов, которые необходимо установить в 0.
 */
void clear_buffer(byte* buffer, int offset, int count) {
    for (int i = 0; i < count; i++) {
        buffer[offset + i] = 0;
    }
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
 * @brief Посимвольный анализ строки для печати типа printf - вывод значения многих переменных
 * %i - вывод целого числа
 * %d - вывод десятичного числа
 * %x - вывод шестнадцатеричного числа
 * %s - вывод строки 
 * @param *str указатель на строку
 *
 */
void printf(char *str,...)
{
  int i = 0;
    
  int *param = (int *)&str;
  for (i = 0; str[i]!='\0'; i++) {
      char symbol = str[i];
      char next_symbol = str[i + 1];
 
      if (symbol == '%' && next_symbol == 'i') {
	  char *param_int = int_to_str(*++param);      
	  printf(param_int);
	  i++;
      } else if (symbol=='%' && next_symbol=='d') {
	  char *param_int = int_to_str(*++param);      
	  printf(param_int);
	  i++;
      } else if (symbol=='%' && next_symbol=='x') {
	  print_hex_num(*++param);
	  i++;
      } else if (symbol=='%' && next_symbol=='s') {
	  printf((char *)*++param);
	  i++;
      } else if (symbol=='%' && next_symbol=='c') {
	  putchar((char)*++param);
	  i++;
      } else
	  putchar(symbol);
  }
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
    while (*str1++ != 0);
    while (*str2 != 0)
	*str1++ = *str2++;
    *str1 = 0;
}

/** 
 * Перевод символа в верхний регистр
 */ 
char toupper(char ch)
{
    if (ch >= 'a' && ch <= 'z')
        return ch - 32;
    return ch;
}
