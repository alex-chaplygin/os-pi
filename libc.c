/**
 * @file libc.c
 * @brief Реализация функций стандартной библиотеки
 * @version 0.1
 * @date 2020-10-26
 * 
 * @copyright Copyright (c) 2020
 * 
 */


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
 * @brief НЕ РАБОТАЕТ Переводит положительное 
// целое число в шестнадцатиричную строку типа 0 = 0x0
 * 
 * @param num Положительное целое число от 0 до 2147483647
 * @return char* Указатель на первый символ строки
 */
char * int_to_str_hex(int num) {
    if ((num < 0) || (num > 2147483647)) return 0; // Проверка на исключение

    int numLength = 1;
    int numLengthBuffer = num;
    while (numLengthBuffer > 9) { // Вычисление символьной длины int числа
        numLength++;
        numLengthBuffer /= 10;
    }
    
    char * outbuf = malloc(numLength); // Выделение памяти для возвращаемого значения
    outbuf[0] = '0'; // Добавление HEX префикса C
    outbuf[1] = 'x';
    int base = 16; // Возвращаемая система счисления
    int i = 12;
    int j = 2;

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
char* intToStr(int n)
{
    char* c;
    c = (char *)malloc(10 * sizeof(char)); 
    for (int i = 0; i < 11; i++)
    {
        c[i] = 0;
    }
    
    int v = 0; //количество цифр в числе n
    //разбиваем на отдельные символы число n
    while (n > 9)
    {
        c[v++] = (n % 10) + '0';
        n = n / 10;
    }
    c[v++] = n + '0';
    c[v] = '\0';
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
