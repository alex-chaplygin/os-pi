void memcpy(unsigned char* destptr, unsigned char* srcptr, int num ){
    for (int i = 0; i < num; i++)
    {
        destptr[i] = srcptr[i];
    }
    
}

//*************************************************************
// Функция char * int_to_str_hex(int num) переводит положительное 
// целое число в шестнадцатиричную строку типа 0 = 0x0.
// int num = положительное целое число от 0 до 2147483647
//*************************************************************
char * int_to_str_hex(int num) {
    if ((num < 0) || (num > 2147483647)) return NULL; // Проверка на исключение

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
