void memcpy(unsigned char* destptr, unsigned char* srcptr, int num ){
    for (int i = 0; i < num; i++)
    {
        destptr[i] = srcptr[i];
    }
    
}

char* intToStr(int n)
{
    char* c;
    //c = (char *)malloc(10 * sizeof(char)); 
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
