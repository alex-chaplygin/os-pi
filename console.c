#include "console.h"
#include "libc.h"

char *videoptr = (char*)0xb8000;
int printPtr = 0;

void kprint(char *str){
    
    while (*str != '\0'){



        int currRow = printPtr / 2 / CONSOLE_COLS;

        if (*str == '\n'){
            int nextRow = CONSOLE_COLS * (currRow + 1);

            printPtr = nextRow * 2;
            
        } else {
            if (printPtr/2 >= CONSOLE_ROWS * CONSOLE_COLS){
                //printPtr = 0;
                scrollConsole(1);
                printPtr = (CONSOLE_ROWS-1) * CONSOLE_COLS * 2;
            }
            videoptr[printPtr++] = *str;

        //printPtr++;
            videoptr[printPtr++] = 0x07;
        }
        
        //videoptr++;

        str++;

        
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


void console_clear(){
  int pos = 0;
  for (int i = 0; i < 80 * 25; i++) {
    videoptr[pos++] = 0;
    videoptr[pos++] = 0;
  }
}

void scrollConsole(int n){
    memcpy(videoptr, videoptr+CONSOLE_COLS*2 * n, CONSOLE_COLS*(CONSOLE_ROWS-1)*2);
    
}