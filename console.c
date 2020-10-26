/**
 * @file console.c
 * @author ubunterro (finko-ilya@yandex.ru)
 * @brief Работа с консолью
 * @version 0.1
 * @date 2020-10-26
 * 
 * @copyright Copyright (c) 2020
 * 
 */

#include "console.h"
#include "libc.h"

char *videoptr = (char*)0xb8000;
int printPtr = 0;

/**
 * @brief Печатает текст в консоль \n
 * Начиная от указателя на первый символ вплоть до нуль-терминатора (\\0)
 * 
 * @param str Указатель на первый символ текста
 */
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

/**
 * @brief Очищает консоль
 * 
 */
void console_clear(){
  int pos = 0;
  for (int i = 0; i < 80 * 25; i++) {
    videoptr[pos++] = 0;
    videoptr[pos++] = 0;
  }
}


/**
 * @brief Сдвигает текст в консоли вверх
 * 
 * @param n Колличество строк, на сколько сдвигается текст
 */
void scrollConsole(int n){
    memcpy(videoptr, videoptr+CONSOLE_COLS*2 * n, CONSOLE_COLS*(CONSOLE_ROWS-1)*2);
    
}
