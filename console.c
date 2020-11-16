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
 * 2.0 Посимвольный вывод в консоль
 * 
 * @param str Указатель на первый символ текста
 * 2.0 с Указатель на переданный символ
 */
void putchar(char *c){
    
  // while (*str != '\0'){
  //Начало цикла для обработки строки

        int currRow = printPtr / 2 / CONSOLE_COLS;

        if (*c == '\n'){
            int nextRow = CONSOLE_COLS * (currRow + 1);

            printPtr = nextRow * 2;
            
        } else {
            if (printPtr/2 >= CONSOLE_ROWS * CONSOLE_COLS){
                //printPtr = 0;
                scrollConsole(1);
                printPtr = (CONSOLE_ROWS-1) * CONSOLE_COLS * 2;
            }
            videoptr[printPtr++] = *c;

        //printPtr++;
            videoptr[printPtr++] = 0x07;
        }
        
        //videoptr++;

	//Код окончания цикла дл обработки строки
	//   str++;
	//  }
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


/**
 * @brief Посимвольный анализ строки для печати типа printf - вывод значения многих переменных
 * @param *str указатель на строку
 *
 */

void kprint(char *str,...){
    int i=0,u=1;
    
     for(i=0; str[i]!='\0'; i++){
      char symbol=str[i];
      char next_symbol=str[i+1];
      
       if(symbol=='%' && next_symbol=='i'){
         int *param = (int *)&str;
         char *param_int = intToStr(*(param + u));      
          kprint(param_int);
          i++;
          u++;
       }
        else{
            putchar(&symbol); 
    }
  }
}
