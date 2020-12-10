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

#include <portable/console.h>
#include <portable/libc.h>
#include <portable/types.h>

char *videoptr = (char*)0xb8000;
int printPtr = 0;
byte buffer[BUFFER_SIZE] = {0};	/**< Буфер экранов */
int current_screen_pos = 0;	/**< Текущая позиция на экране */

/** 
 * @brief Копирует экран из буфера в видеопамять
 * 
 */
void copy_to_screen()
{
  byte *bufferptr = &buffer[current_screen_pos];
  memcpy(videoptr, bufferptr, CONSOLE_ROWS * CONSOLE_COLS * 2);
}

/** 
 * @brief Переключает экран на предыдущий
 * 
 */
void screen_up()
{
  if(current_screen_pos - SHIFT < 0)
    return;
  else
    {
      current_screen_pos -= SHIFT;
      copy_to_screen();
    }
}

/** 
 * @brief Переключает экран на следующий
 * 
 */
void screen_down()
{
  if(current_screen_pos + SHIFT > BUFFER_SIZE)
    return;
  else
    {
      current_screen_pos += SHIFT;
      copy_to_screen();
    }
}

/**
 * @brief Печатает символ в консоль
 * обрабатывает перевод строки
 * 
 * @param с переданный символ
 */
void putchar(char c){
  int currRow = printPtr / 2 / CONSOLE_COLS;

  if (c == '\n'){
    int nextRow = CONSOLE_COLS * (currRow + 1);
    printPtr = nextRow * 2;
            
  } else {
    if (printPtr/2 >= CONSOLE_ROWS * CONSOLE_COLS){
      screen_down();
      printPtr = (CONSOLE_ROWS-1) * CONSOLE_COLS * 2;
    }
    videoptr[printPtr] = c;
    buffer[current_screen_pos + printPtr++] = c;
    buffer[current_screen_pos + printPtr] = 0x7;
    videoptr[printPtr++] = 0x07;
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
/**
 * @brief Посимвольный анализ строки для печати типа printf - вывод значения многих переменных
 * %i - вывод целого числа
 * %d - вывод десятичного числа
 * %x - вывод шестнадцатеричного числа
 * %s - вывод строки 
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
      char *param_int = int_to_str(*(param + u));      
      kprint(param_int);
      i++;
      u++;
    }
    else if(symbol=='%' && next_symbol=='d'){
      int *param = (int *)&str;
      char *param_int = int_to_str(*(param + u));      
      kprint(param_int);
      i++;
      u++;
    }
    else if(symbol=='%' && next_symbol=='x'){
      int *param = (int *)&str;
      char *param_int = int_to_str_hex(*(param + u));
      kprint(param_int);
      i++;
      u++;
    }
    else if(symbol=='%' && next_symbol=='s'){
      char* *param = (char* *)&str;
      kprint(*(param + u));
      i++;
      u++;
    }
    else{
      putchar(symbol);
    }
	
  }
}
