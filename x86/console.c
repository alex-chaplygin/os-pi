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

#include <x86/console.h>
#include <portable/libc.h>
#include <portable/types.h>
#include <x86/x86.h>

char *videoptr = (char*)0xb8000;
int printPtr = 0;
int print_page = 0;
int current_page = 0;
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
      current_page--;
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
      current_page++;
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
      if (current_page == print_page) {
        screen_down();
      }
      
      print_page++;
      printPtr = (CONSOLE_ROWS+1) * CONSOLE_COLS;
    }

    if (print_page == current_page) {
      videoptr[printPtr] = c;
    } else if (current_page - print_page == 1) {
      videoptr[printPtr - SHIFT] = c;
    }

    buffer[print_page * SHIFT + printPtr++] = c;
    buffer[print_page * SHIFT + printPtr] = 0x7;
    
    if (print_page == current_page) {
      videoptr[printPtr] = 0x07;
    } else if (current_page - print_page == 1) {
      videoptr[printPtr - SHIFT] = 0x07;
    }
    move_cursor(printPtr);

    printPtr++;
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
 * @brief Функция включения указателя
 * 
 * @param start Начальная строка
 * @param end Конечная строка
 */
void enable_cursor(byte start, byte end)
{
  write_port(0x3D4, 0x0A);
  write_port(0x3D5, (read_port(0x3D5) & 0xC0) | start);

  write_port(0x3D4, 0x0B);
  write_port(0x3D5, (read_port(0x3D5) & 0xE0) | end);
}

/** 
 * @brief Функция выключения курсора
 * 
 */
void disable_cursor()
{
  write_port(0x3D4, 0x0A);
  write_port(0x3D5, 0x20);
}

/** 
 * @brief Функция перемещения курсора
 * 
 * @param pos Позиция, в которую курсор передвигается
 */
void move_cursor(int pos)
{
  write_port(0x3D4, 0x0F);
  write_port(0x3D5, (byte) (pos & 0xFF));
  write_port(0x3D4, 0x0E);
  write_port(0x3D5, (byte) ((pos >> 8) & 0xFF));
}

