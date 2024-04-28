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
int print_ptr = 0; /**< Смещение видеопамяти, куда печатается очередной символ */
int symbol_color = 7; /**< Код цвета символа */
int back_color = 0; /**< Код цвета фона */

/** 
 * @brief Функция включения указателя
 * 
 * @param start Начальная строка
 * @param end Конечная строка
 */
void enable_cursor(byte start, byte end)
{
    outb(0x3D4, 0x0A);
    outb(0x3D5, (inb(0x3D5) & 0xC0) | start);

    outb(0x3D4, 0x0B);
    outb(0x3D5, (inb(0x3D5) & 0xE0) | end);
}

/** 
 * @brief Функция выключения курсора
 * 
 */
void disable_cursor()
{
    outb(0x3D4, 0x0A);
    outb(0x3D5, 0x20);
}

/** 
 * @brief Функция перемещения курсора
 * 
 * @param pos Позиция, в которую курсор передвигается
 */
void move_cursor()
{
    int pos = print_ptr >> 1;
    outb(0x3D4, 0x0F);
    outb(0x3D5, (byte)(pos & 0xFF));
    outb(0x3D4, 0x0E);
    outb(0x3D5, (byte)((pos >> 8) & 0xFF));
}

/**
 * @brief Печатает символ в консоль
 * обрабатывает перевод строки
 * 
 * @param с переданный символ
 */
void putchar(char c)
{
    int cur_row = (print_ptr >> 1) / CONSOLE_COLS;
    if (c == '\n') {
	int next_row = CONSOLE_COLS * (cur_row + 1);
	print_ptr = next_row << 1;
	move_cursor();
	return;
    } else if (c == '\b') {
	print_ptr -= 2;
	videoptr[print_ptr] = ' ';
	move_cursor();
	return;
    }
    videoptr[print_ptr] = c;
    videoptr[print_ptr + 1] = (back_color << 4) + symbol_color;
    print_ptr += 2;
    move_cursor();
    if (cur_row == CONSOLE_ROWS) {
	console_clear();
	print_ptr = 0;
	move_cursor();
    }
}

/**
 * @brief Устанавливает курсор
 * 
 * @param x номер столбца
 * @param y номер строки
 */
void set_cursor(int x, int y)
{
    print_ptr = (x + y * CONSOLE_COLS) * 2;
    move_cursor();
}

/**
 * @brief Устанавливает цвет символа
 * 
 * @param col - код цвета символа
 */
void set_color(int col)
{
    symbol_color = col;
}

/**
 * @brief Устанавливает цвет фона
 * 
 * @param col - код цвета фона
 */
void set_back_color(int col)
{
    back_color = col;
}

/**
 * @brief Очищает консоль
 * 
 */
void console_clear()
{
    int pos = 0;
    for (int i = 0; i < CONSOLE_ROWS * CONSOLE_COLS; i++) {
	videoptr[pos++] = ' ';
	videoptr[pos++] = (back_color << 4) + symbol_color;
    }
}
