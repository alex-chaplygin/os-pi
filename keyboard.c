/*
Если приложение хочет получить ввод с клавиатуры, оно читает содержимое буфера или ждёт пока в нём появится код клавиши
В буфер помещаются 16 последних нажатых клавиш, если их не считывать они будут теряться
*/

#include <stdarg.h>
#include "stdlib.h"
#include "interrupts.h"

#define KEY_BUFFER_SIZE 16
char key_buffer[KEY_BUFFER_SIZE];
unsigned int key_buffer_head = 0;
unsigned int key_buffer_tail = 0;

/* 
Для организации списка используем указатели на "head" и на "tait" списка кодов клавиш.
При помещении очередного символа в буфер "tait" увеличивается на 1. При чтении символа из буфера "head" тоже сдвигается на 1.
Если  достигают конца массива становятся равными 0
*/


/* 
Чтобы отделить преобразование символов от работы с буфером вынесем чтение скан-кода в отдельную функцию
*/
uint8 in_scancode() {
	uint8 result;
	if (key_buffer_head != key_buffer_tail) {
		if (key_buffer_head >= KEY_BUFFER_SIZE) {
			key_buffer_head = 0;
		}
		result = key_buffer[key_buffer_head];
		key_buffer_head++;
	} else {
		result = 0;
	}
	return result;
} /*Эта функция возвращает скан-код нажатой клавиши или 0, если буфер пуст*/


/*Преобразовывание скан-кода в символ*/
#ifndef SCANCODES_H
#define SCANCODES_H

char scancodes[] = {
		0,
		0, // ESC
		'1','2','3','4','5','6','7','8','9','0', '-', '=',
		8, // BACKSPACE
		'\t', // TAB
		'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']',
		'\n', // ENTER
		0, // CTRL
		'a', 's', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',
		0, // LEFT SHIFT
		'\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/',
		0, // RIGHT SHIFT
		'*', // NUMPAD
		0, // ALT
		' ', // SPACE
		0, // CAPSLOCK
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // F1 - F10
		0, // NUMLOCK
		0, // SCROLLLOCK
		0, // HOME
		0,
		0, // PAGE UP
		'-', // NUMPAD
		0, 0,
		0,
		'+', // NUMPAD
		0, // END
		0,
		0, // PAGE DOWN
		0, // INS
		0, // DEL
		0, // SYS RQ
		0,
		0, 0, // F11 - F12
		0,
		0, 0, 0, // F13 - F15
		0, 0, 0, 0, 0, 0, 0, 0, 0, // F16 - F24
		0, 0, 0, 0, 0, 0, 0, 0
	};
	
char scancodes_shifted[] = {
		0,
		0, // ESC
		'!','@','#','$','%','^','&','*','(',')', '_', '+',
		8, // BACKSPACE
		'\t', // TAB
		'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}',
		'\n', // ENTER
		0, // CTRL
		'A', 'S', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"', '~',
		0, // LEFT SHIFT
		'|', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?',
		0, // RIGHT SHIFT
		'*', // NUMPAD
		0, // ALT
		' ', // SPACE
		0, // CAPSLOCK
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // F1 - F10
		0, // NUMLOCK
		0, // SCROLLLOCK
		0, // HOME
		0,
		0, // PAGE UP
		'-', // NUMPAD
		0, 0,
		0,
		'+', // NUMPAD
		0, // END
		0,
		0, // PAGE DOWN
		0, // INS
		0, // DEL
		0, // SYS RQ
		0,
		0, 0, // F11 - F12
		0,
		0, 0, 0, // F13 - F15
		0, 0, 0, 0, 0, 0, 0, 0, 0, // F16 - F24
		0, 0, 0, 0, 0, 0, 0, 0
	};

#endif 
/*
Эта функция ждет нажатий при необходимости, а также преобразовывает скан-коды в символы с учётом нажатости клавиши Shift
*/
char in_char(bool wait) {
	static bool shift = false;
	uint8 chr;
	do {
		chr = in_scancode();
		switch (chr) {
			case 0x2A:
			case 0x36:
				shift = true;
				break;
			case 0x2A + 0x80:
			case 0x36 + 0x80:
				shift = false;
				break;
		}
		if (chr & 0x80) {
			chr = 0;
		}
		if (shift) {
			chr = scancodes_shifted[chr];
		} else {
			chr = scancodes[chr];
		}
	} while (wait && (!chr));
	return chr;
} 


/*
Поддержка ввода текста
*/
void in_string(char *buffer, size_t buffer_size) {
	char chr;
	size_t position = 0;
	do {
		chr = in_char(true);
		switch (chr) {
			case 0:
				break;
			case 8:
				if (position > 0) {
					position--;
					out_char(8);
				}
				break;
			case '\n':
				out_char('\n');
				break;
			default:
				if (position < buffer_size - 1) {
					buffer[position] = chr;
					position++;
					out_char(chr);
				}
		}
	} while (chr != '\n');
	buffer[position] = 0;
} 
