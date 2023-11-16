/**
 * @file   lexer.c
 * @author alex <alex@alex-home>
 * @date   Tue Oct  3 18:12:08 2023
 * 
 * @brief  Модуль лексического разбора
 */

#include <stdio.h>
#include <ctype.h>
#include "lexer.h"

/// текущий символ
char cur_symbol;
/// если true, не считывать символ
int flag = 0;
/// текущий токен
token_t token;
/// Если 1, то значит была ошибка при лексическом разборе
int token_error;
/// Если 1, то загрузка кода из секции .lisp ядра
int boot_load = 0;
/// Адрес памяти, откуда загружается lisp при загрузке
char *boot_code;

// читать один символ
void get_cur_char()
{
    if (flag)
        flag = 0;
    else {
	if (!boot_load)
	    cur_symbol = getchar();
	else
	    cur_symbol = *boot_code++;
    }
}

// вернуть последн\. символ назад 
void unget_cur_char() 
{
    flag = 1;
}

/** 
 * Проверка символа на пустоту
 */ 
int is_whitespace(char c)
{
    return c == ' ' || c == '\n' || c == '\r' || c == '\t'; 
}

/** 
 * Пропуск символов до конца строки
 */
void skip_comment()
{
    while (cur_symbol != '\n' && cur_symbol != EOF)
	get_cur_char();
}

/** 
 * Пропустить пустоты: пробелы, переводы строк, комментарии
 * "    ; hjhjhjh\n"
 * ";hfjkdhjkfhd\n"
 */
void skip_white_space()
{
    while (is_whitespace(cur_symbol) || cur_symbol == ';') {
	if (cur_symbol == ';')
	    skip_comment();
	get_cur_char();
    }
    unget_cur_char();
}

// проверка символа на цифру
int is_digit(char c)
{
    return c >= '0' && c <= '9';
}

// проверка символа на букву
int is_alpha(char c)
{
    return c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z';
}

/** 
 * Проверка символа на разрешенный символ
 */ 
int is_symbol(char c)
{
    char str[] = "+-*/=_&|<>";
    for (int i = 0; i < sizeof(str); i++)
	if (str[i] == c)
	    return 1;
    return 0;
}

/** 
 * Проверка символа на разрешенный символ
 */ 
int is_hex_symbol(char c) 
{
    return c >= 'a' && c <= 'f' ||
           c >= 'A' && c <= 'F';
}

/**
 * Проверка на разделитель
 */
int is_delimeter(char c)
{
    return c == ')' || c == '(' || is_whitespace(c) || c == EOF;
}

/** 
 * на входе строка FFAA
 *
 * @return преобразованное число AB
 */
int hex_num()
{
    int cur_num =  0;
  
    do {
	get_cur_char();
	if (is_delimeter(cur_symbol))
	    break;
	if (is_digit(cur_symbol)) {
	    cur_num = cur_num * 16 + cur_symbol - '0';
	} else if (is_hex_symbol(cur_symbol)) {
	    cur_symbol = toupper(cur_symbol);
	    cur_num = cur_num * 16 + cur_symbol - 'A' + 10;
	} else {
	    token_error = 1;
	    printf("invalid hex num\n");
	    return 0;
	}
    } while (is_digit(cur_symbol) || is_hex_symbol(cur_symbol));
  
    unget_cur_char();
    return cur_num;
}

/** 
 * Функция считывает числа из строки и преобразует в число  десятичной и шестнадцатиричной СС
 * 10 - десятичной, 0xFFAA - шестнадцатиричное 
 */
int get_num()
{
    get_cur_char();
    int fl = 0;
    int cur_num = 0;
    if (cur_symbol == '0') {
	get_cur_char();
	if (cur_symbol == 'x')
	    return hex_num();
    } else if (cur_symbol == '-') {
        fl = 1;
        get_cur_char();
    }
    while (is_alpha(cur_symbol) || is_digit(cur_symbol) || is_symbol(cur_symbol)) 
    {
	if (is_digit(cur_symbol)) {
	    cur_num = cur_num * 10 + cur_symbol - '0';
	    get_cur_char();
	} else {
	    token_error = 1;
	    printf("invalid num\n");
	    return 0;
	}
    }
    unget_cur_char();
    if (fl == 1)
        cur_num *= -1;
    return cur_num;
}



//прочесть атом
void get_symbol(char *cur_str)
{
    get_cur_char();
    int c = 0;
    while (!is_delimeter(cur_symbol))
    {
	if (!(is_alpha(cur_symbol) || is_digit(cur_symbol) || is_symbol(cur_symbol))){
	    printf("ERROR: lexer.c: Unsupported character in input: %c(#%x)", cur_symbol, cur_symbol);
	    token_error = 1;
	    return;
	}
	
        cur_str[c++] = cur_symbol;
        get_cur_char();
    }
    unget_cur_char();
    cur_str[c] = 0;
}

/** 
 * прочесть строку в двойных кавычках \"1 2 3\"
 *
 * @param cur_str куда сохраняется строка
 */
void get_string(char *cur_str)
{
    int c = 0;
    int ended_quote = 0;
    
    get_cur_char(); // Считываем открывающую кавычку
    get_cur_char();
    while (cur_symbol != EOF) {
	if (cur_symbol == '"') {
	    ended_quote = 1;
	    break;
	} else if (cur_symbol == '\\') {
	    get_cur_char();
	    if (cur_symbol == 'n')
		cur_symbol = '\n';
	}
	cur_str[c++] = cur_symbol;
	get_cur_char();
    }
    if (!ended_quote)
	token_error = 1;
    cur_str[c] = '\0'; // Завершаем строку
}

//COMMA_AT function
token_t get_comma()
{
    char ctr[2];
    ctr[0] = cur_symbol;
    get_cur_char();
    get_cur_char();
    ctr[1] = cur_symbol;
    if (ctr[0] == ',' && ctr[1] == '@') {
        token.type = COMMA_AT;
    }
    else {
        token.type = COMMA;
        unget_cur_char();
    }
}

// '(3 4) 'a
token_t *get_token()
{
    token_error  = 0;
    get_cur_char();
    skip_white_space();
    switch (cur_symbol) {
    case '(':
	get_cur_char();
	token.type = LPAREN;
	break;
    case ')':
	get_cur_char();
	token.type = RPAREN;
	break;
    case EOF:
	token.type = END;
	break;
    case '\'':
	get_cur_char();
	token.type = QUOTE;
	break;
    case '`':
	get_cur_char();
	token.type = BACKQUOTE;
	break;    
    case ',':
    get_comma();
	break;
    case '"':
	// Используем get_string для чтения строки в двойных кавычках
	get_string(token.str); 
	token.type = T_STRING;
	break;
    case '#':
	get_cur_char();
	token.type = SHARP;
	break;
    case '.':
	get_cur_char();
	token.type = DOT;
	break;
    default:
	if (is_digit(cur_symbol)) {
	    token.type = T_NUMBER;
	    token.value = get_num();
	} else if (is_alpha(cur_symbol) || is_symbol(cur_symbol)) {
	    token.type =  T_SYMBOL;
	    get_symbol(token.str);
	} else {
	    token.type = INVALID;
	    get_cur_char();
	}
    }
    return &token;
}

/** 
 * Печать лексемы для отладки
 *
 * @param token указатель на лексему
 */
void print_token(token_t *token)
{
    switch (token->type) {
    case T_NUMBER:
	printf("NUM %d\n", token->value);
	break;
    case T_SYMBOL:
	printf("SYM %s\n", token->str);
	break;
    case LPAREN:
	printf("LPAREN\n");
	break;
    case RPAREN:
	printf("RPAREN\n");
	break;
    case END:
	printf("END\n");
	break;
    case QUOTE:
	printf("QUOTE\n");
	break;
    case BACKQUOTE:
	printf("BACKQUOTE\n");
	break;
    case COMMA:
	printf("COMMA\n");
	break;
    case COMMA_AT:
	printf("COMMA_AT\n");
	break;
    case INVALID:
	printf("INVALID\n");
	break;
    case T_STRING:
	printf("STRING %s\n", token->str);
	break;
    case SHARP:
	printf("SHARP\n");
	break;
    case DOT:
	printf("DOT\n");       
    }
}
