/**
 * @file   lexer.c
 * @author alex <alex@alex-home>
 * @date   Tue Oct  3 18:12:08 2023
 * 
 * @brief  Модуль лексического разбора
 */

#include <stdio.h>
#include <limits.h>
#include "lexer.h"

/// текущий символ
char cur_symbol;
/// текущий токен
token_t token;
/// Если 1, то загрузка кода из секции .lisp ядра
int boot_load = 0;
/// Адрес памяти, откуда загружается lisp при загрузке
char *boot_code;
/// размер буфера символов symbol_buffer
#define SYMBOL_BUFFER_SIZE 8
/// буфер символов из stdin
/// с обратным  порядком элементов
char symbol_buffer[SYMBOL_BUFFER_SIZE];
/// текущая позиция записи в буфере symbol_buffer
int buffer_write_pos = 0;
/// текущая позиция чтения из буфера symbol_buffer
int buffer_read_pos = 0;

void error(char *str, ...);

/** 
 * Считывает очередной символ из входного потока символов в глобальную переменную cur_symbol.
 */
void get_cur_char()
{
    if (buffer_write_pos == buffer_read_pos) {
	if (!boot_load)
	    cur_symbol = getchar();
	else
	    cur_symbol = *boot_code++;
	symbol_buffer[buffer_write_pos++] = cur_symbol;
	buffer_read_pos = buffer_write_pos &= SYMBOL_BUFFER_SIZE - 1;
    } else {
	cur_symbol = symbol_buffer[buffer_read_pos++];
	buffer_read_pos &= SYMBOL_BUFFER_SIZE - 1;
    }
    //printf("get_cur_char: '%c' read=%d write=%d\n", cur_symbol, buffer_read_pos, buffer_write_pos);
}

/** 
 * Возращает символ назад в поток
 */
void unget_cur_char() 
{
    cur_symbol = symbol_buffer[(buffer_read_pos - 2) & SYMBOL_BUFFER_SIZE - 1];
    --buffer_read_pos;
    buffer_read_pos &= SYMBOL_BUFFER_SIZE - 1;
    //printf("unget_cur_char: '%c' read=%d write=%d\n", cur_symbol, buffer_read_pos, buffer_write_pos);
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

/** 
 * Проверка символа на разрешенный символ
 */ 
int is_symbol(char c)
{
    char str[] = "+-*/=_&|<>%";
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
    return c == ')' || c == '(' || is_whitespace(c) || c == EOF || c == '"' || c == '\\';
}

/** 
 * на входе строка FFAA
 *
 * @return преобразованное число AB
 */
int hex_num()
{
    long long cur_num =  0;
    int hex_value;
    const int msb_shr = CHAR_BIT * sizeof(int);
  
    do {
		get_cur_char();
		if (is_delimeter(cur_symbol))
			break;
		if (isdigit(cur_symbol)) {
			cur_num = cur_num * 16 + cur_symbol - '0';
		} else if (is_hex_symbol(cur_symbol)) {
			cur_symbol = toupper(cur_symbol);
			cur_num = cur_num * 16 + cur_symbol - 'A' + 10;
		} else
		    error("invalid hex number");
		if ((cur_num >> msb_shr) & 1)
			error("hex number overflow");
    } while (isdigit(cur_symbol) || is_hex_symbol(cur_symbol));
  
    unget_cur_char();
    return cur_num;
}

/** 
 * Разбирает число с плавающей точкой. 
 * Цифры идут уже после запятой.
 *
 * @param int_num целая часть числа
 * @return число с плавающей точкой в формате int
 */
int get_float_num(int int_num, int sgn)
{
    int cnt = 0;
    float float_num = 0;
    int floatbits = 0;
    int num = int_num;
    int temp;
    get_cur_char();
    float count = 1;
    while (!is_delimeter(cur_symbol)) {
	if (!isdigit(cur_symbol))
	    error("invalid symbol in float");
	float_num = float_num * 10 + (cur_symbol - '0');
	count *= 10;
	get_cur_char();
    }
    //printf("float = %f int = %d count = %f \n", float_num, int_num, count);
    float res_float = int_num >= 0 ?  int_num + (float_num / count) : int_num - (float_num / count);
    if (int_num == 0)
	res_float *= sgn;
    //printf("res_float is equal  %f\n", res_float);
    unget_cur_char();
    return *(int *)&res_float;
}

/** 
 * Функция считывает числа в десятичной и шестнадцатиричной СС
 * 10 - десятичной, 0xFFAA - шестнадцатиричное 
 */
int get_num()
{
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
    const int sgn_shr = CHAR_BIT * sizeof(int) - 1;
    int sgn = fl ? -1 : 1;
    int msb;
    while (!is_delimeter(cur_symbol)) {
	if (isdigit(cur_symbol)) {
	    cur_num = cur_num * 10 + sgn * (cur_symbol - '0');
	    msb = (cur_num >> sgn_shr) & 1;
	    
	    if (msb != fl && cur_num != 0)
		error("number overflow");	      
	    
	    get_cur_char();
	} else if (cur_symbol == '.') {
	    token.type = T_FLOAT;
	    return get_float_num(cur_num, sgn);
	} else {
	    error("invalid character in number"); 
	}
    }
    unget_cur_char();
    return cur_num;
}

//прочесть символ
void get_symbol(char *cur_str)
{
    int c = 0;
    while (!is_delimeter(cur_symbol))
    {
	if (cur_symbol == '\b') {
	    cur_str[--c] = ' ';
	    get_cur_char();
	    continue;
	}
	
	if (!(isalpha(cur_symbol) || isdigit(cur_symbol) || is_symbol(cur_symbol)))
	    error("invalid character in symbol: %c(#%x)", cur_symbol, cur_symbol);	    
		
        cur_str[c++] = cur_symbol;
        get_cur_char();

	if (c > MAX_SYMBOL)
	    error("maximum characters in symbol");
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

    get_cur_char();
    while (cur_symbol != EOF) {
	if (cur_symbol == '"')
	    break;
	else if (c == MAX_STR) {
	    error("maximum characters in string");
	}
    	else if (cur_symbol == '\\') {
	    	get_cur_char();
	    	if (cur_symbol == 'n')
		    cur_symbol = '\n';
		else if (cur_symbol == 'x')
		{
		    int code = hex_num();
		    
		    if (code > 255)
		    	error("invalid symbol in string");
		    
		    cur_str[c++] = code;
		    get_cur_char();
		    continue;
		}
	}
	cur_str[c++] = cur_symbol;
	get_cur_char();
    }
    if (cur_symbol != '"')
	error("expected \" at string end");
    
    cur_str[c] = '\0'; // Завершаем строку
}

/** 
 * Определяет тип лексемы , или ,@
 *
 * @return лексему
 */
token_t get_comma()
{
    char ctr[2];
    ctr[0] = cur_symbol;
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

/** 
 * Определяет тип лексемы 
 * # - SHARP
 * #\<символ> - T_CHAR
 * #' - T_FUNCTION				\
 *
 * @return лексему
 */
token_t get_sharp()
{
    char ctr[2];
    ctr[0] = cur_symbol;
    get_cur_char();
    ctr[1] = cur_symbol;
    if (ctr[0] == '#' && ctr[1] == '\\') {
	token.type = T_CHAR;
	get_cur_char();
	token.value = cur_symbol;
    } else if (ctr[0] == '#' && ctr[1] == '\'') {
	token.type = T_FUNCTION;
    } else {
	token.type = SHARP;
	unget_cur_char();
    }
}

/** 
 * Распознает очередную лексему из потока ввода
 * пропускает пустоты (пробелы, переводы строк \n \r, табуляцию)
 * Числа - T_NUMBER (десятичные, шестнадцатеричные 0xFFF)
 * Вещественные числа T_FLOAT (0.3456)
 * Символ - T_SYMBOL (начинается с символа или буквы, далее - символы, буквы, цифры)
 * Разрешенные символы: +-* /=_&|<>%
 *  T_CHAR одиночный символ #\<символ>
 *  LPAREN левая скобка (
 *  RPAREN, // правая скобка )
 *  QUOTE, // символ '
 *  BACKQUOTE, // символ `
 *  COMMA, // символ ,
 *  COMMA_AT, // символ ,@
 *  T_STRING, // строка в кавычках
 *  SHARP, // символ #
 *  T_FUNCTION, // символ #'
 *  DOT,// точка .
 *  END, // если конец потока
 *
 * @return указатель на структуру лексемы
 */
token_t *get_token()
{
    get_cur_char();
    skip_white_space();
    get_cur_char();
    switch (cur_symbol) {
    case '(':
	token.type = LPAREN;
	break;
    case ')':
	token.type = RPAREN;
	break;
    case EOF:
	token.type = END;
	break;
    case '\'':
	token.type = QUOTE;
	break;
    case '`':
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
	get_sharp();
	break;
    case '.':
	token.type = DOT;
	break;
    default:
	if (cur_symbol == '-' || isdigit(cur_symbol)) {
	    if (cur_symbol == '-') {
	        get_cur_char();
		char c = cur_symbol;
		unget_cur_char();
	        if (!isdigit(c))
	            goto get_token_symbol;
	    }
	    token.type = T_NUMBER;
	    token.value = get_num();
	} else if (isalpha(cur_symbol) || is_symbol(cur_symbol)) {
	    get_token_symbol:
	        token.type =  T_SYMBOL;
	        get_symbol(token.str);
	} else 
	    error("invalid symbol");
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
    case T_FLOAT:
	printf("FLOAT %f\n", *(float*)&token->value);
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
    case T_STRING:
	printf("STRING %s\n", token->str);
	break;
    case SHARP:
	printf("SHARP\n");
	break;
    case DOT:
	printf("DOT\n");
	break;
    case T_FUNCTION:
	printf("T_FUNCTION\n");
	break;
    default:
	printf("UNKNOWN!!!");
    }
}

/** 
 * Сброс указателей в буфере
 */
void reset_buffer()
{
    buffer_read_pos = buffer_write_pos = 0;
}
