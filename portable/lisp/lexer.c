#include <stdio.h>
#include "lexer.h"

char cur_symbol; // текущий символ
int flag = 0; // если true, не считывать символ
token_t token;

// читать один символ
void get_cur_char()
{
    if (flag)
        flag = 0;
    else
        cur_symbol = getchar();
}

// вернуть последн\. символ назад 
void unget_cur_char() 
{
    flag = 1;
}

/** 
 * Пропустить пустоты: пробелы, переводы строк
 */
void skip_white_space()
{
    while (cur_symbol == ' ' || cur_symbol == '\n')
        get_cur_char();
    unget_cur_char();
}

// проверка символа на цыфру
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
 * на входе строка FFAA
 *
 * @return преобразованное число AB
 */
int hex_num()
{ 
  get_cur_char();
  int cur_num =  0;
  while (is_digit(cur_symbol) || cur_symbol >= 'a' && cur_symbol <= 'f' || cur_symbol>= 'A' && cur_symbol <= 'F' ) 
  {
    if(is_digit(cur_symbol))
      {
      cur_num = cur_num * 16 + cur_symbol - '0';
      get_cur_char();
      }
    else
      {
      cur_num = cur_num * 16 + cur_symbol - 'A'+ 10; 
      get_cur_char();
      }
  }
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
    while (is_digit(cur_symbol)) 
    {
        cur_num = cur_num * 10 + cur_symbol - '0';
        get_cur_char();
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
    while (is_alpha(cur_symbol) || is_digit(cur_symbol))
    {
        cur_str[c++] = cur_symbol;
        get_cur_char();
    }
    unget_cur_char();
    cur_str[c] = 0;
}

// '(3 4) 'a
token_t *get_token()
{
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
        default:
            if (is_digit(cur_symbol)) {
                token.type = T_NUMBER;
                token.value = get_num();
            } else if (is_alpha(cur_symbol)) {
               token.type =  T_SYMBOL;
               get_symbol(token.str);
            } else {
                get_cur_char();
                token.type = INVALID;
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
    case INVALID:
	printf("INVALID\n");
	break;
    }
}
   
