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

// пропускать пробелы
void skip_white_space()
{
    while (cur_symbol == ' ')
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

// считать число
int get_num()
{
    get_cur_char();
    int cur_num = 0;
    while (is_digit(cur_symbol))
    {
        cur_num = cur_num * 10 + cur_symbol - '0';
        get_cur_char();
    }
    unget_cur_char();
    return cur_num;
}

//прочесть атом
void get_atom(char *cur_str)
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
        case '\n':
            token.type = END;
            break;
        case '\'':
            token.type = QUOTE;
            break;
        default:
            if (is_digit(cur_symbol)) {
                token.type = T_NUMBER;
                token.value = get_num();
            } else if (is_alpha(cur_symbol)) {
               token.type =  T_ATOM;
               get_atom(token.str);
            } else {
                get_cur_char();
                token.type = INVALID;
            }
    }
    return &token;
}