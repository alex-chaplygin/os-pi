#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexer.h"
#include "objects.h"
#include "symbols.h"
#include "parser.h"

extern int token_error;

token_t *cur_token; // текущий токен

// преобразует строку в верхний регистр
char *strupr(char *str)
{
    char *ptr = str;
    
    while (*ptr != '\0') {
        if (*ptr >= 'a' && *ptr <= 'z') {
            *ptr = *ptr - 'a' + 'A';
        }
        ptr++;
    }    
    return str;
}

object_t parse();
object_t parse_list();
object_t parse_array();

/**
 *  Обработка кавычки, обратной кавычки, запятой
 *  'a -> (quote a)
 * '(4 5 6) -> (quote (4 5 6))
 *
 * @param quote_sym символ функции цитирования
 * @return указатель на список с quote
 */
object_t parse_quote(char *quote_sym)
{
    //printf("parse_quote: ");
    object_t o = parse();
    //printf("quote: ");
    //PRINT(o);
    if (o == NOVALUE)
	error("quote: no args");
    object_t p = new_pair(o, NULLOBJ);
    return new_pair(NEW_SYMBOL(quote_sym), p);
}

/** 
 * Обработка элемента списка
 * Создаём объект пару с текущим элементом и рекурсивной обработкой хвоста
 *  
 * @return указатель на объект списка
 */
object_t parse_element(type_t type, void *data, tokentype_t t_type)
{
    object_t obj;
    if (t_type == QUOTE)
	obj = parse_quote("QUOTE");
    else if (t_type == SHARP)
	obj = parse_array();
    else if (t_type == BACKQUOTE)
	obj = parse_quote("BACKQUOTE");
    else if (t_type == COMMA)
	obj = parse_quote("COMMA");
    else if (t_type == COMMA_AT)
	obj = parse_quote("COMMA-AT");
    else if (t_type == LPAREN)
	obj = parse_list();
    else if (t_type == T_NUMBER)
	obj = new_number(*(int *)data);
    else if (t_type == T_SYMBOL)
	obj = NEW_OBJECT(SYMBOL, find_symbol(data));
    else if (t_type == T_STRING)
	obj = NEW_STRING((char *)data);
    else
	obj = NEW_OBJECT(type, data);
    object_t tail = parse_list();
    return new_pair(obj, tail);
}

/** 
 * Обработка списка без левой скобки
 * (1 2 3) (1 . (2 . (3 . nil)))
 * (1 . 2)
 * Обработка заканчивается когда встречает правую скобку или конец ввода
 *  
 * @return указатель на объект списка
 */
object_t parse_list()
{
    int val;
    char str[MAX_STR];
    token_t *cur_tok = get_token();
    //   printf("parselist: ");
    //   print_token(cur_tok);
    if (token_error == 1)
        error("parse: token_error");
    if (cur_tok->type == END)
	error("expected )");
    if (cur_tok->type == RPAREN)
	return NULLOBJ;
    if (cur_tok->type == T_NUMBER) {
        val = cur_tok->value;
	return parse_element(NUMBER, &val, cur_tok->type);
    } else if (cur_tok->type == T_STRING) {
	strcpy(str, cur_tok->str);
	return parse_element(STRING, str, cur_tok->type);
    } else if (cur_tok->type == T_SYMBOL) {
        strcpy(str, cur_tok->str);
	return parse_element(SYMBOL, strupr(str), cur_tok->type);
    } else if (cur_tok->type == LPAREN || cur_tok->type == QUOTE
	       || cur_tok->type == BACKQUOTE || cur_tok->type == COMMA
	       || cur_tok->type == COMMA_AT || cur_tok->type == SHARP)
	return parse_element(SYMBOL, NULL, cur_tok->type);
    else if (cur_tok->type == DOT) {
	object_t res = parse();
	cur_tok = get_token();       
	if (cur_tok->type != RPAREN)
	    error("expected )");
	return res;
    } else if (cur_token->type == INVALID)
        error("parse: invalid token");
    else 
	error("invalid expression");
}

/** 
 * Обработка массива #(1 2 3)
 *  
 * @return объект списка
 */
object_t parse_array()
{
    object_t o = parse();
    if (o != NULLOBJ && TYPE(o) != PAIR)
	error("invalid array");
    return NEW_ARRAY(o);
}

/**
 * Читает выражение и строит обьект этого выражения
 * (1 2)
 * 12
 * ABC
 * @return указатель на объект
 */
object_t parse()
{   
    object_t el; // создаем новый элемент
    token_t *cur_token = get_token(); // считывается левая скобка
    // printf("parse: ");
    // print_token(cur_token);
    if (token_error == 1)
        error("parse: token_error");
    if (cur_token->type == T_NUMBER) // считывается число
	return new_number(cur_token->value);
    else if (cur_token->type == T_SYMBOL)//считывается символ
	return NEW_OBJECT(SYMBOL, find_symbol(strupr(cur_token->str)));
    else if (cur_token->type == LPAREN)
	return parse_list();
    else if (cur_token->type == QUOTE)
	return parse_quote("QUOTE");
    else if (cur_token->type == BACKQUOTE)
	return parse_quote("BACKQUOTE");
    else if (cur_token->type == COMMA)
	return parse_quote("COMMA");
    else if (cur_token->type == SHARP)
	return parse_array();
    else if (cur_token->type == T_STRING)
	return NEW_STRING(cur_token->str);
    else if (cur_token->type == END)
        return NOVALUE;
    else if (cur_token->type == INVALID)
        error("parse: invalid token");
    else 
	error("invalid expression");
}
