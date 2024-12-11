#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexer.h"
#include "objects.h"
#include "symbols.h"
#include "parser.h"

void parser_error(char *str, ...);

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
	parser_error("quote: no args");
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
    else if (t_type == T_FUNCTION)
	obj = parse_quote("FUNCTION");
    else if (t_type == LPAREN)
	obj = parse_list();
    else if (t_type == T_FLOAT)
	obj = new_float(*(float *)data);
    else if (t_type == T_NUMBER)
	obj = new_number(*(int *)data);
    else if (t_type == T_SYMBOL)
	obj = NEW_OBJECT(SYMBOL, find_symbol(data));
    else if (t_type == T_STRING)
	obj = NEW_STRING((char *)data);
    else if (t_type == T_CHAR)
	obj = NEW_CHAR(*(char *)data);
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

    if (cur_tok->type == END)
	parser_error("expected )");
    if (cur_tok->type == RPAREN)
	return NULLOBJ;
    if (cur_tok->type == T_NUMBER) {
        val = cur_tok->value;
	return parse_element(NUMBER, &val, cur_tok->type);
    } else if(cur_tok->type == T_FLOAT){
    	val = cur_tok->value;
    	return parse_element(FLOAT, &val, cur_tok->type);
    } else if (cur_tok->type == T_STRING) {
	strcpy(str, cur_tok->str);
	return parse_element(STRING, str, cur_tok->type);
    } else if (cur_tok->type == T_SYMBOL) {
        strcpy(str, cur_tok->str);
	return parse_element(SYMBOL, strupr(str), cur_tok->type);
    } else if (cur_tok->type == T_CHAR) {
        val = cur_tok->value;
        return parse_element(CHAR, &val, cur_tok->type);
    } else if (cur_tok->type == LPAREN || cur_tok->type == QUOTE
	       || cur_tok->type == BACKQUOTE || cur_tok->type == COMMA
	       || cur_tok->type == COMMA_AT || cur_tok->type == SHARP || cur_tok->type == T_FUNCTION)
	return parse_element(SYMBOL, NULL, cur_tok->type);
    else if (cur_tok->type == DOT) {
	object_t res = parse();
	cur_tok = get_token();       
	if (cur_tok->type != RPAREN)
	    parser_error("expected )");
	return res;
    } else 
	parser_error("invalid expression");
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
	parser_error("invalid array");
    return NEW_ARRAY(o);
}

/**
 * Читает выражение и строит обьект этого выражения
 * Числа: T_NUMBER -> NUMBER или BIGNUMBER
 *        T_FLOAT -> FLOAT
 * Символы: T_SYMBOL -> SYMBOL
 *          T_CHAR -> CHAR
 * Списки: (<объекты> ... ) -> PAIR
 *         ( 1 A B 2 ) -> (1 . (A . (B . (2 . NIL))))
 *         (1 . 3) -> PAIR (1, 3)
 * Цитирование: '<объект> -> (QUOTE <объект>)
 *              `<объект> -> (BACKQUOTE <объект>)
 *              ,<объект> -> (COMMA <объект>)
 *              ,@<объект> -> (COMMA-AT <объект>)  
 *              #'<объект> -> (FUNCTION <объект>)  
 * Строки: T_STRING -> STRING
 * Массивы: #(<объекты>) -> ARRAY
 * END -> NULLOBJ
 * 
 * @return объект
 */
object_t parse()
{   
    object_t el; // создаем новый элемент
    token_t *cur_token = get_token(); // считывается левая скобка    
    
    if (cur_token->type == T_NUMBER) // считывается число
	return new_number(cur_token->value);
    else if (cur_token->type == T_FLOAT)
	return new_float(*(float *)&cur_token->value);
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
    else if (cur_token->type == T_FUNCTION)
	return parse_quote("FUNCTION");
    else if (cur_token->type == SHARP)
	return parse_array();
    else if (cur_token->type == T_STRING)
	return NEW_STRING(cur_token->str);
    else if (cur_token->type == T_CHAR)
	return NEW_CHAR(cur_token->value);
    else if (cur_token->type == END)
        return NOVALUE;
    else 
	parser_error("invalid expression");
}
