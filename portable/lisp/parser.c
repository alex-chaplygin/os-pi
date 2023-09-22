#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "objects.h"
#include "parser.h"

token_t *cur_token; // текущий токен

// Вывод сообщения об ошибке и выход из программы
// str - сообщение об ошибке
void error(char *str)
{
    printf("%s\n", str);
}

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
object_t *parse();

/**
 *  Обработка кавычки
 *  'a -> (quote a)
 * '(4 5 6) -> (quote (4 5 6))
 *
 * @return указатель на список с quote
 */
object_t *parse_quote()
{
    object_t *o = parse();
    object_t *p = new_pair(o, NULL);
    if (o->type == NUMBER){
	error("quote Number\n");
	return ERROR;
    } else
	return new_pair(object_new(SYMBOL, "QUOTE"), p);
}

/** 
 * Обработка списка без левой скобки
 * Обработка заканчивается когда встречает правую скобку или конец ввода
 *  
 * @return указатель на объект списка
 */
object_t *parse_list()
{
    int val;
    token_t *cur_token = get_token();
    printf("parselist: ");
    print_token(cur_token);
    if (cur_token->type == RPAREN)
	return NULL;
    if (cur_token->type == T_NUMBER)
	return new_pair(object_new(NUMBER, &cur_token->value), parse_list());
    else if (cur_token->type == T_SYMBOL)
	return new_pair(object_new(SYMBOL, cur_token->str), parse_list());
    else if (cur_token->type == LPAREN){
	object_t *list = parse_list();
	return new_pair(list, parse_list());
    } else if (cur_token->type == QUOTE){
	object_t *q = parse_quote();
	return new_pair(q, parse_list());
    }
}


/**
 * Читает выражение и строит обьект этого выражения
 * (1 2)
 * 12
 * ABC
 * @return указатель на объект
 */
object_t *parse()
{   
    object_t *el; // создаем новый элемент
    cur_token = get_token(); // считывается левая скобка
    printf("parse: ");
    print_token(cur_token);
    if (cur_token->type == T_NUMBER) // считывается число
	return object_new(NUMBER, &cur_token->value);
    else if (cur_token->type == T_SYMBOL)//считывается символ
	return object_new(SYMBOL, find_symbol(strupr(cur_token->str)));
    else if (cur_token->type == LPAREN)
	return parse_list();
    else if (cur_token->type == QUOTE)
	return parse_quote();
}
