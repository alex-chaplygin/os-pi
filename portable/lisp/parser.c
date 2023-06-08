#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "list.h"

token_t *cur_token;

// Вывод сообщения об ошибке и выход из программы
// str - сообщение об ошибке
void error(char *str)
{
    printf("%s\n", str);
    exit(1);
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

// Обработка списка без левой скобки
list_t *parse_list()
{
    list_t *list = NULL;
    
    if (cur_token->type != LPAREN)
        error("expected (");
    cur_token = get_token();
    while (cur_token->type != END && cur_token->type != RPAREN) {
        if (cur_token->type == T_NUMBER) 
            list_add(&list, NUMBER, &cur_token->value);
        else if (cur_token->type == T_ATOM)
	  list_add(&list, ATOM, strupr(cur_token->str));
        else if  (cur_token->type == LPAREN)
            list_add(&list, LIST, parse_list());
        else if (cur_token->type == INVALID)
            error("expected number or atom");
        cur_token = get_token();
    }
    if (cur_token->type != RPAREN)
        error("expected )");
    return list;
}

// Обработка выражения
list_t *parse()
{
    cur_token = get_token(); // считывается левая скобка
    return parse_list();
}
