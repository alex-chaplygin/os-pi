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

void parse_list()
{
    if (cur_token->type != LPAREN)
        error("expected (");
    cur_token = get_token();
    while (cur_token->type != END && cur_token->type != RPAREN) {
        if (cur_token->type == NUMBER) 
            list_add(cur_token->type, cur_token->value);
        else if (cur_token->type == ATOM)
            list_add(cur_token->type, cur_token->str);
        else if  (cur_token->type == LPAREN)
            parse_list();
        else if (cur_token->type == INVALID)
            error("expected number or atom");
        cur_token = get_token();
    }
    if (cur_token->type != RPAREN)
        error("expected )");
}

void parse()
{
    cur_token = get_token(); // считывается левая скобка
    parse_list();
}