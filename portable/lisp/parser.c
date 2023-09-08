#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "objects.h"

token_t *cur_token; // текущий токен

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
object_t *parse();

// Обработка кавычки
// 'a -> (quote a)
// '(4 5 6) -> (quote (4 5 6))
/*list_t *parse_quote()
{
    list_t *list = NULL;
    object_t *o = parse();
    if (o->type == NUMBER)
        error("quote Number\n");
    else 
    {
        list_add(&list, object_new(ATOM, "QUOTE"));
        list_add(&list, o);
    }
    return list;
    }*/

/** 
 * Обработка списка без левой скобки
 * Обработка заканчивается когда встречает правую скобку или конец ввода
 *  
 * @return указатель на объект списка
 */
object_t *parse_list()
{
  int val;
  cur_token = get_token();
  if (cur_token->type == RPAREN)
    return NULL;
  if (cur_token->type == T_NUMBER) {
    val = cur_token->value;
    return new_pair(object_new(NUMBER, &val), parse_list());
  }
    /*    else if (cur_token->type == T_ATOM)
      list_add(&list, object_new(ATOM, strupr(cur_token->str)));
    else if  (cur_token->type == LPAREN)
      list_add(&list, object_new(LIST, parse_list()));
    else if (cur_token->type == QUOTE)
      list_add(&list, object_new(LIST, parse_quote()));
    else if (cur_token->type == INVALID)
      error("expected number or atom");
        cur_token = get_token();
    if (cur_token->type != RPAREN)
    error("expected )");*/
}



// Обработка выражения 
// (1 2)
// 12
// ABC
/*object_t *parse()
{   
    object_t *el; // создаем новый элемент
    cur_token = get_token(); // считывается левая скобка
    if (cur_token->type == T_NUMBER) // считывается число
        return object_new(NUMBER, &cur_token->value);
    else if (cur_token->type == T_ATOM)//считывается атом
        return object_new(SYMBOL, find_symbol(strupr(cur_token->str)));
    else if (cur_token->type == LPAREN)
      return parse_list();
    else if (cur_token->type == QUOTE)
        return object_new(LIST, parse_quote());
	}*/
