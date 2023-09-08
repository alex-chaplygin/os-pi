#include <stdio.h>
#include "test.h"
#include "objects.h"
#include "lexer.h"

extern token_t *cur_token; // текущий токен
token_t token = {LPAREN, 0, ""};

token_t tokens[] = {
  {T_NUMBER, 45, ""},
  {T_NUMBER, 65},
  {RPAREN}
};
int count = 0;

object_t *parse_list();
symbol_t *find_symbol(char *str)
{
  return NULL;
} 

/** 
 * Возвращать лексемы в следующей последовательности: 45 65 )
 */
token_t *get_token()
{
  return &tokens[count++];
}


/** 
 * Создать список из 2 элементов (чисел) и проверить корректность создания пар
 */
void test_parse_list()
{
  cur_token = &token; 
  object_t *o = parse_list();
  ASSERT(o->type, PAIR);
  ASSERT(o->u.pair->right->type, PAIR);
  ASSERT(o->u.pair->left->type, NUMBER);
  ASSERT(o->u.pair->left->u.value, 45);
  ASSERT(o->u.pair->right->u.pair->left->u.value, 65);
  ASSERT(o->u.pair->right->u.pair->right, NULL);
}

void main()
{
  test_parse_list();
}

