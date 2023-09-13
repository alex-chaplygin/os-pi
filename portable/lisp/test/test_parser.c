#include <stdio.h>
#include <string.h>
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

char *strupr (char *str);
object_t *parse_list();
symbol_t *find_symbol(char *str)
{
    return NULL;
} 

void test_strupr ()
{
    char str_in[] = "a1a a!aa bb r4n\n";
    char str_ref[] = "A1A A!AA BB R4N\n";
    printf("test_strupr: ");
    strupr (str_in);
    ASSERT (strcmp (str_in, str_ref), 0);
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
    printf("test_parse_list: ");
    cur_token = &token; 
    object_t *o = parse_list();
    ASSERT(o->type, PAIR);
    ASSERT(o->u.pair->right->type, PAIR);
    ASSERT(o->u.pair->left->type, NUMBER);
    ASSERT(o->u.pair->left->u.value, 45);
    ASSERT(o->u.pair->right->u.pair->left->u.value, 65);
    ASSERT(o->u.pair->right->u.pair->right, NULL);
}

int main()
{
    test_strupr();
    test_parse_list();
    return 0;
}
