#include <stdio.h>
#include <string.h>
#include "test.h"
#include "objects.h"
#include "lexer.h"
#include "parser.h"

extern token_t *cur_token; // текущий токен
token_t token = {LPAREN, 0, ""};

token_t numbers_tokens[] = {
    {T_NUMBER, 45},
    {T_NUMBER, 65},
    {RPAREN}
};
int count = 0;

token_t symbols_tokens[] = {
    {T_SYMBOL, 0, "A"},
    {T_SYMBOL, 0, "B"},
    {RPAREN}
};

//"1 (2))"
token_t list_tokens[] = {
    {T_NUMBER, 1},
    {LPAREN},
    {T_NUMBER, 2},
    {RPAREN},
    {RPAREN}
};

token_t quote_tokens[] = {
    {QUOTE},
    {T_SYMBOL, 0, "A"},
    {RPAREN}
};

token_t no_rparen_tokens[] = {
    {LPAREN},
    {T_NUMBER, 1},
    {LPAREN},
    {T_NUMBER, 2},
    {RPAREN},
    {END}
};

token_t *tokens;

symbol_t symbols[] = {
    {"A"},
    {"B"},
    {"QUOTE"}
};

char *strupr (char *str);
object_t *parse_list();
object_t *parse();

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

symbol_t *find_symbol(char *str)
{
    for (int i = 0; i < sizeof(symbols) / sizeof(symbol_t); i++)
	if (strcmp(symbols[i].str, str) == 0)
	    return &symbols[i];
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
void test_parse_list_numbers()
{
    printf("test_parse_list_numbers: ");
    count = 0;
    cur_token = &token;
    tokens = numbers_tokens;
    object_t *o = parse_list();
    ASSERT(o->type, PAIR);
    ASSERT(o->u.pair->right->type, PAIR);
    ASSERT(o->u.pair->left->type, NUMBER);
    ASSERT(o->u.pair->left->u.value, 45);
    ASSERT(o->u.pair->right->u.pair->left->u.value, 65);
    ASSERT(o->u.pair->right->u.pair->right, NULL);
}


/** 
 * Создать список из 2 элементов (символов) и проверить корректность создания пар
 */
void test_parse_list_symbols()
{
    printf("test_parse_list_symbols: ");
    count = 0;
    cur_token = &token;
    tokens = symbols_tokens;
    object_t *o = parse_list();
    ASSERT(o->type, PAIR);
    ASSERT(o->u.pair->right->type, PAIR);
    ASSERT(o->u.pair->left->type, SYMBOL);
    ASSERT(strcmp(o->u.pair->left->u.symbol->str, "A"), 0);
    ASSERT(strcmp(o->u.pair->right->u.pair->left->u.symbol->str, "B"), 0);
    ASSERT(o->u.pair->right->u.pair->right, NULL);
}


/** 
 * Создать список "1 (2))" и проверить корректность создания пар
 */
void test_parse_list_list()
{
    printf("test_parse_list_list: ");
    count = 0;
    cur_token = &token;
    tokens = list_tokens;
    object_t *o = parse_list();
    ASSERT(o->type, PAIR);
    ASSERT(o->u.pair->right->type, PAIR);
    ASSERT(o->u.pair->left->type, NUMBER);
    ASSERT(o->u.pair->left->u.value, 1);
    ASSERT(o->u.pair->right->u.pair->left->type, PAIR);
    ASSERT(o->u.pair->right->u.pair->left->u.pair->left->u.value, 2);
    ASSERT(o->u.pair->right->u.pair->left->u.pair->right, NULL);
    ASSERT(o->u.pair->right->u.pair->right, NULL);
}


/** 
 * Создать "'a" и проверить корректность создания пар
 * (quote a)
 */
void test_parse_quote()
{
    printf("test_parse_quote: ");
    count = 0;
    cur_token = &token;
    tokens = quote_tokens;
    object_t *o = parse();
    ASSERT(o->type, PAIR);
    ASSERT(o->u.pair->right->type, PAIR);
    ASSERT(o->u.pair->left->type, SYMBOL);
    ASSERT(strcmp(o->u.pair->left->u.symbol->str, "QUOTE"), 0);
    ASSERT(strcmp(o->u.pair->right->u.pair->left->u.symbol->str, "A"), 0);
    ASSERT(o->u.pair->right->u.pair->right, NULL);
}

/** 
 * Создать "'a)" и проверить корректность создания пар
 * ((quote a))
 */
void test_parse_list_quote()
{
    printf("test_parse_list_quote: ");
    count = 0;
    cur_token = &token;
    tokens = quote_tokens;
    object_t *o = parse_list();
    ASSERT(o->type, PAIR);
    ASSERT(o->u.pair->right, NULL);
    ASSERT(o->u.pair->left->type, PAIR);
    ASSERT(strcmp(o->u.pair->left->u.pair->left->u.symbol->str, "QUOTE"), 0);
    ASSERT(strcmp(o->u.pair->left->u.pair->right->u.pair->left->u.symbol->str, "A"), 0);
    ASSERT(o->u.pair->left->u.pair->right->u.pair->right, NULL);
}

/** 
 * Создать "(1(2)" и проверить ошибку при создании пар
 */
void test_parse_no_rparen()
{
    printf("test_parse_no_rparen: ");
    count = 0;
    cur_token = &token;
    tokens = no_rparen_tokens;
    object_t *o = parse();
    ASSERT(o, ERROR);
}

int main()
{
    printf("------------test_parser------------\n");
    test_strupr();
    test_parse_list_numbers();
    test_parse_list_symbols();
    test_parse_list_list();
    test_parse_quote();
    test_parse_list_quote();
    test_parse_no_rparen();
    return 0;
}
