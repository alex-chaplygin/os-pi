#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include "test.h"
#include "objects.h"
#include "alloc.h"
#include "lexer.h"
#include "parser.h"
#include "eval.h"

extern token_t *cur_token; // текущий токен
int token_error;
token_t token = {LPAREN, 0, ""};

token_t atoms_tokens[] = {
    {T_NUMBER, 45},
    {T_NUMBER, 65},
    {T_SYMBOL, 0, "A"},
    {T_SYMBOL, 0, "B"},
    {T_STRING, 0, "StrA"},
    {T_STRING, 0, "StrB"},
    {RPAREN}
};
int count = 0;

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

token_t backquote_tokens[] = {
    {BACKQUOTE},
    {T_SYMBOL, 0, "A"},
    {RPAREN}
};

token_t comma_tokens[] = {
    {COMMA},
    {T_SYMBOL, 0, "A"},
    {RPAREN},
    {END}
};

token_t back_comma_tokens[] = {
    {BACKQUOTE},
    {LPAREN},
    {COMMA},
    {T_SYMBOL, 0, "A"},
    {RPAREN},
    {END}
};

token_t back_comma_at_tokens[] = {
    {LPAREN},
    {BACKQUOTE},
    {LPAREN},
    {COMMA_AT},
    {T_SYMBOL, 0, "A"},
    {RPAREN},
    {RPAREN},
    {END}
};

token_t no_rparen_tokens[] = {
    {LPAREN},
    {T_NUMBER, 1},
    {LPAREN},
    {T_NUMBER, 2},
    {RPAREN},
    {END}
};

token_t token_list[] = {
    {T_SYMBOL, 0,  "x"},
    {LPAREN},
    {T_SYMBOL, 0, "y"},
    {RPAREN},
    {T_SYMBOL, 0, "z"},
    {RPAREN}
};

token_t tok_inv[] = 
    {
	{LPAREN},
	{T_NUMBER, 1},
	{T_SYMBOL, 0, "S"},
	{INVALID},
	{T_NUMBER, 22},
	{RPAREN}
    };

token_t tok_array[] = {
    {SHARP},
    {LPAREN},
    {T_NUMBER, 1},
    {T_NUMBER, 2},
    {T_NUMBER, 3},
    {RPAREN}
};

token_t tok_array_error[] = {
    {SHARP},
    {SHARP},
    {LPAREN},
    {T_NUMBER, 1},
    {T_NUMBER, 2},
    {T_NUMBER, 3},
    {RPAREN}
};

token_t tok_array_error_paren[] = {
    {SHARP},
    {RPAREN},
    {T_NUMBER, 1},
    {T_NUMBER, 2},
    {T_NUMBER, 3},
    {END}
};

token_t tok_inner_array[] = {
    {SHARP},
    {LPAREN},
    {T_NUMBER, 1},
    {SHARP},
    {LPAREN},
    {T_NUMBER, 2},
    {T_NUMBER, 3},
    {RPAREN},
    {T_NUMBER, 4},
    {RPAREN},
};

token_t tok_array_list[] = {
    {LPAREN},
    {SHARP},
    {LPAREN},
    {T_NUMBER, 1},
    {T_NUMBER, 2},
    {T_NUMBER, 3},
    {RPAREN},
    {RPAREN}
}; 

token_t tok_inv_quote[] = {
    {QUOTE},
    {INVALID}
};

token_t tok_quote_number[] = {
    {QUOTE},
    {T_NUMBER, 5}
};

token_t tok_number_dot_number[] = {
    {LPAREN},
    {T_NUMBER, 1},
    {DOT},
    {T_NUMBER, 2},
    {RPAREN}
};

token_t no_rparen_tokens_lists[] = {
    {LPAREN},
    {LPAREN},
    {T_SYMBOL, 0, "a"},
    {T_SYMBOL, 0, "b"},
    {LPAREN},
    {T_NUMBER, 1},
    {T_NUMBER, 2},
    {RPAREN},
    {T_SYMBOL, 0, "e"},
    {T_SYMBOL, 0, "f"},
    {RPAREN},
    {END}
};

token_t no_rparen_tokens_arrays[] = {
    {LPAREN},
    {LPAREN},
    {T_SYMBOL, 0, "a"},
    {T_SYMBOL, 0, "b"},
    {SHARP},
    {LPAREN},
    {T_NUMBER, 1},
    {SHARP},
    {LPAREN},
    {T_NUMBER, 2},
    {LPAREN},
    {T_NUMBER, 3},
    {T_NUMBER, 4},
    {RPAREN},
    {T_NUMBER, 5},
    {RPAREN},
    {RPAREN},
    {T_SYMBOL, 0, "c"},
    {T_SYMBOL, 0, "d"},
    {RPAREN},
    {END}
};

token_t str_tokens[] = {
    {T_STRING,0,"Str"}
};

token_t end_tokens[] = {
    {END}
};

token_t tok_list_expected_rparen[] = {
    {LPAREN},
    {T_NUMBER, 1},
    {DOT},
    {T_NUMBER, 1},
    {T_NUMBER, 1},
    {END}
};

token_t tok_list_invalid_token[] = {
    {LPAREN},
    {INVALID},
    {RPAREN}
};
    
token_t *tokens;
jmp_buf jmp_env;

char *strupr (char *str);
object_t parse_list();
object_t parse();

void error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
}

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
 * Создать список из 2 чисел, 2 символов и 2 строк и проверить корректность создания пар
 */
void test_parse_list_atoms()
{
    printf("test_parse_list_atoms: ");
    count = 0;
    cur_token = &token;
    tokens = atoms_tokens;
    object_t o = parse_list();
    printf("o = ");
    PRINT(o);

    ASSERT(TYPE(o), PAIR);
  
    ASSERT(TYPE(TAIL(o)), PAIR);
    ASSERT(TYPE(FIRST(o)), NUMBER);
    ASSERT(get_value(FIRST(o)), 45);
    ASSERT(TYPE(SECOND(o)), NUMBER);
    ASSERT(get_value(SECOND(o)), 65);
    
    o = TAIL(TAIL(o));
    
    ASSERT(TYPE(TAIL(o)), PAIR);
    ASSERT(TYPE(FIRST(o)), SYMBOL);
    ASSERT(strcmp(GET_SYMBOL(FIRST(o))->str, "A"), 0);
    ASSERT(TYPE(SECOND(o)), SYMBOL);
    ASSERT(strcmp(GET_SYMBOL(SECOND(o))->str, "B"), 0);
    
    o = TAIL(TAIL(o));
    
    ASSERT(TYPE(TAIL(o)), PAIR);
    ASSERT(TYPE(FIRST(o)), STRING);
    ASSERT(strcmp(GET_STRING(FIRST(o))->data, "StrA"), 0);
    ASSERT(TYPE(SECOND(o)), STRING);
    ASSERT(strcmp(GET_STRING(SECOND(o))->data, "StrB"), 0);

    ASSERT(TAIL(TAIL(o)), NULLOBJ);
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
    object_t o = parse_list();
    ASSERT(TYPE(o), PAIR);
    ASSERT(TYPE(GET_PAIR(o)->right), PAIR);
    ASSERT(TYPE(GET_PAIR(o)->left), NUMBER);
    ASSERT(get_value(GET_PAIR(o)->left), 1);
    ASSERT(TYPE(GET_PAIR(GET_PAIR(o)->right)->left), PAIR);
    ASSERT(get_value(GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->left)->left), 2);
    ASSERT(GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->left)->right, NULLOBJ);
    ASSERT(GET_PAIR(GET_PAIR(o)->right)->right, NULLOBJ);
}


/**
 * Создать "'a" и проверить корректность создания пар
 * (quote a)
 */
void test_parse_quote(token_t *toks, char* sym) 
{ 
    printf("test_parse_quote: %s ", sym); 
    count = 0;
    cur_token = &token; 
    tokens = toks;
    object_t o = parse(); 
    ASSERT(TYPE(o), PAIR); 
    ASSERT(TYPE(GET_PAIR(o)->right), PAIR); 
    ASSERT(TYPE(GET_PAIR(o)->left), SYMBOL); 
    ASSERT(strcmp(GET_SYMBOL((GET_PAIR(o)->left))->str, sym), 0);
    ASSERT(strcmp(GET_SYMBOL((GET_PAIR(GET_PAIR(o)->right)->left))->str, "A"), 0); 
    ASSERT(GET_PAIR(GET_PAIR(o)->right)->right, NULLOBJ); 
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
    object_t o = parse_list(); 
    ASSERT(TYPE(o), PAIR); 
    ASSERT(GET_PAIR(o)->right, NULLOBJ); 
    ASSERT(TYPE(GET_PAIR(o)->left), PAIR); 
    ASSERT(strcmp(GET_SYMBOL((GET_PAIR(GET_PAIR(o)->left)->left))->str, "QUOTE"), 0); 
    ASSERT(strcmp(GET_SYMBOL((GET_PAIR(GET_PAIR(GET_PAIR(o)->left)->right)->left))->str, "A"), 0); 
    ASSERT(GET_PAIR(GET_PAIR(GET_PAIR(o)->left)->right)->right, NULLOBJ); 
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

    if (setjmp(jmp_env) == 0) {
        // Попробуем выполнить парсинг
        object_t o = parse();
        // Если нет ошибки - тест провален
        FAIL;
    } else // Ошибка была - тест прошел
        OK; 
} 

/**
 * Создать "((a b (1 2) e f)" и проверить ошибку при создании многоуровневого списка
 */
void test_parse_no_rparen_lists() 
{ 
    printf("test_parse_no_rparen_lists: "); 
    count = 0; 
    cur_token = &token; 
    tokens = no_rparen_tokens_lists; 
    if (setjmp(jmp_env) == 0) {
        object_t o = parse();
        FAIL;
    } else
        OK;
} 


/**
 * Создать "((a b #(1 #(2 (3 4) 5)) c d )" и проверить ошибку при создании многоуровневых массивов
 */
void test_parse_no_rparen_arrays() 
{ 
    printf("test_parse_no_rparen_arrays: "); 
    count = 0; 
    cur_token = &token; 
    tokens = no_rparen_tokens_arrays; 
    if (setjmp(jmp_env) == 0) {
        object_t o = parse();
        FAIL;
    } else
        OK;
} 

/**
 * Создать список "(x (y) z)" и проверить корректность создания пар
 */
void test_parse_inner_list() 
{ 
    printf("test_parse_inner_list:"); 
    count = 0; 
    cur_token = &token; 
    tokens = token_list; 
    object_t o = parse_list(); 
    ASSERT(TYPE(o), PAIR); 
    ASSERT(TYPE(GET_PAIR(o)->left), SYMBOL); 
    ASSERT(strcmp(GET_SYMBOL((GET_PAIR(o)->left))->str, "X"), 0);  
    ASSERT(TYPE(GET_PAIR(o)->right), PAIR); 
    ASSERT(TYPE(GET_PAIR(GET_PAIR(o)->right)->left), PAIR);
    ASSERT(strcmp(GET_SYMBOL((GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->left)->left))->str, "Y"), 0);  
    ASSERT(GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->left)->right, NULLOBJ); 
    ASSERT(TYPE(GET_PAIR(GET_PAIR(o)->right)->right), PAIR);
    ASSERT(TYPE(GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->right)->left), SYMBOL);
    ASSERT(strcmp(GET_SYMBOL((GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->right)->left))->str, "Z"), 0);  
    ASSERT(GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->right)->right, NULLOBJ); 
} 

/**
 * Тестируем неверный символ внутри списка
 */
void test_parse_invalid() 
{ 
    printf("test_parse_invalid: "); 
    count = 0; 
    tokens = tok_inv; 
    if (setjmp(jmp_env) == 0) {
        object_t o = parse();
        FAIL;
    } else
        OK;
} 

/**
 * Тестируем неверный символ после кавычки
 */
void test_parse_invalid_quote() 
{ 
    printf("test_parse_invalid_quote: "); 
    count = 0; 
    cur_token = &token; 
    tokens = tok_inv_quote; 
    if (setjmp(jmp_env) == 0) {
        object_t o = parse();
        FAIL;
    } else
        OK;
} 

/**
 * Тестируем массив #(1 2 3)
 * На выходе: #(1 2 3)
 */
void test_parse_array() 
{ 
    printf("test_parse_array: "); 
    count = 0; 
    tokens = tok_array; 
    object_t o = parse(); 
    ASSERT(TYPE(o), ARRAY);
    array_t *a = GET_ARRAY(o); 
    ASSERT(get_value(a->data[0]), 1); 
    ASSERT(get_value(a->data[1]), 2); 
    ASSERT(get_value(a->data[2]), 3); 
} 

/**
 * Тестируем массив ##(1 2 3)
 * На выходе: ошибка
 */
void test_parse_array_error() 
{ 
    printf("test_parse_array_error: "); 
    count = 0; 
    tokens = tok_array_error; 
    if (setjmp(jmp_env) == 0) {
        object_t o = parse();
        FAIL;
    } else
        OK;
} 

/**
 * Тестируем массив #(1 2 3
 * На выходе: ошибка
 */
void test_parse_array_error_paren() 
{ 
    printf("test_parse_array_error_paren: "); 
    count = 0; 
    tokens = tok_array_error_paren; 
    if (setjmp(jmp_env) == 0) {
        object_t o = parse();
        FAIL;
    } else
        OK;
} 

/**
 * Тестируем вложенныий в массив массив #(1 #(2 3) 4)
 * На выходе: #(1 #(2 3) 4)
*/
void test_parse_inner_array() 
{ 
    printf("test_parse_inner_array: "); 
    count = 0; 
    tokens = tok_inner_array; 
    object_t o = parse(); 
    array_t *a1 = GET_ARRAY(o); 
    object_t o2 = a1->data[1]; 
    array_t *a2 = GET_ARRAY(o2); 

    ASSERT(TYPE(o), ARRAY); 
    ASSERT(get_value(a1->data[0]), 1); 
    ASSERT(TYPE(o2), ARRAY); 
    ASSERT(get_value(a2->data[0]), 2); 
    ASSERT(get_value(a2->data[1]), 3); 
    ASSERT(get_value(a1->data[2]), 4); 
} 

/**
 * Тестируем массив в списке (#(1 2 3))
 * На выходе: (#(1 2 3))
 */
void test_parse_array_list() 
{ 
    printf("test_parse_array_list: "); 
    count = 0; 
    tokens = tok_array_list; 
    object_t o = GET_PAIR(parse())->left; 
    ASSERT(TYPE(o), ARRAY); 
    array_t *a = GET_ARRAY(o); 
    ASSERT(get_value(a->data[0]), 1); 
    ASSERT(get_value(a->data[1]), 2); 
    ASSERT(get_value(a->data[2]), 3); 
} 

/**
 * Тестируем `(,a)
 * Должно получиться: (BACKQUOTE ((COMMA A)))
 */
void test_parse_backquote_comma() 
{ 
    printf("test_parse_backquote_comma: "); 
    count = 0; 
    cur_token = &token; 
    tokens = back_comma_tokens; 
    object_t o = parse(); 
    ASSERT(strcmp(GET_SYMBOL(GET_PAIR(o)->left)->str, "BACKQUOTE"), 0);  
    o = GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->left)->left;
    ASSERT(strcmp(GET_SYMBOL(GET_PAIR(o)->left)->str, "COMMA"), 0);  
    ASSERT(strcmp(GET_SYMBOL(GET_PAIR(GET_PAIR(o)->right)->left)->str, "A"), 0);  
} 

/**
 * Тестируем (`(,@a))
 * Должно получиться: ((BACKQUOTE ((COMMA-AT A))))
 */
void test_parse_backquote_comma_at() 
{ 
    printf("test_parse_backquote_comma_at: "); 
    count = 0; 
    cur_token = &token; 
    tokens = back_comma_at_tokens; 
    object_t o = GET_PAIR(parse())->left; 
    printf("o = "); 
    PRINT(o); 
    ASSERT(strcmp(GET_SYMBOL(GET_PAIR(o)->left)->str, "BACKQUOTE"), 0);  
    o = GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->left)->left;
    ASSERT(strcmp(GET_SYMBOL(GET_PAIR(o)->left)->str, "COMMA-AT"), 0);  
    ASSERT(strcmp(GET_SYMBOL(GET_PAIR(GET_PAIR(o)->right)->left)->str, "A"), 0);
} 

/**
 * Тестируем выражение '5
 */
void test_parse_quote_number() 
{ 
    printf("test_parse_quote_number: "); 
    count = 0; 
    cur_token = &token; 
    tokens = tok_quote_number; 
    object_t o = parse(); 
    ASSERT(get_value(GET_PAIR(GET_PAIR(o)->right)->left), 5);
} 

/**
 * Тестируем точечную пару (1 . 2)
 */
void test_parse_number_dot_number() 
{ 
    printf("test_parse_number_dot_number: "); 
    count = 0; 
    cur_token = &token; 
    tokens = tok_number_dot_number; 
    object_t o = parse();
    ASSERT(get_value(GET_PAIR(o)->left), 1); 
    ASSERT(get_value(GET_PAIR(o)->right), 2); 
} 

/**
 * Тест строки
 */
void test_parse_string() 
{ 
    printf("test_parse_string:"); 
    count = 0; 
    cur_token = &token; 
    tokens = str_tokens; 
    object_t o = parse();
    ASSERT(TYPE(o), STRING); 
    ASSERT(strcmp(GET_STRING(o)->data,"Str"),0); 
} 

/**
 * Тест конец потока, без объектов
 */
void test_parse_end() 
{ 
    printf("test_parse_end:"); 
    count = 0; 
    cur_token = &token; 
    tokens = end_tokens; 
    object_t o = parse();
    ASSERT(o, NOVALUE);
} 

/**
 * Тестирование неверной точечной пары (1 . )
 */
void test_parse_list_expected_rparen() 
{ 
    printf("test_parse_list_expected_rparen: "); 
    count = 0; 
    cur_token = &token; 
    tokens = tok_list_expected_rparen; 
    if (setjmp(jmp_env) == 0) {
        object_t res = parse(); 
        FAIL;
    } else
        OK;
} 

/**
 * Тестирование токена кторого нет в возможных токенах
 */
void test_parse_list_invalid_token() 
{ 
    printf("test_parse_list_invalid_token: "); 
    count = 0; 
    cur_token = &token; 
    tokens = tok_list_invalid_token; 
    if (setjmp(jmp_env) == 0) {
        object_t res = parse(); 
        FAIL;
    } else
        OK;
} 

/**
 * Тест ошибки лексера
 */
void test_parse_token_error() 
{ 
    printf("test_parse_token_error:"); 
    count = 0; 
    cur_token = &token; 
    tokens = end_tokens; 
    token_error = 1; 
    if (setjmp(jmp_env) == 0) {
        object_t o = parse();
        FAIL;
    } else
        OK;
    token_error = 0; 
} 

/*
 * условие   | правильный класс       | неправильный класс
 * атом      | 1 число                | 2 invalid
 *           | 3 символ               | 
 *           | 4 строка               |
 * ---------------------------------------------------------------------------
 * список    | 5 правильный список    | 6 список без закрывающей скобки
 *           | 7 правильный           | 8 пустой список без открывающей скобки
 *           |   многоуровневый список| 10 многоуровневый список с несоответствием
 *           | 9 массив внутри списка |    количества открывающих и закрывающих скобок
 *           |                        | 11 вложенный массив с несоответствием
 *           |                        |    количества открывающих и закрывающих скобок
 * ---------------------------------------------------------------------------
 *           | 12 символ(функция,var) |
 * Цитата    | 13 число               |
 * или       | 14 массив              |  
 *           | 15 строка              |
 * квазицита | 16 список              |
 *           | 28 квазицитирование    |
 *           |    внутри квазицитиро- |
 *           |    вания               |
 * -------------------------------------------------------------------------
 * Запятая   | 17 находится внутри    | 
 *           | выражения с backquote  |
 * -------------------------------------------------------------------------
 * Массив    | 18 правильный массив   | 19 отсутсвуют открывающая скобки
 *           | 20 правильный          | 21 больше одного символа "#" в начале массива
 *           |    многоуровн. массив  | 23 многоуровневый массив с несоответствием количества
 *           | 22 правильный вложенный|    открывающих и закрывающих скобок
 *           |    список              | 24 вложенный список с несоответствием количества
 *           |                        |    открывающих и закрывающих скобок
 * --------------------------------------------------------------------------
 * Запятая и | 25 находится внутри    |
 * @         |   выражения с backquote| 
 *           |  и применяется к списку|
 */
int main()
{
    printf("------------test_parser------------\n");
    init_regions();
    init_objects();
    test_strupr();
    test_parse_list_atoms(); // 1, 3, 4, 5
    test_parse_list_list();
    test_parse_quote(quote_tokens, "QUOTE"); //12
    test_parse_quote(backquote_tokens, "BACKQUOTE");//12
    test_parse_quote(comma_tokens, "COMMA"); //17
    test_parse_list_quote(); //16
    test_parse_no_rparen();  //6
    test_parse_no_rparen_lists(); //10
    test_parse_no_rparen_arrays(); //11 23
    test_parse_inner_list(); //7
    test_parse_invalid();    //2
    test_parse_invalid_quote(); //2
    test_parse_array(); //18 
    test_parse_array_list(); //9
    test_parse_inner_array(); // 20
    test_parse_array_error(); //21*/
    test_parse_array_error_paren(); //19
    test_parse_backquote_comma(); //17
    test_parse_quote_number(); //13
    test_parse_backquote_comma_at(); //25
    test_parse_number_dot_number();
    test_parse_string();
    test_parse_end();
    test_parse_token_error();
    test_parse_list_expected_rparen();
    test_parse_list_invalid_token();
    return 0;
}
