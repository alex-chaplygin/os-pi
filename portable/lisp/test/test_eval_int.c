#include <stdio.h>
#include "objects.h"
#include "eval.h"
#include "test.h"
extern object_t *t;
extern object_t *nil;

void error(char *str)
{
  printf("%s", str);
}

/**
 * создать объект для выражения (car (quote (5)))
 * вычислить объект 
 */
void test_car()
{
    printf("test_car: ");

    int e = 5;
    object_t *l = new_pair(object_new(NUMBER, &e), NULL);
    object_t *q = new_pair(object_new(SYMBOL, "QUOTE"),
			   new_pair(l, NULL));
    object_t *o = new_pair(object_new(SYMBOL, "CAR"),
			   new_pair(q, NULL));
    object_t *res = eval(o);
    ASSERT(res->type, NUMBER);
    ASSERT(res->u.value, 5);
}

/**
 * создать объект для выражения (cons (quote a) (quote (5)))
 * вычислить объект (A 5)
 */

void test_cons()
{
    printf("test_cons: ");
    int e = 5;
    object_t *l = new_pair(object_new(NUMBER, &e), NULL);
    object_t *q = new_pair(object_new(SYMBOL, "QUOTE"),
			   new_pair(l, NULL));
    object_t *qa = new_pair(object_new(SYMBOL, "QUOTE"),
			    new_pair(object_new(SYMBOL, "A"), NULL));
    object_t *o = new_pair(object_new(SYMBOL, "CONS"),
			    new_pair(qa, new_pair(q, NULL)));
    object_t *res = eval(o);
    ASSERT(FIRST(res)->type, SYMBOL);
    ASSERT(SECOND(res)->type, NUMBER);
    ASSERT(SECOND(res)->u.value, 5);
}

/**
 * Создать пары объеектов типа(t/nil,number)
 * (cond (nil 1)
        (T 2)  )
 * Обработать их функцией cond и проверить результат
*/
void test_cond()
{
    printf("test_cons: ");
    int n1 = 1, n2 = 2;
    object_t *p1 = new_pair(nil, new_pair(object_new(NUMBER, &n1), NULL));
    object_t *p2 = new_pair(t, new_pair(object_new(NUMBER, &n2), NULL));
    object_t *l = new_pair(object_new(SYMBOL, "COND"), new_pair(p1, new_pair(p2, NULL)));
    object_t *res = eval(l);
    printf("res: ");
    print_obj(res);
    printf("\n");
    ASSERT(res->type, NUMBER);
    ASSERT(res->u.value, 2);
}

/**
 * Создать объект для выражения (lambda (a, b) (atom (car (a, b))))
 * Вызвать функцию is_lambda
 * Проверить результат = 1
 */
void test_is_lambda()
{
    printf("test_is_lambda: ");
    object_t *p1 = object_new(SYMBOL, "a");
    object_t *p2 = object_new(SYMBOL, "b");
    object_t *params = new_pair(p1, new_pair(p2, NULL));

    object_t *q = new_pair(object_new(SYMBOL, "ATOM"),
        new_pair(object_new(SYMBOL, "CAR"), new_pair(params, NULL)));
    
    object_t *list = new_pair(params, new_pair(q, NULL));

    int i = is_lambda(list);
    ASSERT(i, 1);
}

/**
 * Создать объект для некорректного выражения (lambda (a, 5) (atom (car (a, 5))))
 * Вызвать функцию is_lambda
 * Проверить результат = 0
 */
void test_is_lambda_not_symbol()
{
    printf("test_is_lambda_not_symbol: ");
    int num = 5;
    object_t *p1 = object_new(SYMBOL, "a");
    object_t *p2 = object_new(NUMBER, &num);
    object_t *params = new_pair(p1, new_pair(p2, NULL));

    object_t *q = new_pair(object_new(SYMBOL, "ATOM"),
        new_pair(object_new(SYMBOL, "CAR"), new_pair(params, NULL)));
    
    object_t *list = new_pair(params, new_pair(q, NULL));

    int i = is_lambda(list);
    ASSERT(i, 0);
}

int main()
{
    printf("------------test_eval_int---------\n");
    init_eval();
    test_car();
    test_cons();
    test_is_lambda();
    test_is_lambda_not_symbol();
    test_cond();
    
    return 0;
}

