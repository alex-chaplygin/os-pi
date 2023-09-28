#include <stdio.h>
#include "objects.h"
#include "eval.h"
#include "test.h"

extern object_t *t;
extern object_t *nil;

object_t * make_env(object_t *args, object_t *values);
int find_in_env(object_t *env, object_t *sym, object_t **res);
int is_lambda(object_t *list);

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
    object_t *res = eval(o, NULL);
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
    object_t *res = eval(o, NULL);
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
    printf("test_cond: ");
    int n1 = 1;
    int n2 = 2;
    object_t *p1 = new_pair(nil, new_pair(object_new(NUMBER, &n1), NULL));
    object_t *p2 = new_pair(t, new_pair(object_new(NUMBER, &n2), NULL));
    object_t *l = new_pair(object_new(SYMBOL, "COND"), new_pair(p1, new_pair(p2, NULL)));
    object_t *res = eval(l, NULL);
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
    
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(params, new_pair(q, NULL)));

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
    
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(params, new_pair(q, NULL)));

    int i = is_lambda(list);
    ASSERT(i, 0);
}

object_t *create_env()
{
  int num1 = 1;
  int num2 = 2;
  object_t *arg_y = object_new(SYMBOL, "Y");
  object_t *arg_x = object_new(SYMBOL, "X");
  object_t *arg_1 = object_new(NUMBER, &num1);
  object_t *arg_2 = object_new(NUMBER, &num2);
  object_t *args = new_pair(arg_x, new_pair(arg_y, NULL));
  object_t *values = new_pair(arg_1, new_pair(arg_2, NULL));
  return make_env(args,values);
}


/**
 * Создать окружение
 * аргументы (x y)
 * значения (1 2)
 * Проверить результат = ((X 1) (Y 2))
 */
void test_make_env()
{
  printf("test_make_env: ");
  object_t *env = create_env();
  object_t *p1 = FIRST(env);
  object_t *p2 = SECOND(env);  
  ASSERT(p1->type, PAIR);
  ASSERT(p2->type, PAIR);
  ASSERT(FIRST(p1)->u.symbol, find_symbol("X"));
  ASSERT(SECOND(p1)->u.value, 1);
  ASSERT(FIRST(p2)->u.symbol, find_symbol("Y"));
  ASSERT(SECOND(p2)->u.value, 2);      
}

/**
 * Создать окружение
 * аргументы (x y)
 * значения (1 2)
 * Проверить переменную Y
 */
void test_find_in_env()
{
  printf("test_find_in_env: ");
  object_t *env = create_env();
  object_t *res;
  int result = find_in_env(env, object_new(SYMBOL, "Y"), &res);
  ASSERT(result, 1);
  ASSERT(res->type, NUMBER);
  ASSERT(res->u.value, 2);
}

/**
 * Создать функцию (defun null (x) (eq x '()))
 * Проверить значение символа null
 */
void test_defun()
{
    printf("test_defun: ");
    object_t *body = new_pair(object_new(SYMBOL, "EQ"),
			      new_pair(object_new(SYMBOL, "X"),
				       new_pair(NULL, NULL)));
    object_t *args = new_pair(object_new(SYMBOL, "X"), NULL);
    object_t *func = new_pair(object_new(SYMBOL, "DEFUN"),
			      new_pair(object_new(SYMBOL, "NULL"),
				       new_pair(args,
						new_pair(body, NULL))));
    object_t *res = eval(func, NULL);
    ASSERT(res->type, SYMBOL);
    symbol_t *null = find_symbol("NULL");
    ASSERT(res->u.symbol, null);
    ASSERT(null->value->type, PAIR);
    ASSERT(null->value->u.pair->left->u.symbol, find_symbol("LAMBDA"));
}

int main()
{
    printf("------------test_eval_int---------\n");
    init_eval();
    test_car();
    test_cons();
    test_is_lambda();
    //xtest_is_lambda_not_symbol();
    test_cond();
    test_make_env();
    test_find_in_env();
    test_defun();
    return 0;
}

