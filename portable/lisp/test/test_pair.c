#include <stdio.h>
#include "objects.h"
#include "pair.h"
#include "test.h"
#include "parser.h"
#include "eval.h"

object_t *rplaca(object_t *list);
object_t *rplacd(object_t *list);
object_t *cons(object_t *list);
object_t *car(object_t *list);
object_t *cdr(object_t *list);
object_t *list(object_t *list);

void error(char *str, ...)
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

/*
 *создать объект для выражения (car (quote 5))
 *вычислить объект
 */

void test_invalid_car()
{
    printf("test_invalid_car: ");
    
    int e =5;
    object_t *l = object_new(NUMBER, &e);// 5
    object_t *q = new_pair(object_new(SYMBOL, "QUOTE"),new_pair(l,NULL));// (quote 5)
    object_t *o = new_pair(object_new(SYMBOL, "CAR"),new_pair(q,NULL));// (car (quote 5))
    object_t *res = eval(o, NULL);
    ASSERT(res, ERROR);
}

/**
 * Попытка вернуть элемент из пустого объекта 
 */
void test_car_null()
{
    printf("test_car_null: ");

    object_t *res = car(NULL);
    ASSERT(res, ERROR);
}

/**
 * Попытка вернуть элемент из списка объектов 
 */
void test_car_many_args()
{
    printf("test_car_many_args: ");

    int num1 = 1;
    int num2 = 2;
    object_t *num_obj1 = object_new(NUMBER, &num1);
    object_t *num_obj2 = object_new(NUMBER, &num2);

    object_t *list_with_two_args = new_pair(num_obj1, new_pair(num_obj2, NULL));
    object_t *result = car(list_with_two_args);
    
    ASSERT(result, ERROR); 
}

/*
 *создать объект для выражения (cdr (quote 5))
 *вычислить объект
 */
void test_invalid_cdr()
{    
    printf("test_invalid_cdr:\n");

    int e =5;
    object_t *l = object_new(NUMBER, &e);// 5
    object_t *q = new_pair(object_new(SYMBOL, "QUOTE"),new_pair(l,NULL));// (quote 5)
    object_t *o = new_pair(object_new(SYMBOL, "CDR"),new_pair(q,NULL));// (cdr (quote 5))
    object_t *res = eval(o, NULL);
    ASSERT(res, ERROR);
}

/*
 * Попытка вычисления объекта NULL для cdr
 */
void test_cdr_null()
{
    printf("test_cdr_null:\n");

    object_t *res = cdr(NULL);
    ASSERT(res, ERROR);
}

/*
 * Попытка вычисления объекта из нескольких элеемнтов для cdr
 */
void test_cdr_many_args()
{
    printf("test_cdr_many_args:\n");
    
    int num1 = 1;
    int num2 = 2;
    object_t *num_obj1 = object_new(NUMBER, &num1);
    object_t *num_obj2 = object_new(NUMBER, &num2);

    object_t *list_with_two_args = new_pair(num_obj1, new_pair(num_obj2, NULL));
    object_t *res = cdr(list_with_two_args);
    ASSERT(res, ERROR);
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
 * Тест cons без параметров (CONS)
 * 
 */
void test_cons_noparams()
{
    printf("test_cons_noparams: ");
    object_t *a = new_pair(object_new(SYMBOL, "CONS"), NULL);
    object_t *res = eval(a, NULL);
    ASSERT(res, ERROR);
}

/**
 * Тест cons с одним параметром (cons 5)
 * 
 */
void test_cons_one_param()
{
    printf("test_cons_one_param: ");
    int num = 5;
    object_t *a = new_pair(object_new(SYMBOL, "CONS"), new_pair(object_new(NUMBER, &num), NULL));
    object_t *res = eval(a, NULL);
    ASSERT(res, ERROR);
}

/**
 * Тест cons с 3 параметрами (cons 5 5 5)
 * 
 */
void test_cons_3_params()
{
    printf("test_cons_3_params: ");
    int num = 5;
    object_t *a = new_pair(object_new(SYMBOL, "CONS"),
			   new_pair(object_new(NUMBER, &num),
				    new_pair(object_new(NUMBER, &num), 
					     new_pair(object_new(NUMBER, &num), NULL))));
    object_t *res = eval(a, NULL);
    ASSERT(res, ERROR);
}

/*
 *создать объект для выражения (cons (quote a) 4)
 *вычислить объект
 */
void test_cons2()
{
    printf("test_cons2:\n");

    int ee = 4;
    object_t *qa = new_pair(object_new(SYMBOL, "QUOTE"),
			    new_pair(object_new(SYMBOL, "A"), NULL)); //(quote a)))
    object_t *o = new_pair(object_new(SYMBOL, "CONS"),
			   new_pair(qa, new_pair(object_new(NUMBER, &ee), NULL))); //(cons (quote a) 4))
    object_t *res = eval(o, NULL);
    ASSERT(res->type, PAIR);
    ASSERT(res->u.pair->right->u.value, 4);
}

// ( (5(3 NULL)) (2 NULL) ) -> (2(3 NULL))
void test_rplaca()
{
    printf("test_rplaca: ");

    int a = 5;
    int b = 3;
    int c = 2;
    object_t *p1 = new_pair(object_new(NUMBER, &b), NULL); // (3 NULL)
    object_t *p2 = new_pair(object_new(NUMBER, &a), p1); // (5 (3 NULL))
    object_t *p3 = new_pair(object_new(NUMBER, &c), NULL); // (2 NULL)
    object_t *p4 = new_pair(p2, p3); // ( (5(3 NULL)) (2 NULL) )
    
    object_t *p5 = new_pair(object_new(NUMBER, &c), p1); // (2(3 NULL))
    
    object_t *p6 = p4->u.pair->left; //пара 5(3 NULL)    object_t *list
    object_t *p7 = p6->u.pair->left; //5                 list->u.pair->left
    object_t *p8 = p4->u.pair->right; // (2 NULL)
    object_t *p9 = p8->u.pair->left; // 2                SECOND(params)
    printf("rplaca: ");
    print_counter++;
    PRINT(p4);
    object_t *res = rplaca(p4);
    print_counter++;
    PRINT(res);
    
    ASSERT(FIRST(res)->u.value, 2);
    ASSERT(SECOND(res)->u.value, 3);
    ASSERT(res, p2); // (5 (3 NULL))
    ASSERT(p7->u.value, 5);
    ASSERT(p9->u.value, 2);
}

// нет параметров
void test_rplaca_no_params()
{
    printf("test_rplaca_no_params: ");

    object_t *res = rplaca(NULL);
    ASSERT(res, ERROR);
}

// недостаточно параметров
void test_rplaca_not_enought_params()
{
    printf("test_rplaca_not_enought_params: ");

    int a = 5;
    object_t *p1 = new_pair(object_new(NUMBER, &a), object_new(NUMBER, &a));
    object_t *p2 = new_pair(p1, NULL);
    
    object_t *res = rplaca(p2);
    ASSERT(res, ERROR);
}

// слишком много параметров
void test_rplaca_too_many_params()
{
    printf("test_rplaca_too_many_params: ");

    int a = 5;
    object_t *p1 = new_pair(object_new(NUMBER, &a), object_new(NUMBER, &a));
    object_t *p2 = new_pair(object_new(NUMBER, &a), object_new(NUMBER, &a));
    object_t *p3 = new_pair(p1, p2);
    
    object_t *res = rplaca(p3);
    ASSERT(res, ERROR);
}

// первый параметр не pair
void test_rplaca_first_param_is_not_pair()
{
    printf("test_rplaca_first_param_is_not_pair: ");

    int a = 5;
    int b = 4;
    object_t *p1 = new_pair(object_new(NUMBER, &a), NULL);
    object_t *p2 = new_pair(object_new(NUMBER, &b), p1);
    
    object_t *res = rplaca(p2);
    ASSERT(res, ERROR);
}

// пустой список
void test_rplaca_empty_list()
{
    printf("test_rplaca_empty_list: ");

    int a = 5;
    object_t *p1 = new_pair(object_new(NUMBER, &a), NULL);
    object_t *p2 = new_pair(NULL, p1);
    
    object_t *res = rplaca(p2);
    ASSERT(res, ERROR);
}

// Попытка замены пустой пары
void test_rplacd_null()
{
    printf("test_rplacd_null: ");
    object_t *res = rplacd(NULL);
    ASSERT(res, ERROR);
}

// Попытка замены пары без параметра
void test_rplacd_no_params()
{
    printf("test_rplacd_no_params: ");
    int num = 1;
    object_t *num_obj = object_new(NUMBER, &num);
    object_t *list = new_pair(num_obj, NULL);
    object_t *res = rplacd(list);
    ASSERT(res, ERROR);
}

// Попытка замены при нескольких параметрах
void test_rplacd_many_params()
{
    printf("test_rplacd_many_params: ");
    int num = 1;
    object_t *num_obj = object_new(NUMBER, &num);
    object_t *list = new_pair(num_obj, new_pair(num_obj, new_pair(num_obj, NULL)));
    object_t *res = rplacd(list);
    ASSERT(res, ERROR);
}

// Попытка замены для пустой первой пары
void test_rplacd_empty_list()
{
    printf("test_rplacd_empty_list: ");
    int num = 1;
    object_t *num_obj = object_new(NUMBER, &num);
    object_t *list = new_pair(NULL, new_pair(num_obj, NULL));
    object_t *res = rplacd(list);
    ASSERT(res, ERROR);
}

// Попытка замены для не пары
void test_rplacd_not_pair()
{
    printf("test_rplacd_not_pair: ");
    int num = 1;
    object_t *num_obj = object_new(NUMBER, &num);
    object_t *list = new_pair(num_obj, new_pair(num_obj, NULL));
    object_t *res = rplacd(list);
    ASSERT(res, ERROR);
}

// Создание пары ((1 NULL) (2 NULL)) и замена правой части пары (1 2)
void test_rplacd()
{
    printf("test_rplacd: ");
    int num1 = 1, num2 = 2;
    object_t *left_obj = object_new(NUMBER, &num1);
    object_t *right_obj = object_new(NUMBER, &num2);
    object_t *pair = new_pair(left_obj, NULL);
    object_t *params = new_pair(pair, new_pair(right_obj, NULL));

    object_t *res = rplacd(params);
    
    ASSERT(res->type, PAIR);
    ASSERT(FIRST(res)->type, NUMBER);
    ASSERT(FIRST(res)->u.value, num1);
    ASSERT(TAIL(res)->type, NUMBER);
    ASSERT(TAIL(res)->u.value, num2);
}

/** 
 * Тест функции list
 */
void test_list()
{
    printf("test_list: ");
    int num1 = 1;
    int num2 = 2;
    object_t *p = new_pair(object_new(NUMBER, &num1), 
                new_pair(object_new(NUMBER, &num2), NULL));
    //PRINT(p);
    object_t *res = list(p);
    
    ASSERT(FIRST(res)->type, NUMBER);
    ASSERT(FIRST(res)->u.value, num1);
    ASSERT(FIRST(TAIL(res))->type, NUMBER);
    ASSERT(FIRST(TAIL(res))->u.value, num2);
}

int main()
{
    printf("------------test_pair---------\n");
    init_pair();
    init_eval();
    test_car();//10
    test_car_null();//59
    test_car_many_args();//60
    test_invalid_car();//58
    test_cons();//12
    test_invalid_cdr();//61
    test_cdr_null();//62
    test_cdr_many_args();//63    test_rplaca();
    test_cons_noparams();//64
    test_cons2();//12
    test_cons_one_param();//66
    test_cons_3_params();//65
    test_rplaca();
    test_rplaca_no_params();
    test_rplaca_not_enought_params();
    test_rplaca_too_many_params();
    test_rplaca_first_param_is_not_pair();
    test_rplaca_empty_list();
    test_rplacd();//21
    test_rplacd_null();//79
    test_rplacd_no_params();//81
    test_rplacd_many_params();//80
    test_rplacd_empty_list();//81
    test_rplacd_not_pair();//82
    test_list();
    return 0;
}

