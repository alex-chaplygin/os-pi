#include <stdio.h>
#include <setjmp.h>
#include "objects.h"
#include "alloc.h"
#include "pair.h"
#include "test.h"
#include "parser.h"
#include "eval.h"
#include "symbols.h"

object_t rplaca(object_t list);
object_t rplacd(object_t list);
object_t cons(object_t list);
object_t car(object_t list);
object_t cdr(object_t list);
object_t list(object_t list);

extern object_t nil;

jmp_buf jmp_env;

void error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
}

/**
 * создать объект для выражения (car (quote (5)))
 * вычислить объект
 */
void test_car()
{
    printf("test_car: ");

    int e = 5;
    object_t l = new_pair(new_number(e), NULLOBJ);
    object_t q = new_pair(NEW_SYMBOL("QUOTE"),
			   new_pair(l, NULLOBJ));
    object_t o = new_pair(NEW_SYMBOL("CAR"),
			   new_pair(q, NULLOBJ));
    object_t res = eval(o, NULLOBJ);
    ASSERT(TYPE(res), NUMBER);
    ASSERT(get_value(res), 5);
}

/*
 *создать объект для выражения (car (quote 5))
 *вычислить объект
 */

void test_invalid_car()
{
    printf("test_invalid_car: ");
    
    int e = 5;
    object_t l = new_number(e);// 5
    object_t q = new_pair(NEW_SYMBOL("QUOTE"), new_pair(l,NULLOBJ));// (quote 5)
    object_t o = new_pair(NEW_SYMBOL("CAR"),new_pair(q,NULLOBJ));// (car (quote 5))
    if (setjmp(jmp_env) == 0) {
        object_t res = eval(o, NULLOBJ); 
        FAIL;
    } else 
        OK;
}

/**
 * Попытка вернуть элемент из пустого объекта 
 */
void test_car_null()
{
    printf("test_car_null: ");
    if (setjmp(jmp_env) == 0) {
	object_t res = car(NULLOBJ);
        FAIL;
    } else 
        OK;
}

/**
 * Попытка вернуть элемент из списка объектов 
 */
void test_car_many_args()
{
    printf("test_car_many_args: ");

    int num1 = 1;
    int num2 = 2;
    object_t num_obj1 = new_number(num1);
    object_t num_obj2 = new_number(num2);

    object_t list_with_two_args = new_pair(num_obj1, new_pair(num_obj2, NULLOBJ));
    if (setjmp(jmp_env) == 0) {
	object_t result = car(list_with_two_args);
        FAIL;
    } else 
        OK;
}

/**
 * создать объект для выражения (cdr (quote (5)))
 * вычислить объект
 */
void test_cdr()
{
    printf("test_cdr: ");
    object_t l = new_pair(new_number(5), NULLOBJ);
    object_t q = new_pair(NEW_SYMBOL("QUOTE"),
			   new_pair(l, NULLOBJ));
    object_t o = new_pair(NEW_SYMBOL("CDR"),
			   new_pair(q, NULLOBJ));
    object_t res = eval(o, NULLOBJ);
    ASSERT(res, nil);
}

/*
 *создать объект для выражения (cdr (quote 5))
 *вычислить объект
 */
void test_invalid_cdr()
{
    printf("test_invalid_cdr:\n");
    object_t l = new_number(5);// 5
    object_t q = new_pair(NEW_SYMBOL("QUOTE"),new_pair(l,NULLOBJ));// (quote 5)
    object_t o = new_pair(NEW_SYMBOL("CDR"),new_pair(q,NULLOBJ));// (cdr (quote 5))
    if (setjmp(jmp_env) == 0) {
	object_t res = eval(o, NULLOBJ);
        FAIL;
    } else 
        OK;
}

/* /\* */
/*  * Попытка вычисления объекта NULLOBJ для cdr */
/*  *\/ */
 void test_cdr_null() 
 { 
     printf("test_cdr_null:\n"); 
     if (setjmp(jmp_env) == 0) {
	 object_t res = cdr(NULLOBJ); 
	 FAIL;
     } else 
	 OK;
 } 

/* /\* */
/*  * Попытка вычисления объекта из нескольких элеемнтов для cdr */
/*  *\/ */
 void test_cdr_many_args() 
 { 
     printf("test_cdr_many_args:\n"); 
     object_t num_obj1 = new_number(1); 
     object_t num_obj2 = new_number(2); 
     object_t list_with_two_args = new_pair(num_obj1, new_pair(num_obj2, NULLOBJ)); 
     if (setjmp(jmp_env) == 0) {
	 object_t res = cdr(list_with_two_args); 
	 FAIL;
     } else 
	 OK;
 } 

/**
 * создать объект для выражения (cons (quote a) (quote (5)))
 * вычислить объект (A 5)
 */
void test_cons()
{
    printf("test_cons: ");
    object_t l = new_pair(new_number(5), NULLOBJ); 
    object_t q = new_pair(NEW_SYMBOL("QUOTE"), 
			  new_pair(l, NULLOBJ)); 
    object_t qa = new_pair(NEW_SYMBOL("QUOTE"), 
 			   new_pair(NEW_SYMBOL("A"), NULLOBJ)); 
    object_t o = new_pair(NEW_SYMBOL("CONS"), 
			  new_pair(qa, new_pair(q, NULLOBJ))); 
    object_t res = eval(o, NULLOBJ); 
    ASSERT(TYPE(FIRST(res)), SYMBOL); 
    ASSERT(TYPE(SECOND(res)), NUMBER); 
    ASSERT(get_value(SECOND(res)), 5);
}

/**
 * Тест cons без параметров (CONS)
 *
 */
void test_cons_noparams()
{
    printf("test_cons_noparams: ");
    object_t a = new_pair(NEW_SYMBOL("CONS"), NULLOBJ); 
    if (setjmp(jmp_env) == 0) {
        object_t res = eval(a, NULLOBJ); 
	FAIL;
    } else 
	OK;
}

/**
 * Тест cons с одним параметром (cons 5)
 *
 */
void test_cons_one_param()
{
    printf("test_cons_one_param: ");
    object_t a = new_pair(NEW_SYMBOL("CONS"), new_pair(new_number(5), NULLOBJ)); 
    if (setjmp(jmp_env) == 0) {
        object_t res = eval(a, NULLOBJ); 
	FAIL;
    } else 
	OK;
}

/**
 * Тест cons с 3 параметрами (cons 5 5 5)
 *
 */
void test_cons_3_params()
{
    printf("test_cons_3_params: ");
    int num = 5; 
    object_t a = new_pair(NEW_SYMBOL("CONS"), 
 			  new_pair(new_number(num), 
 				   new_pair(new_number(num),  
 					    new_pair(new_number(num), NULLOBJ)))); 
    if (setjmp(jmp_env) == 0) {
        object_t res = eval(a, NULLOBJ); 
	FAIL;
    } else 
	OK;
}

/*
 *создать объект для выражения (cons (quote a) 4)
 *вычислить объект
 */
void test_cons2()
{
    printf("test_cons2:\n");
    object_t qa = new_pair(NEW_SYMBOL("QUOTE"), 
			   new_pair(NEW_SYMBOL("A"), NULLOBJ)); //(quote a))) 
    object_t o = new_pair(NEW_SYMBOL("CONS"), 
 			  new_pair(qa, new_pair(new_number(4), NULLOBJ))); //(cons (quote a) 4)) 
    object_t res = eval(o, NULLOBJ); 
    ASSERT(TYPE(res), PAIR); 
    ASSERT(get_value(TAIL(res)), 4);
}

// ( (5(3 NULLOBJ)) (2 NULLOBJ) ) -> (2(3 NULLOBJ))
void test_rplaca()
{
    printf("test_rplaca: ");
    int a = 5;
    int b = 3;
    int c = 2;
    object_t p1 = new_pair(new_number(b), NULLOBJ); // (3 NULLOBJ) 
    object_t p2 = new_pair(new_number(a), p1); // (5 (3 NULLOBJ)) 
    object_t p3 = new_pair(new_number(c), NULLOBJ); // (2 NULLOBJ) 
    object_t p4 = new_pair(p2, p3); // ( (5(3 NULLOBJ)) (2 NULLOBJ) ) 
    object_t p5 = new_pair(new_number(c), p1); // (2(3 NULLOBJ)) 
    object_t p6 = FIRST(p4); //пара 5(3 NULLOBJ)    object_t list 
    object_t p7 = FIRST(p6); //5                 list->u.pair->left 
    object_t p8 = TAIL(p4); // (2 NULLOBJ) 
    object_t p9 = FIRST(p8); // 2                SECOND(params) 
    printf("rplaca: "); 
    PRINT(p4); 
    object_t res = rplaca(p4); 
    PRINT(res); 
    ASSERT(get_value(FIRST(res)), 2); 
    ASSERT(get_value(SECOND(res)), 3); 
    ASSERT(res, p2); // (5 (3 NULLOBJ)) 
    ASSERT(get_value(p7), 5); 
    ASSERT(get_value(p9), 2);
}

// нет параметров
void test_rplaca_no_params()
{
    printf("test_rplaca_no_params: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = rplaca(NULLOBJ); 
	FAIL;
    } else 
	OK;
}

// недостаточно параметров
void test_rplaca_not_enought_params()
{
    printf("test_rplaca_not_enought_params: ");
    object_t p1 = new_pair(new_number(5), new_number(5)); 
    object_t p2 = new_pair(p1, NULLOBJ); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplaca(p2); 
	FAIL;
    } else 
	OK;
}

// слишком много параметров
void test_rplaca_too_many_params()
{
    printf("test_rplaca_too_many_params: ");
    object_t p1 = new_pair(new_number(5), new_number(5)); 
    object_t p2 = new_pair(new_number(5), new_number(5)); 
    object_t p3 = new_pair(p1, p2); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplaca(p3); 
	FAIL;
    } else 
	OK;
}

// первый параметр не pair
void test_rplaca_first_param_is_not_pair()
{
    printf("test_rplaca_first_param_is_not_pair: ");
    object_t p1 = new_pair(new_number(5), NULLOBJ); 
    object_t p2 = new_pair(new_number(4), p1); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplaca(p2); 
	FAIL;
    } else 
	OK;
}

// пустой список
void test_rplaca_empty_list()
{
    printf("test_rplaca_empty_list: ");
    object_t p1 = new_pair(new_number(5), NULLOBJ); 
    object_t p2 = new_pair(NULLOBJ, p1); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplaca(p2); 
	FAIL;
    } else 
	OK;
}

// Попытка замены пустой пары
void test_rplacd_null()
{
    printf("test_rplacd_null: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = rplacd(NULLOBJ); 
	FAIL;
    } else 
	OK;
}

// Попытка замены пары без параметра
void test_rplacd_no_params()
{
    printf("test_rplacd_no_params: ");
    object_t num_obj = new_number(1); 
    object_t list = new_pair(num_obj, NULLOBJ); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplacd(list); 
	FAIL;
    } else 
	OK;
}

// Попытка замены при нескольких параметрах
void test_rplacd_many_params()
{
    printf("test_rplacd_many_params: ");
    object_t num_obj = new_number(1); 
    object_t list = new_pair(num_obj, new_pair(num_obj, new_pair(num_obj, NULLOBJ))); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplacd(list); 
	FAIL;
    } else 
	OK;
}

// Попытка замены для пустой первой пары
void test_rplacd_empty_list()
{
    printf("test_rplacd_empty_list: ");
    object_t num_obj = new_number(1); 
    object_t list = new_pair(NULLOBJ, new_pair(num_obj, NULLOBJ)); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplacd(list); 
	FAIL;
    } else 
	OK;
}

// Попытка замены для не пары
void test_rplacd_not_pair()
{
    printf("test_rplacd_not_pair: ");
    object_t num_obj = new_number(1); 
    object_t list = new_pair(num_obj, new_pair(num_obj, NULLOBJ)); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplacd(list); 
	FAIL;
    } else 
	OK;
}

// Создание пары ((1 NULLOBJ) (2 NULLOBJ)) и замена правой части пары (1 2)
void test_rplacd()
{
    printf("test_rplacd: ");
    object_t left_obj = new_number(1); 
    object_t right_obj = new_number(2); 
    object_t pair = new_pair(left_obj, NULLOBJ); 
    object_t params = new_pair(pair, new_pair(right_obj, NULLOBJ)); 

    object_t res = rplacd(params); 
    
    ASSERT(TYPE(res), PAIR); 
    ASSERT(TYPE(FIRST(res)), NUMBER); 
    ASSERT(get_value(FIRST(res)), 1); 
    ASSERT(TYPE(TAIL(res)), NUMBER); 
    ASSERT(get_value(TAIL(res)), 2);
}

/**
 * Тест функции list
 */
void test_list()
{
    printf("test_list: ");
    object_t p = new_pair(new_number(1),  
 			  new_pair(new_number(2), NULLOBJ)); 
    object_t res = list(p); 
    ASSERT(TYPE(FIRST(res)), NUMBER); 
    ASSERT(get_value(FIRST(res)), 1); 
    ASSERT(TYPE(SECOND(res)), NUMBER); 
    ASSERT(get_value(SECOND(res)), 2);
}

int main()
{
    printf("------------test_pair---------\n");
    init_regions();
    init_objects();
    init_pair();
    init_eval();
    test_car();//10
    test_car_null();//59
    test_car_many_args();//60
    test_invalid_car();//58
    test_cdr();
    test_invalid_cdr();//61
    test_cdr_null();//62
    test_cdr_many_args();//63
    test_cons_noparams();//64
    test_cons2();//12
    test_cons();//12
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
