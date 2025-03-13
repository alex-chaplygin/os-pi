#include <stdio.h>
#include <setjmp.h>
#include "objects.h"
#include "alloc.h"
#include "pair.h"
#include "test.h"
#include "parser.h"
#include "eval.h"
#include "symbols.h"

object_t rplaca(object_t p1, object_t p2);
object_t rplacd(object_t p1, object_t p2);
object_t cons(object_t list);
object_t car(object_t arg);
object_t cdr(object_t arg);
object_t list(object_t list);

extern object_t nil;

jmp_buf jmp_env;

void error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
    printf("-------");
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
        object_t res = eval(o, NULLOBJ, NULLOBJ); 
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
    object_t res = eval(o, NULLOBJ, NULLOBJ);
    ASSERT(res, nil);
}

/**
 * создать объект для выражения (cdr (quote 5))
 * вычислить объект
*/
void test_invalid_cdr()
{
    printf("test_invalid_cdr:\n");
    object_t l = new_number(5);// 5
    object_t q = new_pair(NEW_SYMBOL("QUOTE"),new_pair(l,NULLOBJ));// (quote 5)
    object_t o = new_pair(NEW_SYMBOL("CDR"),new_pair(q,NULLOBJ));// (cdr (quote 5))
    if (setjmp(jmp_env) == 0) {
	object_t res = eval(o, NULLOBJ, NULLOBJ);
        FAIL;
    } else 
        OK;
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
    object_t res = rplaca(p4, p3); 
    PRINT(res); 
    ASSERT(get_value(SECOND(res)), 2); 
    ASSERT(get_value(p7), 5); 
    ASSERT(get_value(p9), 2);
}

// недостаточно параметров
void test_rplaca_not_enought_params()
{
    printf("test_rplaca_not_enought_params: ");
    object_t p1 = new_pair(new_number(5), new_number(5)); 
    object_t p2 = new_pair(p1, NULLOBJ); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplaca(p2, p1); 
	OK;
    } else 
	FAIL;
}

// первый параметр не pair
void test_rplaca_first_param_is_not_pair()
{
    printf("test_rplaca_first_param_is_not_pair: ");
    object_t p1 = new_pair(new_number(5), NULLOBJ); 
    object_t p2 = new_pair(new_number(4), p1); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplaca(p2, p1); 
	OK;
    } else 
	FAIL;
}

// пустой список
void test_rplaca_empty_list()
{
    printf("test_rplaca_empty_list: ");
    object_t p1 = new_pair(new_number(5), NULLOBJ); 
    object_t p2 = new_pair(NULLOBJ, p1); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplaca(p2, p1); 
	OK;
    } else 
	FAIL;
}

// Попытка замены пары без параметра
void test_rplacd_no_params()
{
    printf("test_rplacd_no_params: ");
    object_t num_obj = new_number(1); 
    object_t list = new_pair(num_obj, NULLOBJ); 
    if (setjmp(jmp_env) == 0) {
        object_t res = rplacd(num_obj, list); 
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
        object_t res = rplacd(num_obj, list); 
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
        object_t res = rplacd(num_obj, list); 
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

    object_t res = rplacd(pair, params); 
    
    ASSERT(TYPE(res), PAIR); 
    ASSERT(TYPE(FIRST(res)), NUMBER); 
    ASSERT(get_value(FIRST(res)), 1); 
}


int main()
{
    printf("------------test_pair---------\n");
    init_regions();
    init_objects();
    init_regions();
    init_pair();
    init_eval();
    test_invalid_car();//58
    test_cdr();
    test_invalid_cdr();//61
    test_rplaca();
    test_rplaca_not_enought_params();
    test_rplaca_first_param_is_not_pair();
    test_rplaca_empty_list();
    test_rplacd();//21
    test_rplacd_no_params();//81
    test_rplacd_empty_list();//81
    test_rplacd_not_pair();//82
    printf("------------end_test_pair---------\n");
    return 0;
}
