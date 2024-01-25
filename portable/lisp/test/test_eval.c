#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "test.h"
#include "parser.h"

extern object_t *t;
object_t test_res;
pair_t test_pair;
int print_counter;

void print_obj(object_t *head){}

object_t *object_new(type_t type, void *data)
{
    return NULL;
}

array_t *new_empty_array(int length)
{
    return NULL;
}

object_t *new_pair(object_t *left, object_t *right)
{
    test_pair.left = left;
    test_pair.right = right;
    test_res.type = PAIR;
    test_res.u.pair = &test_pair;
    return &test_res;
}

object_t *car(object_t *list);
object_t *cdr(object_t *list);
object_t *cons(object_t *list);
object_t *eq(object_t *list);
object_t *atom(object_t *obj);
object_t *quote(object_t *list);

void error(char *str, ...)
{
  printf("%s", str);
}

symbol_t *find_symbol(char *str)
{
    return NULL;
}

symbol_t *check_symbol(char *str)
{
    return NULL;
}

void register_func(char *name, func_t func_ptr)
{
}

void test_eq_num()
{
    printf("test_eq_num: ");
    object_t num1, num2;
    pair_t p1, p2;
    object_t ob1, ob2;
    
    num1.type = NUMBER;
    num1.u.value = 6;
    num2.type = NUMBER;
    num2.u.value = 6;

    p2.right = NULL;
    p2.left = &num2;

    ob2.type = PAIR;
    ob2.u.pair = &p2;
    
    p1.right = &ob2;
    p1.left = &num1;

    ob1.type = PAIR;
    ob1.u.pair = &p1;
    
    ASSERT(eq(&ob1), NULL);
}

//символы не равны
void test_eq_sym()
{
    printf("test_eq_sym: ");
    object_t num1, num2;
    pair_t p1, p2;
    object_t ob1, ob2;
    symbol_t s1, s2;
        
    num1.type = SYMBOL;
    num1.u.symbol = &s1;
    num2.type = SYMBOL;
    num2.u.symbol = &s2;

    p2.right = NULL;
    p2.left = &num2;

    ob2.type = PAIR;
    ob2.u.pair = &p2;
    
    p1.right = &ob2;
    p1.left = &num1;

    ob1.type = PAIR;
    ob1.u.pair = &p1;
    
    ASSERT(eq(&ob1), NULL);
}

//символы равны
void test_eq_sym_eq()
{
    printf("test_eq_sym_eq: ");
    object_t num1, num2, obj_t;
    pair_t p1, p2;
    object_t ob1, ob2;
    symbol_t s1;

    t = &obj_t;
    
    num1.type = SYMBOL;
    num1.u.symbol = &s1;
    num2.type = SYMBOL;
    num2.u.symbol = &s1;

    p2.right = NULL;
    p2.left = &num2;

    ob2.type = PAIR;
    ob2.u.pair = &p2;
    
    p1.right = &ob2;
    p1.left = &num1;

    ob1.type = PAIR;
    ob1.u.pair = &p1;
    
    ASSERT(eq(&ob1), t);
}

void test_cdr()
{
    printf("test_cdr: ");

    object_t num;
    num.type = NUMBER;
    num.u.value = 5;
    
    pair_t node1;
    node1.left = &num;
    node1.right = &num;
    
    object_t pair;
    pair.type = PAIR;
    pair.u.pair = &node1;
    
    struct pair_s node;
    node.left = &pair;
    node.right = NULL;
    
    object_t lst;
    lst.type = PAIR;
    lst.u.pair = &node;
    
    object_t* res = cdr(&lst);
    
    ASSERT(res->u.value, 5);
}

void test_car()
{
    printf("test_car: ");
  
    object_t num;
    num.type = NUMBER;
    num.u.value = 5;
    
    object_t pair;
    pair.type = PAIR;
    pair.u.pair = &(struct pair_s){&num};
    
    struct pair_s node;
    node.left = &pair;
    node.right = NULL;
    
    object_t lst;
    lst.type = PAIR;
    lst.u.pair = &node;
    
    object_t* res = car(&lst);
    ASSERT(res->u.value, 5);
}

void test_cons()
{
    printf("test_cons: ");
    object_t num1, num2, obj_p1, obj_p2, obj_p3;
    pair_t p1, p2, p3;
        
    num1.type = NUMBER;
    num1.u.value = 1;
    num2.type = NUMBER;
    num2.u.value = 2;
    
    p1.left = &num2;
    p1.right = NULL;
    obj_p1.type = PAIR;
    obj_p1.u.pair = &p1;
    
    p2.left = &obj_p1;
    p2.right = NULL;
    obj_p2.type = PAIR;
    obj_p2.u.pair = &p2;
    
    p3.left = &num1;
    p3.right = &obj_p2;
    
    obj_p3.type = PAIR;
    obj_p3.u.pair = &p3;
    
    object_t *res = cons(&obj_p3);

    ASSERT(res->type, PAIR);
    ASSERT(res->u.pair->left->u.value, 1);
    ASSERT(res->u.pair->right->u.pair->left->u.value, 2);
}

void test_cons_second_not_pair() 
{
    printf("test_cons_second_not_pair: ");
    object_t num1; // 8
    object_t num2; // 9
    pair_t pair; // (8
    pair_t pair2; // (9
    object_t obj_pair; // (8
    object_t obj_pair2; // (9

    num1.type = NUMBER;
    num1.u.value = 8;

    num2.type = NUMBER;
    num2.u.value = 9;

    pair.left = &num1;
    pair.right = &obj_pair2;

    pair2.left = &num2;
    pair2.right = NULL;

    obj_pair2.type = PAIR;
    obj_pair2.u.pair = &pair2;

    obj_pair.type = PAIR;
    obj_pair.u.pair = &pair;

    object_t *res = cons(&obj_pair);  

    ASSERT(res->type, PAIR);
}

/**
 * Создать объект список (5)
 * Вызвать функцию quote
 * Проверить результат =  5 
 */
void test_quote()
{
    printf("test_quote: ");
    object_t num;
    num.type = NUMBER;
    num.u.value = 5;
    
    struct pair_s node;
    node.left = &num;
    node.right = NULL;
    
    object_t lst;
    lst.type = PAIR;
    lst.u.pair = &node;

    object_t *o = quote(&lst);
    ASSERT(o->type, NUMBER);
    ASSERT(o->u.value, 5); 
}


int main()
{
    printf("-------test eval--------\n");
    test_car();
    test_cdr();
    test_eq_num();
    test_eq_sym();
    test_eq_sym_eq();
    test_cons();
    test_cons_second_not_pair();
    test_quote();
    return 0;
}
