#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "test.h"
#include "parser.h"

extern object_t *t;
object_t test_res;
pair_t test_pair;

void print_obj(object_t *head){}

object_t *object_new(type_t type, void *data){
    return NULL;
}

object_t *new_pair(object_t *left, object_t *right)
{
    test_pair.left = left;
    test_pair.right = right->u.pair->left;
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

void error(char *str)
{
  printf("%s", str);
}

symbol_t *find_symbol(char *str)
{
    return NULL;
}

symbol_t *find_symbol_get(char *str)
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
    num2.u.value = 15;

    p2.right = NULL;
    p2.left = &num2;

    ob2.type = PAIR;
    ob2.u.pair = &p2;
    
    p1.right = &ob2;
    p1.left = &num1;

    ob1.type = PAIR;
    ob1.u.pair = &p1;
    
    ASSERT(eq(&ob1), ERROR);
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
    node.right = &num;
    
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
    ASSERT(res->u.pair->right->u.value, 2);
}

// тип объекта - атом
void test_atom_is()
{
    printf("test_atom_is: ");
  
    object_t obj, obj_t;
    t = &obj_t;

    obj.type = NUMBER;
    obj.u.value = 5;

    object_t* res = atom(&obj);
    ASSERT(res, t);
}

// тип объекта - не атом
void test_atom_not()
{
    printf("test_atom_not: ");

    object_t obj_t;
    t = &obj_t;
  
    object_t obj;
    obj.type = PAIR;

    object_t* res = atom(&obj);
    ASSERT(res, NULL);
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
    test_car();
    test_cdr();
    test_eq_num();
    test_eq_sym();
    test_eq_sym_eq();
    test_cons();
    test_atom_is();
    test_atom_not();
    test_quote();
    return 0;
}
