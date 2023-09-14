#include <stdio.h>
#include "symbols.h"
#include "test.h"

extern object_t *t;

void print_elem(object_t *head){}

object_t *object_new(type_t type, void *data){
    return NULL;
}


object_t *car(object_t *list);
object_t *cdr(object_t *list);
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
    
    struct pair_s node;
    node.left = &num;
    node.right = NULL;
    
    object_t lst;
    lst.type = PAIR;
    lst.u.pair = &node;
    
    object_t* res = cdr(&lst);
    ASSERT(res, NULL);
}

void test_car()
{
    printf("test_car: ");
  
    object_t num;
    num.type = NUMBER;
    num.u.value = 5;
    
    struct pair_s node;
    node.left = &num;
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
    object_t num1, num2;
    pair_t p1, p2, p3;
        
    num1.type = NUMBER;
    num1.u.value = 1;
    num2.type = NUMBER;
    num2.u.value = 2;    
    //object_t* res = CONS()
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
    test_atom_is();
    test_atom_not();
    test_quote();
    return 0;
}
