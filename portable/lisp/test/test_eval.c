#include <stdio.h>
#include "list.h"
#include "test.h"

void print_elem(object_t *head){}

object_t *object_new(type_t type, void *data){
    return NULL;
}

atom_t *find_atom(char *str){
    return NULL;
}

object_t *CAR(object_t *list);
object_t *CDR(object_t *list);
object_t *eval_eq(object_t *p1, object_t *p2);
object_t *eval(object_t *obj);

void test_eq()
{
    object_t v1, v2;
    v1.type = ATOM;
    v2.type = ATOM;
    
    struct atom_s a1, a2;
    
    v1.u.atom = &a1;
    v2.u.atom = &a2;

    ASSERT(eval_eq(&v1, &v2), NULL);
}

void test_eval()
{
    object_t num;
    num.type = NUMBER;
    num.u.value = 6;
    
    ASSERT(eval(&num)->u.value, 6);
    
    
}

void test_cdr()
{
    object_t num;
    num.type = NUMBER;
    num.u.value = 5;
    
    struct pair_s node;
    node.left = &num;
    node.right = NULL;
    
    object_t lst;
    lst.type = PAIR;
    lst.u.pair = &node;
    
    object_t* res = CDR(&lst);
    ASSERT(res, NULL);
}

void test_car()
{
    object_t num;
    num.type = NUMBER;
    num.u.value = 5;
    
    struct pair_s node;
    node.left = &num;
    node.right = NULL;
    
    object_t lst;
    lst.type = PAIR;
    lst.u.pair = &node;
    
    object_t* res = CAR(&lst);
    ASSERT(res->u.value, 5);
}

int main()
{
    test_car();
    test_cdr();
    test_eq();
    test_eval();
    
    return 0;
}
