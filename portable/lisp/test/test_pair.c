#include <stdio.h>
#include "objects.h"
#include "pair.h"
#include "test.h"
#include "parser.h"
#include "eval.h"

object_t *rplaca(object_t *list);

void error(char *str)
{
  printf("%s", str);
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


int main()
{
    printf("------------test_pair---------\n");
    test_rplaca();
    test_rplaca_no_params();
    test_rplaca_not_enought_params();
    test_rplaca_too_many_params();
    test_rplaca_first_param_is_not_pair();
    test_rplaca_empty_list();
    return 0;
}

