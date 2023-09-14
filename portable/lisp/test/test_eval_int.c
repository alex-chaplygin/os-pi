#include <stdio.h>
#include "eval.h"
#include "test.h"

/**
 * создать объект для выражения (car (quote 5))
 * вычислить объект 
 */
void test_car()
{
    printf("test_car: ");
  
    object_t num = {NUMBER, 5};  
    pair_t node = {&num, NULL};
    object_t lst = {PAIR, &node};
    
    ASSERT(res->u.value, 5);
}

int main()
{
    init_eval();
    return 0;
}

