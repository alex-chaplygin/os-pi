#include <stdlib.h>
#include "objects.h"
#include "eval.h"
#include "symbols.h"

/** 
 * Сложение аргументов (+ 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return сумму
 */
object_t *add(object_t *list)
{
    int num = 0;
    if (list == NULL)
	return object_new(NUMBER, &num);
    object_t *sum = FIRST(list);
    num = sum->u.value + add(TAIL(list))->u.value;
    return object_new(NUMBER, &num);
}

/** 
 * Инициализация арифметических функций
 */
void init_arith()
{
    register_func("+", add);
}
