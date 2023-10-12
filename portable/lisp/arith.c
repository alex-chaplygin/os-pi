#include <stdlib.h>
#include "objects.h"
#include "eval.h"
#include "symbols.h"
#include "parser.h"

extern object_t *t;

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
 * Вычитание аргументов (- 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return разность
 */
object_t *sub(object_t *list)
{
    int num = 0;
    if (list == NULL)
	return object_new(NUMBER, &num);
    object_t *diff = FIRST(list);
    num = diff->u.value - add(TAIL(list))->u.value;
    return object_new(NUMBER, &num);
}

/** 
 * Умножение аргументов (* 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return произведение
 */
object_t *mul(object_t *list)
{
    int num = 1;
    if (list == NULL)
	return object_new(NUMBER, &num);
    else if (list == ERROR)
	return ERROR;
    object_t *prod = FIRST(list);
    if (prod == NULL) {
	error("mul: Invalid first\n");
	return ERROR;
    }
    object_t *tail = TAIL(list);
    object_t *res = mul(tail);
    if (res == ERROR)
	return ERROR;
    num = prod->u.value * res->u.value;
    return object_new(NUMBER, &num);
}

/** 
 * Сравнение числовых аргументов (= 1 2)
 *
 * @param list - список чисел (1 2)
 *
 * @return T - если равно, иначе NIL
 */
object_t *num_eq(object_t *list)
{
    object_t *n1 = FIRST(list);
    object_t *n2 = SECOND(list);
    if (n1->u.value == n2->u.value)
	return t;
    else
	return NULL;
}

/** 
 * Инициализация арифметических функций
 */
void init_arith()
{
    register_func("+", add);
    register_func("-", sub);
    register_func("*", mul);
    register_func("=", num_eq);
}
