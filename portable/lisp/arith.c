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
    if (list == NULL) {
	error("add: no arguments\n");
	return ERROR;
    }
    while (list != NULL) {
	object_t *first = FIRST(list);
	if (first->type == NUMBER) {  
	    num += first->u.value;
	    list = TAIL(list);
	}
	else {
	    error("add: Not number\n");
	    return ERROR;
	}
    }
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
    if (list == NULL) {
        error("sub: no arguments\n");
        return ERROR;
    }
    object_t *first = FIRST(list);
    int num = first->u.value;
    list = TAIL(list);
    while (list != NULL) {
        object_t *first = FIRST(list);
        if (first->type == NUMBER) {
            num -= first->u.value;
            list = TAIL(list);
        } else {
            error("sub: Not number\n");
            return ERROR;
        }
    }
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
    if (list == NULL)
	error("mul: no arguments\n");
    object_t *first = FIRST(list);
    int num = first->u.value;
    list = TAIL(list);
    while(list != NULL){
	object_t *first = FIRST(list);
	if(first->type == NUMBER){
	    num *= first->u.value;
	    list = TAIL(list);
	}
	else{
	    error("mul: Not number\n");
	    return ERROR;
	}
    }
    return object_new(NUMBER, &num);
}

/**
 * Деление (/ 8 2)
 * 
 * @param list - список чисел (8 2)
 *
 * @return результат от деления
 */

object_t *int_div(object_t *list)
{
    if (list == NULL) {
        error("div: no arguments\n");
        return ERROR;
    }
    if (SECOND(list) == NULL) {
        error("div: no divisor\n");
        return ERROR;
    }
    object_t *first = FIRST(list);
    object_t *second = SECOND(list);
    if (second->u.value != 0) {
        int num = first->u.value / second->u.value;
        return object_new(NUMBER, &num);
    } else {
        error("div: divisor = 0 \n");
        return ERROR;
    }
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
    register_func("/", int_div);
}
