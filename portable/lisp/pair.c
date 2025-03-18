#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "parser.h"
#include "eval.h"

/** 
 * возвращает первый элемент списка
 * 
 * @param arg - список
 * 
 * @return указатель на значиение первого элемента списка
 */
object_t car(object_t arg)
{
    if (TYPE(arg) != PAIR)
        error("Not list in car");
    return FIRST(arg);
}

/** 
 * возвращает список без первого элемента
 * 
 * @param arg - объект типа список
 * 
 * @return указатель на второй элемент списка
 */
object_t cdr(object_t arg)
{
    if (TYPE(arg) != PAIR)
        error("cdr: Not list in cdr");
    return TAIL(arg);
}

/**
 * Заменить левую часть пары
 * @param params параметры (пара, любой объект)
 * @return возвращает список с изменённой левой частью
 */
object_t rplaca(object_t p1, object_t p2)
{
    if (p1 == NULLOBJ)
	error("RPLACA: list is empty");
    if (TYPE(p1) != PAIR)
	error("RPLACA: first param is not list");
    object_t list = p1;
    GET_PAIR(list)->left = p2;
    return list;
}

/**
 * Заменить правую часть пары
 * @params params параметры (пара, любой объект)
 * @return возвращает список с изменённой правой частью
 */
object_t rplacd(object_t p1, object_t p2)
{
    if (p1 == NULLOBJ)
	error("RPLACD: list is empty");
    if (TYPE(p1) != PAIR)
	error("RPLACD: first param is not list");
    object_t list = p1;
    GET_PAIR(list)->right = p2;
    return list;
}

void init_pair()
{
    register_func("CAR", car, 0, 1);
    register_func("CDR", cdr, 0, 1);
    register_func("CONS", new_pair, 0, 2);
    register_func("RPLACA", rplaca, 0, 2);
    register_func("RPLACD", rplacd, 0, 2);
}
