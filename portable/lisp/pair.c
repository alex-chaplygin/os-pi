#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "parser.h"
#include "eval.h"

/** 
 * возвращает первый элемент списка
 * 
 * @param list - список параметров
 * 
 * @return указатель на значиение первого элемента списка
 */
object_t car(object_t list)
{
    if (list == NULLOBJ)
	error("car: No args");
    if (TAIL(list) != NULLOBJ)
        error("car: Too many args");
    object_t arg = FIRST(list);
    if (TYPE(arg) != PAIR)
        error("Not list in car");
    return FIRST(arg);
}


/** 
 * возвращает список без первого элемента
 * 
 * @param list - объект типа список
 * 
 * @return указатель на второй элемент списка
 */
object_t cdr(object_t list)
{
    if (list == NULLOBJ)
	error("cdr: No args");
    if (TAIL(list) != NULLOBJ)
        error("cdr: Too many args");
    object_t arg = FIRST(list);
    if (TYPE(arg) != PAIR)
        error("cdr: Not list in cdr");
    return TAIL(arg);
}

/** 
 * Создание новой пары
 *
 * @param list - список параметров (должно быть два параметра - левый и правый объекты)
 *
 * @return список который содержит 1 параметр, и продолжается с элементами 2 параметра.
 */
object_t cons(object_t list)
{			
    if (list == NULLOBJ || TAIL(list) == NULLOBJ || TAIL(TAIL(list)) != NULLOBJ)
	error("CONS: invalid params");
    object_t p1 = FIRST(list);
    object_t p2 = SECOND(list);
    return new_pair(p1, p2);
}

/**
 * Заменить левую часть пары
 * @param params параметры (пара, любой объект)
 * @return возвращает список с изменённой левой частью
 */
object_t rplaca(object_t params)
{
    if (params == NULLOBJ)
	    error("RPLACA: no params");
    if (TAIL(params) == NULLOBJ)
	    error("RPLACA: not enough params");
    if (TAIL(TAIL(params)) != NULLOBJ)
	    error("RPLACA: too many params");
    if (FIRST(params) == NULLOBJ)
	    error("RPLACA: empty list");
    if (TYPE(FIRST(params)) != PAIR)
	    error("RPLACA: first param is not pair");
    object_t list = FIRST(params);
    GET_PAIR(list)->left = SECOND(params);
    return list;
}

/**
 * Заменить правую часть пары
 * @params params параметры (пара, любой объект)
 * @return возвращает список с изменённой правой частью
 */
object_t rplacd(object_t params)
{
    if (params == NULLOBJ)
	error("RPLACD: no params");
    if (TAIL(params) == NULLOBJ)
	error("RPLACD: not enough params");
    if (TAIL(TAIL(params)) != NULLOBJ)
	error("RPLACD: too many params");
    if (FIRST(params) == NULLOBJ)
	error("RPLACD: empty list");
    if (TYPE(FIRST(params)) != PAIR)
	error("RPLACD: first param is not pair");
    object_t list = FIRST(params);
    GET_PAIR(list)->right = SECOND(params);
    return list;
}

void init_pair()
{
    register_func("CAR", car);
    register_func("CDR", cdr);
    register_func("CONS", cons);
    register_func("RPLACA", rplaca);
    register_func("RPLACD", rplacd);
}
