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
object_t *car(object_t *list)
{
    if (list == NULL)
    {
	error("car: No args");
	return ERROR;
    }
    if (TAIL(list) != NULL)
    {
        error("car: Too many args");
        return ERROR;
    }
    object_t *arg = FIRST(list);
    if (arg->type != PAIR){
        error("Not list in car");
	return ERROR;
    }
    return FIRST(arg);
}


/** 
 * возвращает список без первого элемента
 * 
 * @param list - объект типа список
 * 
 * @return указатель на второй элемент списка
 */
object_t *cdr(object_t *list)
{
    if (list == NULL)
    {
	error("cdr: No args");
	return ERROR;
    }
    if (TAIL(list) != NULL)
    {
        error("cdr: Too many args");
        return ERROR;
    }
    object_t *arg = FIRST(list);
    if (arg->type != PAIR){
        error("Not list in cdr");
    	return ERROR;
    }
    return TAIL(arg);
}

/** 
 * Создание новой пары
 *
 * @param list - список параметров (должно быть два параметра - левый и правый объекты)
 *
 * @return список который содержит 1 параметр, и продолжается с элементами 2 параметра.
 */
object_t *cons(object_t *list)
{			
    if (list == NULL || TAIL(list) == NULL || TAIL(TAIL(list)) != NULL) {
	error("CONS: invalid params");
	return ERROR;
    }
    object_t *p1 = FIRST(list);
    object_t *p2 = SECOND(list);
    return new_pair(p1, p2);
}

/**
 * Заменить левую часть пары
 * @param params параметры (пара, любой объект)
 * @return возвращает список с изменённой левой частью
 */
object_t *rplaca(object_t *params)
{
    if (params == NULL) {
	error("RPLACA: no params");
	return ERROR;
    }
    if (TAIL(params) == NULL) {
	error("RPLACA: not enough params");
	return ERROR;
    }
    if (TAIL(TAIL(params)) != NULL) {
	error("RPLACA: too many params");
	return ERROR;
    }
    if (FIRST(params) == NULL) {
	error("RPLACA: empty list");
	return ERROR;
    }
    if (FIRST(params)->type != PAIR) {
	error("RPLACA: first param is not pair");
	return ERROR;
    }
    object_t *list = FIRST(params);
    list->u.pair->left = SECOND(params);
    return list;
}

/**
 * Заменить правую часть пары
 * @params params параметры (пара, любой объект)
 * @return возвращает список с изменённой правой частью
 */
object_t *rplacd(object_t *params)
{
    if (params == NULL) {
	error("RPLACD: no params");
	return ERROR;
    }
    if (TAIL(params) == NULL) {
	error("RPLACD: not enough params");
	return ERROR;
    }
    if (TAIL(TAIL(params)) != NULL) {
	error("RPLACD: too many params");
	return ERROR;
    }
    if (FIRST(params) == NULL) {
	error("RPLACD: empty list");
	return ERROR;
    }
    if (FIRST(params)->type != PAIR) {
	error("RPLACD: first param is not pair");
	return ERROR;
    }
    object_t *list = FIRST(params);
    list->u.pair->right = SECOND(params);
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
