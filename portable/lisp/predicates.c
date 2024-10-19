#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "parser.h"
#include "eval.h"

/*
 * Функция symbolp, которая возвращает T, если obj - символ и NIL, если нет
 */
object_t symbolp(object_t params)
{   
    if (params == NULLOBJ) 
	error("SYMBOLP: no params");
    else if (TAIL(params) != NULLOBJ) 
	error("SYMBOLP: many params");
    object_t symbol = FIRST(params);    
    if (TYPE(symbol) == SYMBOL)
	return t;
    else
	return nil;
}

/*
 * Функция integerp, которая возвращает T, если obj - целое число и NIL, если нет
 */
object_t integerp(object_t params)
{   
    if (params == NULLOBJ) 
	error("INTEGERP: no params");
     else if (TAIL(params) != NULLOBJ) 
	error("INTEGERP: many params");
    object_t integer = FIRST(params);
    if (TYPE(integer) == NUMBER || TYPE(integer) == BIGNUMBER)
	return t;
    else
	return nil;
}

void init_predicates()
{
    register_func("SYMBOLP", symbolp);
    register_func("INTEGERP", integerp);	
}
