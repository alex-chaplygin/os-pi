#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "parser.h"
#include "eval.h"

int list_length(object_t args);

/*
 * Создает функцию предикат 
 * @param name имя создаваемой функции 
 * @param cond условие, когда функция возвращает Т
 */
#define MAKE_PREDICATE(name, cond)\
object_t name (object_t params)\
{   \
    if (params == NULLOBJ) \
	error(#name": no params");    \
     else if (TAIL(params) != NULLOBJ) \
	 error(#name": many params"); \
    object_t el  = FIRST(params);\
    if (cond)\
	return t;\
    else\
	return nil;\
}

// Пара
MAKE_PREDICATE(pairp, TYPE(el) == PAIR)
// Символ
MAKE_PREDICATE(symbolp, TYPE(el) == SYMBOL)
// Число
MAKE_PREDICATE(integerp, TYPE(el) == NUMBER || TYPE(el) == BIGNUMBER)

void init_predicates()
{
    register_func("SYMBOLP", symbolp);
    register_func("PAIRP", pairp);
    register_func("INTEGERP", integerp);	
}
