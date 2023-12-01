#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "parser.h"
#include "eval.h"

/*
 * Функция symbolp, которая возвращает T, если obj - символ и NIL, если нет
 */
object_t *symbolp(object_t *params)
{   
    if (params == NULL) {
	error("SYMBOLP: no params");
	return ERROR;
    } else if (TAIL(params) != NULL) {
	error("SYMBOLP: many params");
	return ERROR;
    }
    object_t *symbol = FIRST(params);
    
    if (symbol->type == SYMBOL)
	return object_new(SYMBOL, "T");
    else
	return NULL;
}


void init_predicates()
{
    register_func("SYMBOLP", symbolp);
}
