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
object_t name (object_t el)\
{\    	  
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
// Функция
MAKE_PREDICATE(functionp, TYPE(el) == FUNCTION)
// Строка
MAKE_PREDICATE(stringp, TYPE(el) == STRING)
// Массив
MAKE_PREDICATE(arrayp, TYPE(el) == ARRAY)
// Число с плавающей точкой 
MAKE_PREDICATE(floatp, TYPE(el) == FLOAT)

void init_predicates()
{
    register_func("SYMBOLP", symbolp, 0, 1);
    register_func("PAIRP", pairp, 0, 1);
    register_func("INTEGERP", integerp, 0, 1);
    register_func("STRINGP", stringp, 0, 1);
    register_func("ARRAYP", arrayp, 0, 1);
    register_func("FLOATP", floatp, 0, 1);
    register_func("FUNCTIONP", functionp, 0, 1);
}
