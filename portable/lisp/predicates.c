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
    register_func("STRING", stringp, 0, 1);
    register_func("ARRAY", arrayp, 0, 1);
    register_func("FLOAT", floatp, 0, 1);
}

//необходимо убрать убрать проверку на количество аргументов, изменить макрос чтобы вместо списка аргументов ему подставлялся 1 аргумент
