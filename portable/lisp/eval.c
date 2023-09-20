#include <stdio.h>
#include "objects.h"
#include "parser.h"
#include "eval.h"

object_t *t;
object_t *nil;
symbol_t *quote_sym;

/** 
 * возвращает первый элемент списка
 * 
 * @param list - список параметров
 * 
 * @return указатель на значиение первого элемента списка
 */
object_t *car(object_t *list)
{
    printf("car: ");
    print_obj(list);
    printf("\n");
    printf("car: %d\n", FIRST(list)->type);
    if (list->type != PAIR)
        error("Not list in car\n");
    return list->u.pair->left;
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
    if (list->type != PAIR)
        error("Not list in cdr\n");
    return object_new(PAIR, list->u.pair->right);
}

// (eq 'a 'a) -> T
// (eq 'a 'b) -> ()
/** 
 * возвращает t если значения первых двух элементов списка это один и тот же символ а иначе возвращает nil
 *
 * @param list - список параметров
 *
 * @return t или nil
 */
object_t *eq(object_t *list)
{
    object_t *p1 = FIRST(list);
    object_t *p2 = SECOND(list);
    // printf("p1=%d\n",p1->type);
    //printf("p2=%d\n",p2->type);
    if (p1->type != SYMBOL || p2->type != SYMBOL)
        error("not symbol in eq\n");
    if (p1->u.symbol == p2->u.symbol)
        return t;
    else
        return nil;
}

/** 
 * если аргумент атом, то возвращает T, иначе NIL
 *
 * @param list - список параметров
 *
 * @return t или nil
 */
object_t *atom(object_t *obj)
{
    if(obj->type != PAIR)
        return t;
    else
        return NULL;
}

/** 
 * возвращает свой аргумент без вычисления
 *
 * @param list - список параметров (1 параметр)
 *
 * @return аргумент
 */
object_t *quote(object_t *list)
{
    printf("quote: ");
    print_obj(list);
    printf("\n");
    return FIRST(list);
}

//
/** 
 * Конструирование объекта ожидает, что 2 параметр - список
 *
 * @param list - список параметров
 *
 * @return список который содержит 1 параметр, и продолжается с элементами 2 параметра.
 */
object_t *cons(object_t *list)
{			
    printf("cons: ");
    print_obj(list);
    printf("\n");
    if (list->type != PAIR)
	error("Not list in cons\n");
    object_t *p1 = FIRST(list);
    printf("p1: ");
    print_obj(p1);
    printf("\n");
    object_t *p2 = SECOND(list);
    printf("p2: ");
    print_obj(p2);
    printf("\n");
    if (p2->type != PAIR)
	error("second parameter not list");
    return new_pair(p1, p2);
}

/**
 * Обработка условия
 * Возвращаем список объектов из выражения, оцененных  eval
 * @param obj входное выражение
 * @return возвращает вычисленный объект
*/
object_t *cond(object_t *obj)
{
    if (obj == NULL)
        error("NULL in COND");
    object_t *pair = FIRST(obj);
    object_t *p = FIRST(pair);
    if (eval(p) == t)
        return eval(SECOND(pair));
    else
        return cond(TAIL(obj));
}

/**
 * Рекурсивно вычисляет список аргументов, создаёт новый список
 * @param args список аргументов
 * @return возвращает список вычисленных аргументов
 */
object_t *eval_args(object_t *args)
{
    if (args == NULL)
	return NULL;
    object_t *f = FIRST(args);
    return new_pair(eval(f), eval_args(TAIL(args))); 
}

/**
 * Вычисление выражения
 * Если выражение число, возвращаем его же
 * Если выражение атом, то выдаем ошибку
 * Если список, получаем первый элемент списка и обрабатываем эту функцию
 *  (quote a) -> a
 *  (eq 'a 'a) -> T
 *  (eq 'a 'b) -> ()
 *  (car '(1 2 3)) -> 1
 *  (cdr '(1 2 3)) -> (2 3)
 * @param obj входное выражение
 * @return возвращает вычисленный объект
 */
object_t *eval(object_t *obj)
{
    printf("eval: ");
    print_obj(obj);
    printf("\n");
    
    if (obj->type == NUMBER)
        return obj;
    else if (obj == t)
        return t;
    else if (obj->type == SYMBOL)
        error("Unknown SYMBOL \n");
    else if (obj->type == PAIR) {
	symbol_t *s = find_symbol(FIRST(obj)->u.symbol->str);
	object_t *args;
	if (s == quote_sym)
	    args = SECOND(obj);
	else
	    args = eval_args(TAIL(obj));
	return s->func(args);
    }
    else 
        error("Unknown func\n");
}

//инициализация примитивов
void init_eval()
{
  register_func("CAR", car);
  register_func("CDR", cdr);    
  register_func("EQ", eq);
  register_func("QUOTE", quote);
  register_func("CONS", cons);
  register_func("COND", cond);
  t = object_new(SYMBOL, "T");
  quote_sym = find_symbol("QUOTE");
  nil = NULL;
}
