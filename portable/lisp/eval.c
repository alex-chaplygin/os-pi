#include <stdio.h>
#include "objects.h"
#include "parser.h"
#include "eval.h"

/// объект истина
object_t *t;
/// объект пусто
object_t *nil;
/// символ "QUOTE"
symbol_t *quote_sym;
symbol_t *lambda_sym;

/** 
 * возвращает первый элемент списка
 * 
 * @param list - список параметров
 * 
 * @return указатель на значиение первого элемента списка
 */
object_t *car(object_t *list)
{
    object_t *arg = FIRST(list);
    if (arg->type != PAIR)
        error("Not list in car\n");
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
    object_t *arg = FIRST(list);
    if (arg->type != PAIR)
        error("Not list in cdr\n");
    return TAIL(arg);
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
        return nil;
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
    if (list->type != PAIR)
	error("Not list in cons\n");
    object_t *p1 = FIRST(list);
    object_t *p2 = SECOND(list);
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
 * Проверка списка на то, что все элементы типа type
 *
 * @param list - список, type - тип
 *
 * @return 0 или 1
 */
int is_arr_ells_of_type(object_t *list, type_t type){
    if(list->u.pair->left->type != type)
        return 0;
    
    if(list->u.pair->right == NULL)
        return 1;
    
    return is_arr_ells_of_type(list->u.pair->right, type);
}

/** 
 * Проверка объекта на то, что он является корректной lambda-функцией
 *
 * @param list - список параметров
 *
 * @return 0 или 1
 */
int is_lambda(object_t *list){
    if(!(FIRST(list) != NULL && list->u.pair->right != NULL &&
        SECOND(list) != NULL &&
        FIRST(list)->type == PAIR && SECOND(list)->type == PAIR))
      {
        error("Invalid lambda declaration. Example: (lambda (p1 ... pn) e)\n");
        return 0;
      }

    object_t *args = FIRST(list);
    object_t *temp_el = args;

    int ress = is_arr_ells_of_type(temp_el, SYMBOL);

    if(ress == 0){
        error("Not symbol in lambda attrs\n");
        return 0;
    }

    return 1;
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
    if (obj->type == NUMBER)
        return obj;
    else if (obj == t)
        return t;
    else if (obj->type == SYMBOL)
        error("Unknown SYMBOL \n");
    else if (obj->type == PAIR) {
	symbol_t *s = find_symbol(FIRST(obj)->u.symbol->str);
	object_t *args;
	if (s == quote_sym || s == lambda_sym)
	    args = TAIL(obj);
	else
	    args = eval_args(TAIL(obj));
	return s->func(args);
    }
    else 
        error("Unknown func\n");
}

/** 
 * инициализация примитивов 
 */
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
  lambda_sym = find_symbol("LAMBDA");
  nil = NULL;
}
