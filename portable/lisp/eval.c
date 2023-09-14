#include <stdio.h>
#include "objects.h"
#include "parser.h"

// Первый элемент списка
#define FIRST(o) o->u.pair->left

// Второй элемент списка
#define SECOND(o) o->u.pair->right->u.pair->left

// Третий элемент списка
#define THIRD(o) o->u.pair->right->u.pair->right->u.pair->left

object_t *t;
object_t *nil;


/** 
 * возвращает первый элемент списка
 * 
 * @param list - список параметров
 * 
 * @return указатель на значиение первого элемента списка
 */
object_t *car(object_t *list)
{
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
  /*    if (list->type != PAIR)
        error("Not list in cons\n");
    object_t *p1 = FIRST(list);
    object_t *p2 = SECOND(list);
    if (p2->type != PAIR)
        error("second parameter not list");
	return new_pair(p1, p2);*/
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
    else if (obj->type == SYMBOL)
        error("Unknown SYMBOL \n");
    else if (obj->type == PAIR) {
      symbol_t *s = find_symbol(obj->u.symbol->str);
      return s->func(obj->u.pair->right);
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
  t = object_new(SYMBOL, "T");
  nil = NULL;
}
