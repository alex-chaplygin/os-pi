#include "list.h"
#include "parser.h"

// Второй элемент списка
#define CDAR(o) o->u.list->next->elem

// Третий элемент списка
#define CDDAR(o) o->u.list->next->next->elem

atom_t *car;
atom_t *cdr;
atom_t *quote;
atom_t *eq;
object_t *t;
object_t *nil;

// возвращает первый элемент списка
// list - объект типа список
object_t *CAR(object_t *list)
{
    if (list->type != LIST)
        error("Not list in car\n");
    return list->u.list->elem;
}

// возвращает список без первого элемента
// list - объект типа список
object_t *CDR(object_t *list)
{
    if (list->type != LIST)
        error("Not list in cdr\n");
    return object_new(LIST, list->u.list->next);
}

// (eq 'a 'a) -> T
// (eq 'a 'b) -> ()
object_t *eval_eq(object_t *p1, object_t *p2)
{
    if (p1->type != ATOM || p2->type != ATOM)
        error("not atom in eq\n");
    if (p1->u.atom == p2->u.atom)
        return t;
    else
        return nil;
}

//Вычисление выражения
//Если выражение число, возвращаем его же
//Если выражение атом, то выдаем ошибку
//Если список, получаем первый элемент списка и обрабатываем эту функцию
// (quote a) -> a
// (eq 'a 'a) -> T
// (eq 'a 'b) -> ()
// (car '(1 2 3)) -> 1
// (cdr '(1 2 3)) -> (2 3)
//obj - входное выражение
//возвращает вычисленный объект
object_t *eval(object_t *obj)
{
    if (obj->type == NUMBER)
        return obj;
    else if (obj->type == ATOM)
        error("Unknown ATOM \n");
    else if (CAR(obj)->u.atom == quote)
        return CDAR(obj);
    else if (CAR(obj)->u.atom == eq)
        return eval_eq(eval(CDAR(obj)), eval(CDDAR(obj)));
    else if (CAR(obj)->u.atom == car)
        return CAR(eval(CDAR(obj)));
    else if (CAR(obj)->u.atom == cdr)
        return CDR(eval(CDAR(obj)));    
    else 
        error("Unknown func\n");
}

//инициализация примитивов
void init_eval()
{
    car = find_atom("CAR");
    cdr = find_atom("CDR");
    quote = find_atom("QUOTE");
    eq = find_atom("EQ");
    t = object_new(ATOM, "T");
    nil = object_new(LIST, 0);
}