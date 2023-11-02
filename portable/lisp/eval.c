#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "parser.h"
#include "eval.h"
#include "arith.h"

/// объект истина
object_t *t;
/// объект пусто
object_t *nil;
/// символ "QUOTE"
symbol_t *quote_sym;
/// символ "BACKQUOTE"
symbol_t *backquote_sym;
/// символ "LAMBDA"
symbol_t *lambda_sym;
/// символ "COND"
symbol_t *cond_sym;
/// символ "DEFUN"
symbol_t *defun_sym;
/// символ "DEFMACRO"
symbol_t *defmacro_sym;
/// символ "DEFVAR"
symbol_t *defvar_sym; 
/// символ "SETQ"
symbol_t *setq_sym; 
/// символ "T"
symbol_t *t_sym;
/// символ "NIL"
symbol_t *nil_sym;
/// текущее окружение
object_t *current_env;

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
    if (arg->type != PAIR){
        error("Not list in car\n");
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
    object_t *arg = FIRST(list);
    if (arg->type != PAIR){
        error("Not list in cdr\n");
    	return ERROR;
    }
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
    //printf("eq: ");
    //PRINT(list);
    object_t *p1 = FIRST(list);
    object_t *p2 = SECOND(list);
    //printf("p1: ");
    //PRINT(p1);
    //printf("p2: ");
    //PRINT(p2);
    if (p1 != NULL && p1->type != SYMBOL || p2 != NULL && p2->type != SYMBOL){
        error("not symbol in eq\n");
	return ERROR;
    }
    if (p1 == NULL && p2 == NULL || p1 != NULL && p2 != NULL && p1->u.symbol == p2->u.symbol)
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

/**
 * возвращает аргумент с вычислением
 * b = 7 `(a ,b c) -> (a 7 c)
 * b = (1 2) `(a ,b c) -> (a (1 2) c)
 * b = (1 2) `(a ,b c) -> (list 'a '(1 2) 'c)
 * b = (1 2) `(a (,b) c) -> (a ((1 2)) c)
 * b = (1 2) `(a ,@b c) -> (a 1 2 c)
 *
 * @param list - список параметров (1 парамет)
 *
 * @return аргумент
 */
object_t *backquote_rec(object_t *list)
{
    if (list == NULL)
	return NULL;
    //PRINT(list);
    object_t *o;
    if (list->type == NUMBER)
	return object_new(NUMBER, &list->u.value);
    else if (list->type == SYMBOL)
	return object_new(SYMBOL, list->u.symbol->str);
    /*else if (list->type == ARRAY) {
	array_t *arr = new_empty_array(list->u.arr->length);
	return object_new(ARRAY, list->u.arr->data);
	} */else if (list->type == STRING)
	return object_new(STRING, list->u.str->data);
    else if (list->type == PAIR)
	return new_pair(backquote_rec(FIRST(list)), backquote_rec(TAIL(list)));
    return ERROR;
}

object_t *backquote(object_t *list)
{
    if (list == NULL){
	error("backquote: NULL");
	return ERROR;
    }
    //PRINT(list);
    return backquote_rec(FIRST(list));
}

/** 
 * Конструирование объекта ожидает, что 2 параметр - список
 *
 * @param list - список параметров
 *
 * @return список который содержит 1 параметр, и продолжается с элементами 2 параметра.
 */
object_t *cons(object_t *list)
{			
    if (list->type != PAIR){
	error("Not list in cons\n");
	return ERROR;
    }
    if (list->u.pair->right->type != PAIR) {
        error("second parameter not list\n");
	return ERROR;
    }
    object_t *p1 = FIRST(list);
    object_t *p2 = SECOND(list);
    if (p2->type != PAIR){
	error("second parameter not list");
	return ERROR;
    }
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
    if (obj == NULL){
        error("NULL in COND");
	return ERROR;
    }
    object_t *pair = FIRST(obj);
    object_t *p = FIRST(pair);
    if (eval(p, current_env) == t)
        return eval(SECOND(pair), current_env);
    else
        return cond(TAIL(obj));
}

/** 
 * Создаёт новую функцию
 *
 * @param obj (имя_функции список_аргументов тело_функции)
 *
 * @return символ имени новой функции
 */
object_t *defun(object_t *obj)
{
    symbol_t *name = find_symbol(FIRST(obj)->u.symbol->str);
    name->lambda = new_pair(object_new(SYMBOL, "LAMBDA"), TAIL(obj));
    return object_new(SYMBOL, name->str);
}

/** 
 * Создаёт новый макрос
 *
 * @param obj (имя_макроса список_аргументов тело_макроса)
 *
 * @return символ имени нового макроса
 */
object_t *defmacro(object_t *obj)
{
    symbol_t *name = find_symbol(FIRST(obj)->u.symbol->str);
    name->macro = new_pair(object_new(SYMBOL, "LAMBDA"), TAIL(obj));
    return object_new(SYMBOL, name->str);
}

/** 
 * Создаёт новый глобальный символ (defvar имя значение)
 *
 * @param params (имя значение)
 *
 * @return объект переменной
 */
object_t *defvar(object_t *params)
{
    symbol_t *name = find_symbol(FIRST(params)->u.symbol->str);
    if (TAIL(params) == NULL)
        name->value = NULL;
    else
        name->value = eval(SECOND(params), NULL);
    return FIRST(params);
}    

/**
 * (progn e1 e2 .. en)
 * 
 * Вычисляет все выражения e1 .. en. 
 *
 * @param params - список (e1 e2 .. en)
 *
 * @return Возвращает результат последнего выражения en
 */
object_t *progn(object_t *params)
{
    if (params == NULL) {
	error("progn: params = NULL \n");
	return ERROR;
    } else if (params == ERROR)
	return ERROR;
    else if (TAIL(params) == NULL)
	return FIRST(params);
    return progn(TAIL(params));
}

/** 
 * Проверка списка на то, что все элементы типа type
 *
 * @param list - список
 *
 * @return 1 - параметры правильные, 0 - нет
 */
int check_params(object_t *list)
{
    if (list == NULL)
	return 1;
    if (FIRST(list)->type != SYMBOL)
        return 0;
    return check_params(TAIL(list));
}

/** 
 * Проверка объекта на то, что он является корректной lambda-функцией
 * (lambda params body)
 * params - список символов
 * body - тело функции, любой объект
 *
 * @param list - lambda выражение
 *
 * @return 0 или 1
 */
int is_lambda(object_t *list)
{
    object_t *lambda = FIRST(list);
    if (lambda->type != SYMBOL || lambda->u.symbol != lambda_sym){
	error("Invalid lambda symbol\n");
	return (int)ERROR;
    }
    if (list->u.pair->right == NULL){
	error("No params in lambda\n");
	return (int)ERROR;
    }
    object_t *params = SECOND(list);
    if (params->type != PAIR){
	error("Invalid params in lambda\n");
	return (int)ERROR;
    }
    if (!check_params(params)){
        error("Not symbol in lambda attrs\n");
        return 0;
    } else
	return 1;
}

/**
 * Создает окружение
 * 
 * @param args - список аргументов (x y)
 * @param values - список значений (1 2)
 *
 * @return окружение ((X 1) (Y 2))
 */
object_t *make_env(object_t *args, object_t *values)
{
    if (args != NULL && values == NULL){
	error("Not enough values for params\n");
	return ERROR;
    }
    if (args == NULL)
	return NULL;
    object_t *param = FIRST(args);
    object_t *val = FIRST(values);
    object_t *pair = new_pair(param, new_pair(val, nil));
    return new_pair(pair, make_env(TAIL(args), TAIL(values)));
}

/**
 * Поиск символа в окружении
 * 
 * @param env - окружение, где ищем
 * @param sym - символ, который ищем
 * @param res - результат поиска (значение аргумента)
 *
 * @return 1 - найдено, 0 - нет
 */
int find_in_env(object_t *env, object_t *sym, object_t **res)
{
    if (env == NULL)
	return 0;
    object_t *pair = FIRST(env);
    object_t *var = FIRST(pair);
    if (var->u.symbol  == sym->u.symbol){
	*res = SECOND(pair);
	return 1;
    } else
	return find_in_env(TAIL(env), sym, res);
}

/**
 * Объединить два списка
 * 
 * @param l1 - первый список
 * @param l2 - второй список
 */
void append_env(object_t *l1, object_t *l2)
{
    while (l1->u.pair->right != NULL)
	l1 = l1->u.pair->right;
    l1->u.pair->right = l2;
}

/**
 * Вычислить lambda функцию с заданными аргументами.
 * Подставить PROGN
 * (e1 ... en) -> (PROGN e1 ... en)
 * 
 * @param lambda - функция (lambda (x) e1 e2)
 * @param args - список значений аргументов (1)
 * @param env окружение
 * @return вычисленное значение функции
 */
object_t *eval_func(object_t *lambda, object_t *args, object_t *env)
{
    object_t *new_env = make_env(SECOND(lambda), args);
    object_t *body;
    if (TAIL(TAIL(lambda))->u.pair->right == NULL)
	body = THIRD(lambda);
    else
	body = new_pair(object_new(SYMBOL, "PROGN"), TAIL(TAIL(lambda)));
    if (body == ERROR)
	return ERROR;
    append_env(new_env, env);
    return eval(body, new_env);
}
    
/**
 * Рекурсивно вычисляет список аргументов, создаёт новый список
 * @param args список аргументов
 * @param env окружение
 * @return возвращает список вычисленных аргументов
 */
object_t *eval_args(object_t *args, object_t *env)
{
    //printf("eval_args: ");
    //PRINT(args);
    //printf(" ");
    //PRINT(env);
    if (args == NULL)
	return NULL;
    object_t *f = FIRST(args);
    //printf("f = %x pair = %x\n", f, f->u.pair);
    //PRINT(f);
    object_t *arg = eval(f, env);
    if (arg == ERROR)
	return ERROR;
    object_t *tail = eval_args(TAIL(args), env);
    if (tail == ERROR)
	return ERROR;
    return new_pair(arg, tail); 
}

/**
 * Проверка на специальную форму
 * @param s - имя функции
 * @return 1 - специальная форма, а 0 - нет
 */
int is_special_form(symbol_t *s)
{
    return s == quote_sym || s == defun_sym || s == defmacro_sym ||
	s == defvar_sym || s == setq_sym || s == backquote_sym ||
	s == cond_sym;
}

/**
 * Возвращает объект, соответствующий значению символа
 * @param obj - символьный объект
 * @return объект, соответствующий obj
 */
object_t *eval_symbol(object_t *obj, object_t *env)
{
    object_t *res;
    
    //printf("eval_symbol: ");
    //PRINT(obj);
    //printf("env: ");
    //PRINT(env);  
    
    if (find_in_env(env, obj, &res))
        return res;
    else {
	symbol_t *res_sym = check_symbol(obj->u.symbol->str);
        if (res_sym != NULL && res_sym->value != NOVALUE)
           return res_sym->value;
        else
           return ERROR;
    }
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
 * @param env окружение
 * @return возвращает вычисленный объект
 */
object_t *eval(object_t *obj, object_t *env)
{
    //printf("eval: ");
    //PRINT(obj);
    //printf("env: ");
    //PRINT(env);
    current_env = env;
    if (obj == NULL)
        return NULL;
    else if (obj->type == NUMBER || obj->type == STRING || obj->type == ARRAY)
	return obj;
    else if (obj->type == SYMBOL) {
        object_t *symbol = eval_symbol(obj, env);
        if (symbol == ERROR) {
	    error("Unknown SYMBOL \n");
	    return ERROR;
	}
        return symbol;
    } else if (obj->type == PAIR) {
	object_t *first = FIRST(obj);
	if (first->type == PAIR && is_lambda(first))
	    return eval_func(first, eval_args(TAIL(obj), env), env);
	symbol_t *s = find_symbol(first->u.symbol->str);
	object_t *args;
	if (is_special_form(s))
	    args = TAIL(obj);
	else
	    args = eval_args(TAIL(obj), env);
	if (s->lambda != NULL)
	    return eval_func(s->lambda, args, env);
	else if (s->func != NULL)
	    return s->func(args);
	else {
	    error("Unknown func\n");
	    return ERROR;
	}
    } else { 
        error("Unknown object_type\n");
	return ERROR;
    }
    current_env = env;
}

/**
 * Установление значения символа в окружении
 * 
 * @param env - окружение, где ищем символ
 * @param sym - символ, для которого устан. значение
 * @param val - устанавливаемое значение
 */
void set_in_env(object_t *env, object_t *sym, object_t *val)
{
    if (env == NULL) {
        error("ERROR: NULL as env in set_in_env");
        return;
    }
    object_t *pair = FIRST(env);
    object_t *var = FIRST(pair);
    if (var->u.symbol  == sym->u.symbol)
        SECOND(pair) = val;
    else
        set_in_env(TAIL(env), sym, val);
}

/**
 * Присвоение значения переменной
 * @param param параметры (символьный объект, значение, окружение)
 * @return возвращает значение переменной
 */
object_t *setq_rec(object_t *params)
{
    if (params == NULL)
        return NULL;
    symbol_t *sym = FIRST(params)->u.symbol;
    object_t *res;
    int find_res = find_in_env(current_env, FIRST(params), &res);
    if (!find_res)
        sym = find_symbol(FIRST(params)->u.symbol->str);
    if (TAIL(params) == NULL) {
	error("setq: no value");
	return ERROR;
    }
    object_t *obj = eval(SECOND(params), current_env);
    if (find_res)
        set_in_env(current_env, FIRST(params), obj);
    else
        sym->value = obj;
    if (TAIL(TAIL(params)) == NULL)
	return obj;
    return setq_rec(TAIL(TAIL(params)));
}

object_t *setq(object_t *params)
{
    if (params == NULL) {
	error("setq: params = NULL\n");
        return ERROR;
    }
    return setq_rec(params);
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
    register_func("BACKQUOTE",backquote);
    register_func("CONS", cons);
    register_func("COND", cond);
    register_func("DEFUN", defun);
    register_func("DEFMACRO", defmacro);
    register_func("DEFVAR", defvar);
    register_func("PROGN", progn);
    register_func("SETQ", setq);
    t = object_new(SYMBOL, "T");
    nil = NULL;
    quote_sym = find_symbol("QUOTE");
    backquote_sym = find_symbol("BACKQUOTE");
    lambda_sym = find_symbol("LAMBDA");
    cond_sym = find_symbol("COND");
    defun_sym = find_symbol("DEFUN");
    defmacro_sym = find_symbol("DEFMACRO");
    defvar_sym = find_symbol("DEFVAR");
    setq_sym = find_symbol("SETQ");
    t_sym = t->u.symbol;
    t_sym->value = t;
    nil_sym = find_symbol("NIL");
    nil_sym->value = nil;
}
