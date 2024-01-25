#include <stdio.h>
#include <string.h>
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
/// символ "SETQ"
symbol_t *setq_sym;
/// символ "OR"
symbol_t *or_sym;
/// символ "AND"
symbol_t *and_sym;
/// символ "T"
symbol_t *t_sym;
/// символ "NIL"
symbol_t *nil_sym;
/// символ "&REST"
symbol_t *rest_sym;
/// текущее окружение
object_t *current_env;

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
    if (list == NULL) {
	error("eq: no args");
	return ERROR;
    }
    if (TAIL(list) == NULL) {
	error("eq: one arg");
	return ERROR;
    }
    if (TAIL(TAIL(list)) != NULL) {
	error("eq: too many args");
	return ERROR;
    }
        object_t *p1 = FIRST(list);
    object_t *p2 = SECOND(list);
    //printf("p1: ");
    //PRINT(p1);
    //printf("p2: ");
    //PRINT(p2);
    /*    if (p1 != NULL && p1->type != SYMBOL || p2 != NULL && p2->type != SYMBOL){
        error("not symbol in eq");
	return ERROR;
	}*/
    if (p1 == NULL && p2 == NULL || p1 != NULL && p2 != NULL && p1->u.symbol == p2->u.symbol && p1->type == SYMBOL && p2->type == SYMBOL)
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
object_t *atom(object_t *list)
{
    if (list == NULL) {
	error("atom: no args");
	return ERROR;
    }
    else if (TAIL(list) != NULL) {
	error("atom: many args");
	return ERROR;
    }
    object_t *obj = FIRST(list);
    if (obj == NULL || obj->type != PAIR)
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
    if (TAIL(list) != NULL) {
	error("quote: many args");
	return ERROR;
    }
    return FIRST(list);
}

void append_env(object_t *l1, object_t *l2);

/**
 * возвращает аргумент с вычислением
 * b = 7 `(a ,b c) -> (a 7 c)
 * (a (COMMA b) c)
 * b = (1 2) `(a ,b c) -> (a (1 2) c)
 * b = (1 2) `(a ,b c) -> (list 'a '(1 2) 'c)
 * b = (1 2) `(a (,b) c) -> (a ((1 2)) c)
 * b = (1 2) `(a ,@b c) -> (a 1 2 c)
 * (A (COMMA-AT B) C)
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
    else if (list->type == ARRAY) {
	array_t *arr = new_empty_array(list->u.arr->length);
	int l = list->u.arr->length;
	for (int i = 0; i < l; i++)
	    arr->data[i] = backquote_rec(list->u.arr->data[i]);
	return object_new(ARRAY, arr);
    } else if (list->type == STRING)
	return object_new(STRING, list->u.str->data);
    else if (list->type == PAIR) {
	object_t *el = FIRST(list); // list = (COMMA B)
	if (el == NULL)
	    return new_pair(NULL, backquote_rec(TAIL(list)));
	if (el->type == SYMBOL && !strcmp(el->u.symbol->str, "BACKQUOTE"))
	    return list;
	if (el->type == SYMBOL && !strcmp(el->u.symbol->str, "COMMA"))
	    return eval(SECOND(list), current_env);
	object_t *first = backquote_rec(el);
	if (first == ERROR)
	    return ERROR;
	if (first != NULL && first->type == PAIR) {  // first = (COMMA-AT B)
	    object_t *comma_at = FIRST(first);
	    if (comma_at != NULL && comma_at->type == SYMBOL && !strcmp(comma_at->u.symbol->str, "COMMA-AT")) {
		object_t *l = eval(SECOND(first), current_env);
		if (l == NULL)
		    return backquote_rec(TAIL(list));
		/*if (l->type != PAIR) {
			error("COMMA-AT: not list");
			return ERROR;
			}*/
		object_t *new_comma = backquote_rec(l);
		append_env(new_comma, backquote_rec(TAIL(list)));
		return new_comma;
	    }
	}
	object_t *tail = backquote_rec(TAIL(list));
	if (tail == ERROR)
	    return ERROR;
	return new_pair(first, tail);
    }
    return ERROR;   
}

/**
 * (BACKQOUTE (a b c))
 *
 */
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
    if (TAIL(pair) == NULL)
    {
        error("cond: not enough params");
        return ERROR;
    }
    if (TAIL(TAIL(pair)) != NULL)
    {
        error("cond: too many params");
        return ERROR;
    }
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
	error("progn: params = NULL");
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
	error("Invalid lambda symbol");
	return 0;
    }
    if (TAIL(list) == NULL){
	error("No params in lambda");
	return 0;
    }
    object_t *params = SECOND(list);
    if (params != NULL && params->type != PAIR){
	error("Invalid params in lambda");
	return 0;
    }
    if (!check_params(params)){
        error("Not symbol in lambda attrs");
        return 0;
    }
    if (TAIL(TAIL(list)) == NULL) {
	error("No body in lambda");
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
 * @return окружение ((X.1) (Y.2))
 */
object_t *make_env(object_t *args, object_t *values)
{
    if (args != NULL && values == NULL && FIRST(args)->u.symbol != rest_sym){
	error("Not enough values for params");
	return ERROR;
    }
    if (args == NULL && values != NULL) {
	error("Invalid number of arguments");
	return ERROR;
    }
    if (args == NULL)
	return NULL;
    if (values == ERROR)
	return ERROR;
    object_t *param = FIRST(args);
    if (param->u.symbol == rest_sym) {
        if (TAIL(args) == NULL) {
            error("Missing parameter after &rest");
            return ERROR;
        }
        if (TAIL(TAIL(args)) != NULL) {
            error("Too many parameters after &rest");
            return ERROR;
        }
        return new_pair(new_pair(SECOND(args), values), nil);
    }
    object_t *val = FIRST(values);
    object_t *pair = new_pair(param, val);
    object_t *new_env = make_env(TAIL(args), TAIL(values));
    if (new_env == ERROR)
        return ERROR;
    return new_pair(pair, new_env);
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
	*res = TAIL(pair);
	return 1;
    } else
	return find_in_env(TAIL(env), sym, res);
}

/**
 * К первому списку присоединяет второй список
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
    if (new_env == ERROR)
	return ERROR;
    object_t *body;
    if (TAIL(TAIL(lambda))->u.pair->right == NULL)
	body = THIRD(lambda);
    else
	body = new_pair(object_new(SYMBOL, "PROGN"), TAIL(TAIL(lambda)));
    if (body == ERROR)
	return ERROR;
    if (new_env == NULL)
	new_env = env;
    else
	append_env(new_env, env);
    return eval(body, new_env);
}

/**
 * Вычислить macro подстановку с заданными аргументами.
 * 
 * @param macro - тело макроса (lambda (x) e1 e2 .. en)
 * @param args - список значений аргументов (1)
 * @param env окружение
 * @return вычисленное значение макроса
 */
object_t *macro_call(object_t *macro, object_t *args, object_t *env)
{
    object_t *new_env = make_env(SECOND(macro), args);
    object_t *body;
    object_t *eval_res;
    if (new_env == ERROR)
	return ERROR;
    body = TAIL(TAIL(macro));
    append_env(new_env, env);
    while (body != NULL) {
	eval_res = eval(FIRST(body), new_env);
	if (eval_res == ERROR)
	    return ERROR;
	//printf("macro = ");
	//PRINT(eval_res);
	eval_res = eval(eval_res, new_env);
	body = TAIL(body);
    }
    return eval_res;
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
    //printf("f = %x pair = %x", f, f->u.pair);
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
        s == setq_sym || s == backquote_sym ||
	s == cond_sym || s == or_sym || s == and_sym;
}

/**
 * Возвращает объект, соответствующий значению символа
 * @param obj - символьный объект
 * @return объект, соответствующий obj
 */
object_t *eval_symbol(object_t *obj)
{
    object_t *res;
    
    //    printf("eval_symbol: ");
    //    PRINT(obj);
    //    printf("env: ");
    //    PRINT(env);  
    
    if (find_in_env(current_env, obj, &res))
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
    // PRINT(obj);
    //printf("env: ");
    //PRINT(env);
    current_env = env;
    if (obj == NULL)
        return NULL;
    else if (obj->type == NUMBER || obj->type == STRING || obj->type == ARRAY)
	return obj;
    else if (obj->type == SYMBOL) {
        object_t *symbol = eval_symbol(obj);
        if (symbol == ERROR) {
            error("Unknown SYMBOL: %s", obj->u.symbol->str);
            return ERROR;
	}
        return symbol;
    } else if (obj->type == PAIR) {
	object_t *first = FIRST(obj);
	if (first->type == PAIR)
	    if(is_lambda(first) == 1)
		return eval_func(first, eval_args(TAIL(obj), env), env);
	    else {
	        error("");
	        return ERROR;
	    }
	symbol_t *s = find_symbol(first->u.symbol->str);
	object_t *args;
	if (is_special_form(s) || s->macro != NULL)
	    args = TAIL(obj);
	else
	    args = eval_args(TAIL(obj), env);
	if (args == ERROR){
	    printf("Error in args: ");
	    PRINT(obj);
	    error("");
	    return ERROR;
	}
	if (s->lambda != NULL)
	    return eval_func(s->lambda, args, env);
	else if (s->func != NULL)
	    return s->func(args);
	else if (s->macro != NULL)
	    return macro_call(s->macro, args, env);
	else {
	    error("Unknown func: %s", s->str);
	    return ERROR;
	}
    } else { 
        error("Unknown object_type");
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
        TAIL(pair) = val;
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
    if (obj == ERROR)
	return ERROR;
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
	error("setq: params = NULL");
        return ERROR;
    }
    return setq_rec(params);
}

/**
 * Логическое И (and (= 1 2) (= 2 2))
 * Возвращает результат ЛОЖЬ после нахождения первого ложного условия
 * Должно быть хотя бы одно условие
 * @param param параметры (условие 1, условие 2, и т.д.)
 * @return возвращает результат И
 */
object_t *and(object_t *params)
{
    if (params == NULL) {
	error("and: no params");
	return ERROR;
    }
    while (params != NULL) {
	object_t *first = FIRST(params);
	object_t *res = eval(first, current_env);
	if (res == nil)
	    return nil;
	else if (res == t)
	    params = TAIL(params);
	else {
	    error("and: invalid param");
	    return ERROR;
	}
    }
    return t;
}

/**
 * Логическое ИЛИ (or (= 1 2) (= 2 2))
 * Возвращает результат ИСТИНА после нахождения первого истинного условия
 * Должно быть хотя бы одно условие
 * @param param параметры (условие 1, условие 2, и т.д.)
 * @return возвращает результат ИЛИ
 */
object_t *or(object_t *params)
{
    if (params == NULL) {
	error("or: no params");
	return ERROR;
    }
    while (params != NULL) {
	object_t *first = FIRST(params);
	object_t *res = eval(first, current_env);
	if (res == t)
	    return t;
	else if (res == nil)
	    params = TAIL(params);
	else {
	    error("or: invalid param");
	    return ERROR;
	}
    }
    return nil;
}

/**
 * Выполняет макро подстановку
 * @param param параметры (if (= 1 1) 2 3)
 * @return возвращает результат макро подстановки
 */
object_t *macroexpand(object_t *params)
{
    if (params == NULL) {
	error("macroexpand: no params");
	return ERROR;
    }
    if (TAIL(params) != NULL) {
	error("macroexpand: many params");
	return ERROR;
    }
    object_t *macro_c = FIRST(params);
    if (macro_c->type != PAIR) {
	error("macroexpand: invalid macro call");
	return ERROR;
    }
    object_t *macro_name = FIRST(macro_c);
    object_t *macro;
    if (macro_name == NULL || macro_name->type != SYMBOL || macro_name->u.symbol->macro == NULL) {
	error("macroexpand: invalid macro");
	return ERROR;
    }
    macro = macro_name->u.symbol->macro;
    object_t *args = TAIL(macro_c);
    object_t *new_env = make_env(SECOND(macro), args);
    if (new_env == ERROR)
	return ERROR;
    append_env(new_env, current_env);
    object_t *body = TAIL(TAIL(macro));
    object_t *eval_res;
    object_t *res = NULL;
    while (body != NULL) {
	eval_res = eval(FIRST(body), new_env);
	if (eval_res == ERROR)
	    return ERROR;
	if (res == NULL)
	    res = eval_res;
	else if (res->type == STRING)
	    res = NULL;
	else
	    append_env(res, eval_res);
	body = TAIL(body);
    }
    return res;
}

/**
 * Выполняет функцию с аргументами
 * @param param параметры (функция аргумент 1, аргумент 2 ...)
 * @return возвращает результат выполнения функции
 */
object_t *funcall(object_t *params)
{
    if (params == NULL) {
        error("funcall: no arguments");
        return ERROR;
    }
    object_t *func = FIRST(params);
    if (func == NULL || func->type != SYMBOL &&
        !(func->type == PAIR && is_lambda(func) == 1)) {
        error("funcall: invalid func");
        return ERROR;
    }
    object_t *args = TAIL(params);
    if (func->type == PAIR)
        return eval_func(func, args, current_env);
    symbol_t *s = find_symbol(func->u.symbol->str);
    if (s->lambda != NULL)
        return eval_func(s->lambda, args, current_env);
    else if (s->func != NULL)
	return s->func(args);
    else {
	printf("Unknown func: %s", s->str);
	return ERROR;
    }
}

/**
 * Возвращает список из аргументов
 * @param args (аргумент 1, аргумент 2 ...)
 * @return возвращает список из аргументов
 */
object_t *list(object_t *args)
{
    return args;
}

/**
 * Вычисляет свой аргумент
 * @param args (выражение)
 * @return возвращает список из аргументов
 */
object_t *lisp_eval(object_t *args)
{
    return eval(FIRST(args), current_env);
}

/** 
 * инициализация примитивов 
 */
void init_eval()
{
    register_func("ATOM", atom);
    register_func("EQ", eq);
    register_func("QUOTE", quote);
    register_func("BACKQUOTE",backquote);
    register_func("COND", cond);
    register_func("DEFUN", defun);
    register_func("DEFMACRO", defmacro);
    register_func("PROGN", progn);
    register_func("SETQ", setq);
    register_func("OR", or);
    register_func("AND", and);
    register_func("MACROEXPAND", macroexpand);
    register_func("FUNCALL", funcall);
    register_func("LIST", list);
    register_func("EVAL", lisp_eval);
    t = object_new(SYMBOL, "T");
    nil = NULL;
    quote_sym = find_symbol("QUOTE");
    backquote_sym = find_symbol("BACKQUOTE");
    lambda_sym = find_symbol("LAMBDA");
    cond_sym = find_symbol("COND");
    defun_sym = find_symbol("DEFUN");
    defmacro_sym = find_symbol("DEFMACRO");
    setq_sym = find_symbol("SETQ");
    or_sym = find_symbol("OR");
    and_sym = find_symbol("AND");
    t_sym = t->u.symbol;
    t_sym->value = t;
    nil_sym = find_symbol("NIL");
    nil_sym->value = nil;
    rest_sym = find_symbol("&REST");
}
