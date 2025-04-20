#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include "objects.h"
#include "cont.h"
#include "symbols.h"
#include "parser.h"
#include "eval.h"
#include "arith.h"
#include "bind.h"

#define DEBUG_STACK_MAX_FRAME 7

/// объект истина
object_t t;
/// объект пусто
object_t nil;
/// символ "QUOTE"
symbol_t *quote_sym;
/// символ "BACKQUOTE"
symbol_t *backquote_sym;
/// символ "LAMBDA"
symbol_t *lambda_sym;
/// символ "IF"
symbol_t *if_sym;
/// символ "DEFUN"
symbol_t *defun_sym;
/// символ "DEFMACRO"
symbol_t *defmacro_sym;
/// символ "SETQ"
symbol_t *setq_sym;
/// символ "T"
symbol_t *t_sym;
/// символ "NIL"
symbol_t *nil_sym;
/// символ "&REST"
symbol_t *rest_sym;
/// символ "TAGBODY"
symbol_t *tagbody_sym;
/// символ "GO"
symbol_t *go_sym;
/// символ "CATCH"
symbol_t *catch_sym;
/// символ "THROW"
symbol_t *throw_sym;
/// символ "LABELS"
symbol_t *labels_sym;
/// символ "PROGN"
symbol_t *progn_sym;
/// символ "FUNCTION"
symbol_t *func_sym;
/// текущее окружение
object_t current_env = NULLOBJ;
/// окружение для пользовательских функций
object_t func_env = NULLOBJ;
/// точка вычисления меток tagbody
extern continuation_t tagbody_buffers[MAX_TAGBODY_SIZE];
///текущий индекс-буфер для tagbody
extern int tb_index_buf;
/// точка для возврата в цикл REPL
jmp_buf repl_buf;
/// точка возврата из catch
jmp_buf catch_buf;
///текущая метка перехода go
object_t cur_label = NULLOBJ;
#ifdef DEBUG
/// Стек
object_t debug_stack = NULLOBJ;

/*
 * Выводит текущее состояние стека.
 */
void print_debug_stack()
{
    object_t current = debug_stack;
    int frame = 0;
    while (current != NULLOBJ) {
#ifdef OS
	if (frame == DEBUG_STACK_MAX_FRAME)
	    break;
#endif
	// Выводим текущий элемент стека
	printf("#%d: ", frame++);
	PRINT(FIRST(current));
	current = TAIL(current);
    }
}
#endif

// (eq 'a 'a) -> T 
// (eq 'a 'b) -> () 
/*  
 * возвращает t если значения первых двух элементов списка это один и тот же символ а иначе возвращает nil 
 * 
 * @param list - список параметров 
 * 
 * @return t или nil 
 */ 
object_t eq(object_t list) 
{ 
    //printf("eq: "); 
    //PRINT(list); 
    if (list == NULLOBJ)
	error("eq: no args"); 
    if (TAIL(list) == NULLOBJ)
	error("eq: one arg"); 
    if (TAIL(TAIL(list)) != NULLOBJ) 
	error("eq: too many args"); 
    object_t p1 = FIRST(list); 
    object_t p2 = SECOND(list);  
    if (p1 == p2)
	return t; 
    else 
	return nil; 
} 

/*  
 * если аргумент атом, то возвращает T, иначе NIL 
 * 
 * @param list - список параметров 
 * 
 * @return t или nil 
 */ 
object_t atom(object_t list) 
{ 
    if (list == NULLOBJ)
	error("atom: no args");
    else if (TAIL(list) != NULLOBJ)
	error("atom: many args"); 
    object_t obj = FIRST(list); 
    if (obj == NULLOBJ || TYPE(obj) != PAIR) 
	return t; 
    else 
	return nil;
} 

/** 
 * Создает копию объекта
 *
 * @param o объект
 *
 * @return копия объекта
 */
object_t make_copy(object_t o)
{
    if (o == NULLOBJ) 
 	return NULLOBJ;
    if (TYPE(o) == BIGNUMBER) 
	return new_bignumber(GET_BIGNUMBER(o)->value);
    else if (TYPE(o) == FLOAT)
	return new_float(GET_FLOAT(o)->value);
    else if (TYPE(o) == CHAR || TYPE(o) == NUMBER || TYPE(o) == SYMBOL)
	return o;
    else if (TYPE(o) == ARRAY) { 
 	array_t *arr = new_empty_array(GET_ARRAY(o)->length); 
 	int l = GET_ARRAY(o)->length; 
 	for (int i = 0; i < l; i++) 
 	    arr->data[i] = make_copy(GET_ARRAY(o)->data[i]); 
 	return NEW_OBJECT(ARRAY, arr); 
    } else if (TYPE(o) == STRING) 
	return NEW_STRING(GET_STRING(o)->data); 
    else if (TYPE(o) == PAIR)
	return new_pair(make_copy(FIRST(o)), make_copy(TAIL(o)));
    else
	error("quote: invalid object");
}

/*  
 * возвращает свой аргумент без вычисления 
 * 
 * @param list - список параметров (1 параметр) 
 * 
 * @return аргумент 
 */
object_t quote(object_t list) 
{
    if (list == NULLOBJ)
	error("quote: empty");
    else if (TAIL(list) != NULLOBJ)
 	error("quote: many args");
    return FIRST(list); 
} 

void append_env(object_t l1, object_t l2); 

/* 
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
object_t backquote_rec(object_t list) 
{
    object_t res = NULLOBJ;
    object_t first = NULLOBJ;
    object_t l = NULLOBJ;
    if (list == NULLOBJ) 
 	return NULLOBJ;
    PROTECT3(list, res, first);
    object_t env = current_env;
    object_t func = func_env;
    if (TYPE(list) == BIGNUMBER || TYPE(list) == FLOAT || TYPE(list) == CHAR || TYPE(list) == NUMBER || TYPE(list) == SYMBOL || TYPE(list) == STRING)
	res = make_copy(list);
    else if (TYPE(list) == ARRAY) { 
 	array_t *arr = new_empty_array(GET_ARRAY(list)->length); 
 	res = NEW_OBJECT(ARRAY, arr); 
 	int l = GET_ARRAY(list)->length; 
 	for (int i = 0; i < l; i++) 
 	    arr->data[i] = backquote_rec(GET_ARRAY(list)->data[i]); 
    } else if (TYPE(list) == PAIR) {
 	object_t el = FIRST(list); // list = (COMMA B)
	if (TYPE(el) == SYMBOL && !strcmp(GET_SYMBOL(el)->str, "BACKQUOTE"))
	    res = list;
	else if (TYPE(el) == SYMBOL && !strcmp(GET_SYMBOL(el)->str, "COMMA") && TAIL(list) != NULLOBJ)
 	    res = eval(SECOND(list), env, func);
	else {
	    first = backquote_rec(el); 
	    if (first != NULLOBJ && TYPE(first) == PAIR) {  // first = (COMMA-AT B) 
		object_t comma_at = FIRST(first);
		if (comma_at != NULLOBJ && TYPE(comma_at) == SYMBOL && !strcmp(GET_SYMBOL(comma_at)->str, "COMMA-AT") && TAIL(first) != NULLOBJ) {
		    l = eval(SECOND(first), env, func);
		    if (l == NULLOBJ) {
			res = backquote_rec(TAIL(list));
			UNPROTECT;
			return res;
		    } else if (TYPE(l) != PAIR)
			error("COMMA-AT: not list");
		    else {
			res = backquote_rec(l); 
			append_env(res, backquote_rec(TAIL(list))); 
			UNPROTECT;
			return res;
		    }
		}
	    }
	    object_t tail = backquote_rec(TAIL(list)); 
	    res = new_pair(first, tail);
	}
    } else
	error("backqoute: unknown type: %d\n", TYPE(list));    
    UNPROTECT;
    return res;
}
/* 
 * (BACKQOUTE (a b c)) 
 * 
 */ 
object_t backquote(object_t list) 
{ 
    if (list == NULLOBJ)
 	error("backquote: NULLOBJ"); 
    //PRINT(list); 
    return backquote_rec(FIRST(list)); 
}

/* 
 * (if test True False)
 * Обработка условия. Истинное условие - не nil
 * Возвращаем список объектов из выражения, оцененных  eval 
 * @param obj - список парамметров (<Условие> <Выражение по истине> <Выражение по лжи>)
 * @return возвращает значение соответствующего выражения
 */ 
object_t IF(object_t obj)
{
    if (obj == NULLOBJ)
	error("NULLOBJ in IF");    
    object_t env = current_env;
    object_t func = func_env;
    object_t res;
    if (TAIL(obj) == NULLOBJ)
	error("True is empty");
    if (TAIL(TAIL(obj)) == NULLOBJ)
	error("False is empty");
    else if (TAIL(TAIL(TAIL(obj))) != NULLOBJ)
	error("if: too many params");
    PROTECT1(obj);
    if (eval(FIRST(obj), env, func) != nil)
	res = eval(SECOND(obj), env, func);
    else
	res = eval(THIRD(obj), env, func);
    UNPROTECT;
    return res;
}

/*  
 * Создаёт новую функцию 
 * 
 * @param obj (имя_функции список_аргументов тело_функции) 
 * 
 * @return символ имени новой функции 
 */ 
object_t defun(object_t obj) 
{
    if (obj == NULLOBJ)
	error("defun: empty");
    symbol_t *name = find_symbol(GET_SYMBOL(FIRST(obj))->str); 
    name->lambda = new_pair(NEW_SYMBOL("LAMBDA"), TAIL(obj));
    set_global(name);
    return NEW_SYMBOL(name->str);  
}

/*  
 * Создаёт новый макрос 
 * 
 * @param obj (имя_макроса список_аргументов тело_макроса) 
 * 
 * @return символ имени нового макроса 
 */ 
object_t defmacro(object_t obj) 
{ 
    if (obj == NULLOBJ)
	error("defmacro: empty");
    symbol_t *name = find_symbol(GET_SYMBOL(FIRST(obj))->str); 
    name->macro = new_pair(NEW_SYMBOL("LAMBDA"), TAIL(obj));
    set_global(name);
    return NEW_SYMBOL(name->str);
}

/* 
 * (progn e1 e2 .. en) 
 *  
 * Вычисляет все выражения e1 .. en.  
 * 
 * @param params - список (e1 e2 .. en) 
 * 
 * @return Возвращает результат последнего выражения en 
 */ 
object_t progn(object_t params) 
{ 
    if (params == NULLOBJ)
	return NULLOBJ;
    object_t env = current_env;
    object_t func = func_env;
    object_t obj = NULLOBJ;
    PROTECT1(params);
    while (params != NULLOBJ) {
	obj = eval(FIRST(params), env, func);
	params = TAIL(params);
    }
    UNPROTECT
    return obj;
} 

/*  
 * Проверка списка на то, что все элементы типа type 
 * 
 * @param list - список 
 * 
 * @return 1 - параметры правильные, 0 - нет 
 */ 
int check_params(object_t list) 
{ 
    if (list == NULLOBJ) 
 	return 1; 
    if (TYPE(FIRST(list)) != SYMBOL) 
	return 0; 
    return check_params(TAIL(list)); 
} 

/*  
 * Проверка объекта на то, что он является корректной lambda-функцией 
 * (lambda params body) 
 * params - список символов 
 * body - тело функции, любой объект 
 * 
 * @param list - lambda выражение 
 * 
 * @return 0 или 1 
 */ 
int is_lambda(object_t list) 
{
    if (list == NULLOBJ)
	return 0;
    object_t lambda = FIRST(list); 
    if (TYPE(lambda) != SYMBOL || GET_SYMBOL(lambda) != lambda_sym)
	return 0; 
    if (TAIL(list) == NULLOBJ)
	return 0; 
    object_t params = SECOND(list); 
    if (params != NULLOBJ && TYPE(params) != PAIR)
	return 0;
    if (!check_params(params))
	return 0; 
    if (TAIL(TAIL(list)) == NULLOBJ)
	return 0;
    return 1;
} 

/* 
 * Создает окружение 
 *  
 * @param args - список аргументов (x y) 
 * @param values - список значений (1 2) 
 * 
 * @return окружение ((X.1) (Y.2)) 
 */ 
object_t make_env(object_t args, object_t values) 
{ 
    if (args != NULLOBJ && values == NULLOBJ && GET_SYMBOL(FIRST(args)) != rest_sym)
	error("Not enough values for params"); 
    if (args == NULLOBJ && values != NULLOBJ)
	error("Invalid number of arguments"); 
    if (args == NULLOBJ) 
	return NULLOBJ;
    object_t param = FIRST(args); 
    if (GET_SYMBOL(param) == rest_sym) { 
	if (TAIL(args) == NULLOBJ)
	    error("Missing parameter after &rest"); 
	if (TAIL(TAIL(args)) != NULLOBJ)
	    error("Too many parameters after &rest"); 
	return new_pair(new_pair(SECOND(args), values), nil); 
    } 
    object_t val = FIRST(values); 
    object_t pair = new_pair(param, val); 
    object_t new_env = make_env(TAIL(args), TAIL(values)); 
    return new_pair(pair, new_env); 
} 

/* 
 * Поиск символа в окружении 
 *  
 * @param env - окружение, где ищем 
 * @param sym - символ, который ищем 
 * @param res - результат поиска (значение аргумента) 
 * 
 * @return 1 - найдено, 0 - нет 
 */ 
int find_in_env(object_t env, object_t sym, object_t *res) 
{ 
    if (env == NULLOBJ) 
 	return 0; 
    object_t pair = FIRST(env); 
    object_t var = FIRST(pair); 
    if (GET_SYMBOL(var)  == GET_SYMBOL(sym)){ 
 	*res = TAIL(pair); 
 	return 1; 
    } else 
 	return find_in_env(TAIL(env), sym, res); 
} 

/* 
 * К первому списку присоединяет второй список 
 *  
 * @param l1 - первый список 
 * @param l2 - второй список 
 */ 
void append_env(object_t l1, object_t l2) 
{ 
    while (GET_PAIR(l1)->right != NULLOBJ) 
 	l1 = GET_PAIR(l1)->right; 
    GET_PAIR(l1)->right = l2; 
} 

/* 
 * Вычислить lambda функцию с заданными аргументами. 
 * Подставить PROGN 
 * (e1 ... en) -> (PROGN e1 ... en) 
 *  
 * @param lambda - функция (lambda (x) e1 e2) 
 * @param args - список значений аргументов (1) 
 * @param env окружение 
 * @param func окружение функций
 * @return вычисленное значение функции 
 */ 
object_t eval_func(object_t lambda, object_t args, object_t env, object_t func) 
{ 
    object_t new_env = make_env(SECOND(lambda), args); 
    object_t body; 
    if (GET_PAIR(TAIL(TAIL(lambda)))->right == NULLOBJ) 
 	body = THIRD(lambda); 
    else 
 	body = new_pair(NEW_SYMBOL("PROGN"), TAIL(TAIL(lambda))); 
    if (new_env == NULLOBJ) 
 	new_env = env; 
    else 
 	append_env(new_env, env);
    PROTECT2(body, new_env);
    current_env = new_env;
    object_t result = eval(body, new_env, func);
    current_env = env;
    UNPROTECT;
    return result;
} 

/* 
 * Вычислить macro подстановку с заданными аргументами. 
 *  
 * @param macro - тело макроса (lambda (x) e1 e2 .. en) 
 * @param args - список значений аргументов (1) 
 * @param env окружение 
 * @param func окружение функций
 * @return вычисленное значение макроса 
 */ 
object_t macro_call(object_t macro, object_t args, object_t env, object_t func) 
{ 
    object_t new_env = make_env(SECOND(macro), args); 
    object_t body; 
    object_t eval_res = NULLOBJ;
    object_t eval_res2 = NULLOBJ;
    body = TAIL(TAIL(macro));
    if (new_env != NULLOBJ)
	append_env(new_env, env);
    PROTECT3(macro, eval_res, eval_res2);
    while (body != NULLOBJ) {
	current_env = new_env;
 	eval_res = eval(FIRST(body), new_env, func);
	current_env = env;
 	eval_res2 = eval(eval_res, env, func);
 	body = TAIL(body);
    }
    UNPROTECT;
    return eval_res2;
} 
    
/* 
 * Рекурсивно вычисляет список аргументов, создаёт новый список 
 * @param args список аргументов 
 * @param env окружение 
 * @param func окружение функций
 * @return возвращает список вычисленных аргументов 
 */ 
object_t eval_args(object_t args, object_t env, object_t func) 
{ 
    if (args == NULLOBJ) 
 	return NULLOBJ;
    if (TYPE(args) != PAIR)
	error("arguments are not list");
    object_t f;
    object_t arg;
    object_t vals = new_pair(NULLOBJ, NULLOBJ);
    object_t v = vals;
    PROTECT2(args, vals);
    while (args != NULLOBJ) {
	f = FIRST(args); 
	arg = eval(f, env, func);
	GET_PAIR(v)->left = arg;
	if (TAIL(args) == NULLOBJ)
	    GET_PAIR(v)->right = NULLOBJ;
	else
	    GET_PAIR(v)->right = new_pair(NULLOBJ, NULLOBJ);
	args = TAIL(args);
	v = TAIL(v);
    }
    UNPROTECT;
    return vals;
} 

/* 
 * Проверка на специальную форму 
 * @param s - имя функции 
 * @return 1 - специальная форма, а 0 - нет 
 */ 
int is_special_form(symbol_t *s) 
{ 
    return s == quote_sym || s == defun_sym || s == defmacro_sym
	|| s == setq_sym || s == backquote_sym || s == if_sym 
	|| s == throw_sym
	|| s == labels_sym || s == tagbody_sym || s == progn_sym
	|| s == go_sym || s == catch_sym || s == func_sym; 
} 

/* 
 * Возвращает объект, соответствующий значению символа 
 * @param obj - символьный объект 
 * @param env - окружение, в котором ищется значение переменной
 * @return объект, соответствующий obj 
 */ 
object_t eval_symbol(object_t obj, object_t env) 
{ 
    object_t res; 
    
    //    printf("eval_symbol: "); 
    //    PRINT(obj); 
    //    printf("env: "); 
    //    PRINT(env);   
    
    if (find_in_env(env, obj, &res)) 
	return res; 
    else { 
 	symbol_t *res_sym = check_symbol(GET_SYMBOL(obj)->str); 
	if (res_sym != NULL && res_sym->value != NOVALUE) 
            return res_sym->value; 
	else 
            error("Unknown SYMBOL: %s", GET_SYMBOL(obj)->str);
    } 
} 

/**
 * Вычисление выражения
 * Если выражение число или строка, массив, одиночный символ, возвращаем его же
 * Если выражение символ, то вычисляется значение переменной
 * Если выражение список, то вычисляем функцию:
 * первый элемент списка может быть лямбда-функцией или символом(именем функции)
 * если функция - особая форма или макрос, то аргументы функции не вычисляются
 * для обычной функции вычисляются аргументы.
 * функция может быть локальной(находится в окружении функций)
 * функция может быть задана пользователем
 * функция может быть встроенным примитивом
 * функция может быть макросом
 * @param obj входное выражение
 * @param env окружение переменных
 * @param func окружение функций
 * @return возвращает вычисленный объект
 */
object_t eval(object_t obj, object_t env, object_t func)
{
    object_t args;
    object_t res;
    /* printf("eval: "); PRINT(obj); */
    /* printf("env: "); PRINT(env); */
    if (need_grabage_collect())
	garbage_collect();
    if (obj == NULLOBJ)
        return NULLOBJ;
    if (obj == t)
        return t;
    else if (TYPE(obj) == NUMBER || TYPE(obj) == BIGNUMBER || TYPE(obj) == FLOAT || TYPE(obj) == STRING || TYPE(obj) == ARRAY || TYPE(obj) == CHAR)
	return obj;
    else if (TYPE(obj) == SYMBOL)
        return eval_symbol(obj, env);
    else if (TYPE(obj) == PAIR) {
        object_t first = FIRST(obj);
        if (TYPE(first) == PAIR) {
	    if (!is_lambda(first))
		error("Invalid lambda function");
	    else
		return eval_func(first, eval_args(TAIL(obj), env, func), env, func);
        } else if (TYPE(first) != SYMBOL) {
	    PRINT(first);
	    error("not function");
	}
        symbol_t *s = find_symbol(GET_SYMBOL(first)->str);
#ifdef DEBUG
    	debug_stack = new_pair(obj, debug_stack);
#endif
        if (is_special_form(s) || s->macro != NULLOBJ)
            args = TAIL(obj);
        else
            args = eval_args(TAIL(obj), env, func);

        object_t result;
        if (find_in_env(func, first, &res))
            result = eval_func(res, args, env, func);
        else if (s->lambda != NULLOBJ)
            result = eval_func(s->lambda, args, env, func);
        else if (s->func != NULL)
            result = s->func(args);
        else if (s->macro != NULLOBJ)
            result = macro_call(s->macro, args, env, func);
        else
            error("Unknown func: %s", s->str);
#ifdef DEBUG
    	debug_stack = TAIL(debug_stack);
#endif
        return result;
    } else
        error("Unknown object_type");
}

/* 
 * Установление значения символа в окружении 
 *  
 * @param env - окружение, где ищем символ 
 * @param sym - символ, для которого устан. значение 
 * @param val - устанавливаемое значение 
 */ 
void set_in_env(object_t env, object_t sym, object_t val) 
{
    if (env == NULLOBJ)
	error("ERROR: NULLOBJ as env in set_in_env"); 
    object_t pair = FIRST(env); 
    object_t var = FIRST(pair); 
    //    printf("set_in_env: var = ");PRINT(var);
    //    printf("sym = ");PRINT(sym);
    //    printf("val = ");PRINT(val);
    if (GET_SYMBOL(var) == GET_SYMBOL(sym)) 
	TAIL(pair) = val; 
    else 
	set_in_env(TAIL(env), sym, val); 
} 

/* 
 * Присвоение значения переменной 
 * @param param параметры (символьный объект, значение, окружение) 
 * @return возвращает значение переменной 
 */ 
object_t setq(object_t params) 
{ 
    object_t obj = NULLOBJ;
    object_t env = current_env;
    object_t func = func_env;
    if (params == NULLOBJ)
 	error("setq: params = NULLOBJ"); 
    PROTECT1(params);
    while (params != NULLOBJ) {
	symbol_t *sym = GET_SYMBOL(FIRST(params)); 
	object_t res; 
	int find_res = find_in_env(env, FIRST(params), &res); 
	if (!find_res) 
	    sym = find_symbol(GET_SYMBOL(FIRST(params))->str); 
	if (TAIL(params) == NULLOBJ)
	    error("setq: no value");
	obj = eval(SECOND(params), env, func);
	if (find_res) 
	    set_in_env(env, FIRST(params), obj); 
	else {
	    sym->value = obj;
	    set_global(sym);
	}
	if (TAIL(TAIL(params)) == NULLOBJ) {
	    UNPROTECT;
	    return obj;
	}
	params = TAIL(TAIL(params));
    }
    error("setq: out of vars");
} 

/* 
 * Выполняет функцию с аргументами 
 * @param param параметры (функция аргумент 1, аргумент 2 ...) 
 * @return возвращает результат выполнения функции 
 */ 
object_t funcall(object_t params) 
{ 
    if (params == NULLOBJ)
	error("funcall: no arguments"); 
    object_t func = FIRST(params); 
    if (func == NULLOBJ || TYPE(func) != FUNCTION)
	error("funcall: invalid func"); 
    object_t args = TAIL(params);
    function_t *f = GET_FUNCTION(func);
    if (f->func != NULL) 
 	return f->func(args);
    else 
	return eval_func(new_pair(NEW_SYMBOL("LAMBDA"), new_pair(f->args, f->body)), args, f->env, f->func_env); 
} 

/* 
 * Применияет функцию к списку аргументов
 * @param param параметры (функция <список аргументов>)
 * @return возвращает результат выполнения функции 
 */ 
object_t apply(object_t params) 
{ 
    if (params == NULLOBJ)
	error("apply: no arguments"); 
    object_t func = FIRST(params); 
    if (func == NULLOBJ || TYPE(func) != FUNCTION)
	error("apply: invalid func"); 
    object_t args = SECOND(params);
    function_t *f = GET_FUNCTION(func);
    if (f->func != NULL) 
 	return f->func(args);
    else 
	return eval_func(new_pair(NEW_SYMBOL("LAMBDA"), new_pair(f->args, f->body)), args, f->env, f->func_env); 
} 

/* 
 * Вычисляет свой аргумент 
 * @param args (выражение) 
 * @return возвращает список из аргументов 
 */ 
object_t lisp_eval(object_t args) 
{ 
    object_t res;
    PROTECT1(args);
    res =  eval(FIRST(args), current_env, func_env);
    UNPROTECT;
    return res;
} 

/* 
 * Функция прерывания вычислений и вывода сообщения об ошибке 
 * @param args (выражение) 
 */ 
object_t error_func(object_t args)
{
    printf("ERROR: ");
    PRINT(FIRST(args));
    last_protected = 0;
    longjmp(repl_buf, 1);
}

/** 
 * Функция вычисления форм с возможностью перехода по меткам
 *
 * @param params список форм и меток
 *
 * @return nil
 */
object_t tagbody(object_t params)  
{                                  
    object_t obj;
    object_t params2;
    object_t res;
    object_t tags = NULLOBJ; // Список функций метки
    object_t env;
    object_t func;
    params2 = params;
    while (params != NULLOBJ) {
        obj = FIRST(params);
	params = TAIL(params); 
        if (TYPE(obj) == SYMBOL)
            tags = new_pair(new_pair(obj, params), tags);
    }
    PROTECT1(tags);
#ifdef DEBUG
    object_t debug = debug_stack;
#endif
    tagbody_buffers[tb_index_buf].environment = current_env;
    tagbody_buffers[tb_index_buf].func_environment = func_env;
    tagbody_buffers[tb_index_buf].last_protected = last_protected;
    cur_label = NULLOBJ;
    if (setjmp(tagbody_buffers[tb_index_buf++].buffer) == 1) {
	if (tb_index_buf >= MAX_TAGBODY_SIZE)
	    error("tagbody: buffer haven't true length");
	if (!find_in_env(tags, cur_label, &res)) {
	    tb_index_buf--;
            error("tagbody: label %s not found", GET_SYMBOL(cur_label)->str);
	} else
            params2 = res;	    
    }
    env = current_env;
    func = func_env;
    while (params2 != NULLOBJ) {
        obj = FIRST(params2);
	params2 = TAIL(params2); 
        if (TYPE(obj) != SYMBOL)
            eval(obj, env, func);
#ifdef DEBUG
	debug_stack = debug;
#endif
    }
    tb_index_buf--;
    UNPROTECT;
    return nil;
}

/* 
 * Функция перехода 
 * @param args метка перехода
 */ 
object_t go(object_t args)
{
    if (args == NULLOBJ)
	error("go: no label");
    cur_label = FIRST(args);
    current_env = tagbody_buffers[tb_index_buf - 1].environment;
    func_env = tagbody_buffers[tb_index_buf - 1].func_environment;
    last_protected = tagbody_buffers[tb_index_buf - 1].last_protected;
    longjmp(tagbody_buffers[tb_index_buf - 1].buffer, 1);
}

/** 
 * Создает блок с лексическим именем, из которого можно выйти
 * с помощью return-from и вычисляет внутренние формы как progn
 *
 * @param list (<символ - имя блока> <форма_1> ... <форма_n>)
 *
 * @return 
 */
object_t catch(object_t list) 
{ 
    object_t obj; 
    if (list == NULLOBJ)
	error("catch: no arguments");
    object_t first_param = eval(FIRST(list), current_env, func_env);
    object_t rest_params = TAIL(list);
    if (setjmp(catch_buf) == 0)
        return progn(rest_params);
    else
	return cur_label;
}

/* 
 * Выходит из лексического блока, созданного catch
 * @param args (имя блока, результат)
 */ 
object_t throw(object_t args) 
{ 
    PROTECT1(args);
    cur_label = SECOND(args);
    UNPROTECT;
    longjmp(catch_buf, 1);
} 

/** 
 * Определяет локальные функции (могут быть рекурсивными) и выполняет формы
 * Список функций - (функция 1 ... функция n)
 * Функция - (имя параметры тело)
 * @param param (<список функций> <форма1> .. <форма n>)
 *
 * @return значение последней формы
 */
object_t labels(object_t param) 
{ 
    if (param == NULLOBJ)
        error("labels: no parameters");
    object_t forms = FIRST(param);
    object_t oldf = func_env;
    while (forms != NULLOBJ) {
        object_t first = FIRST(forms);
        if (TYPE(first) != PAIR || TYPE(SECOND(first)) != PAIR)
            error("labels: invalid function");
        func_env = new_pair(new_pair(FIRST(first), new_pair(NEW_SYMBOL("LAMBDA"), TAIL(first))), func_env);
	forms = TAIL(forms);
    }
    object_t res = progn(TAIL(param));
    func_env = oldf;
    return res;
}

/** 
 * Создаёт объект типа функция - лексическое замыкание. 
 * Один параметр - или lambda функция или символ - имя функции: локальная, глобальная или встроенная
 * @param param список парметров - один параметр - функция
 *
 * @return объект функция. 
 */
object_t function(object_t param) 
{
    if (param == NULLOBJ)
	error("function: no arguments");
    if (GET_PAIR(param)->right != NULLOBJ)
	error("function: more than one argument is given");

    object_t func = FIRST(param);
    if (is_lambda(func)) ;
    else if (TYPE(func) == SYMBOL) {
	symbol_t *s = find_symbol(GET_SYMBOL(func)->str);
	object_t res;
	if (find_in_env(func_env, func, &res)) // поиск локальной функции
	    func = res;
	else if (s->lambda != NULLOBJ) // если символ - пользовательская функция
	    func = s->lambda;
	else if (s->func != NULL) // если символ - примитив
	    return new_prim_function(s->func);
	else
	    error("function: unknown symbol %s", s->str);
    } else
	error("function: invalid param");
    pair_t* pair_right = GET_PAIR(GET_PAIR(func)->right);
    return new_function(pair_right->left, pair_right->right, current_env, func_env);   
}

/*  
 * инициализация примитивов  
 */ 
void init_eval() 
{ 
    register_func("ATOM", atom); 
    register_func("EQ", eq); 
    register_func("QUOTE", quote); 
    register_func("BACKQUOTE",backquote);
    register_func("IF", IF);
    register_func("DEFUN", defun); 
    register_func("DEFMACRO", defmacro); 
    register_func("PROGN", progn); 
    register_func("SETQ", setq); 
    register_func("FUNCALL", funcall); 
    register_func("APPLY", apply); 
    register_func("EVAL", lisp_eval);
    register_func("GC", print_gc_stat);
    register_func("DUMP-MEM", dump_mem);
    register_func("ERROR", error_func);
    register_func("TAGBODY", tagbody);
    register_func("GO", go);
    register_func("CATCH", catch); 
    register_func("THROW", throw);
    register_func("LABELS", labels);
    register_func("FUNCTION", function);
    t = NEW_SYMBOL("T"); 
    nil = NULLOBJ;
    bind_static(t);
    quote_sym = find_symbol("QUOTE"); 
    backquote_sym = find_symbol("BACKQUOTE"); 
    lambda_sym = find_symbol("LAMBDA");
    if_sym = find_symbol("IF");
    defun_sym = find_symbol("DEFUN"); 
    defmacro_sym = find_symbol("DEFMACRO"); 
    setq_sym = find_symbol("SETQ"); 
    t_sym = GET_SYMBOL(t);
    t_sym->value = new_number(1);
    nil_sym = find_symbol("NIL"); 
    nil_sym->value = nil;
    bind_static(NEW_OBJECT(SYMBOL, nil_sym));
    rest_sym = find_symbol("&REST");
    bind_static(NEW_OBJECT(SYMBOL, rest_sym));
    bind_static(NEW_OBJECT(SYMBOL, lambda_sym));
    tagbody_sym = find_symbol("TAGBODY");
    go_sym = find_symbol("GO");
    catch_sym = find_symbol("CATCH");
    labels_sym = find_symbol("LABELS"); 
    progn_sym = find_symbol("PROGN");
    func_sym = find_symbol("FUNCTION");
} 
