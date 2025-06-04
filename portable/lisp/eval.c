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

/** 
 * Определение длины списка
 *
 * @param args список
 *
 * @return длина
 */
int list_length(object_t args)
{
    int c = 0;
    while (args != NULLOBJ) {
	args = TAIL(args);
	c++;
    }
    return c;
}

/** 
 * Проверка нахождения символа в списке
 *
 * @param list список
 * @param symbol символ
 *
 * @return позицию первого вхождения сивола в списке или -1 если символ не найден
 */
int list_contains(object_t list, symbol_t* symbol) 
{
    int c = 0;
    while (list != NULLOBJ) {
        object_t obj = FIRST(list);
        if (TYPE(obj) == SYMBOL && GET_SYMBOL(obj) == symbol)
            return c;
        list = TAIL(list);
        c++;
    }
    return -1;
}

// (eq 'a 'a) -> T 
// (eq 'a 'b) -> () 
/*  
 * возвращает t если значения двух элементов это один и тот же символ а иначе возвращает nil 
 * 
 * @param p1, p2 - два элемента
 * 
 * @return t или nil 
 */ 
object_t eq(object_t p1, object_t p2) 
{ 
    if (p1 == p2)
	return t; 
    else 
	return nil; 
} 

/*  
 * если аргумент атом, то возвращает T, иначе NIL 
 * 
 * @param obj - аргумент 
 * 
 * @return t или nil 
 */ 
object_t atom(object_t obj1) 
{      
    if (obj1 == NULLOBJ || TYPE(obj1) != PAIR) 
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
 * @param obj1 - 1 аргумент 
 * 
 * @return аргумент 
 */
object_t quote(object_t obj1) 
{
    return obj1;
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
 * @param list - 1 парамет 
 * 
 * @return аргумент 
 */ 
object_t backquote(object_t list) 
{
    object_t res = NULLOBJ;
    object_t first = NULLOBJ;
    object_t l = NULLOBJ;
    if (list == NULLOBJ) 
 	return NULLOBJ;
    PROTECT3(list, res, first);
    object_t env = current_env;
    object_t func = func_env;
    
    if (TYPE(list) == BIGNUMBER
	|| TYPE(list) == FLOAT
	|| TYPE(list) == CHAR
	|| TYPE(list) == NUMBER
	|| TYPE(list) == SYMBOL
	|| TYPE(list) == STRING)
	res = make_copy(list);
    else if (TYPE(list) == ARRAY) { 
 	array_t *arr = new_empty_array(GET_ARRAY(list)->length); 
 	res = NEW_OBJECT(ARRAY, arr); 
 	int l = GET_ARRAY(list)->length; 
 	for (int i = 0; i < l; i++)
 	    arr->data[i] = backquote(GET_ARRAY(list)->data[i]);
    } else if (TYPE(list) == PAIR) {
 	object_t el = FIRST(list);	
	if (TYPE(el) == SYMBOL && !strcmp(GET_SYMBOL(el)->str, "BACKQUOTE"))
	    res = list;	
	else if (TYPE(el) == SYMBOL && !strcmp(GET_SYMBOL(el)->str, "COMMA") && TAIL(list) != NULLOBJ) 
	    res = eval(SECOND(list), env, func);
	else {
	    first = backquote(el);
	    if (first != NULLOBJ && TYPE(first) == PAIR) {  // first = (COMMA-AT B) 
		object_t comma_at = FIRST(first);
		if (comma_at != NULLOBJ && TYPE(comma_at) == SYMBOL && !strcmp(GET_SYMBOL(comma_at)->str, "COMMA-AT") && TAIL(first) != NULLOBJ) {
		    l = eval(SECOND(first), env, func);
		    if (l == NULLOBJ) {
			res = backquote(TAIL(list));
			UNPROTECT;
			return res;
		    } else if (TYPE(l) != PAIR)
			error("COMMA-AT: not list");
		    else {
			res = backquote(l); 
			append_env(res, backquote(TAIL(list))); 
			UNPROTECT;
			return res;
		    }
		}
	    }
	    object_t tail = backquote(TAIL(list)); 
	    res = new_pair(first, tail);
	}
    }
    UNPROTECT;
    return res;
}


/* 
 * (if test True False)
 * Обработка условия. Истинное условие - не nil
 * Возвращаем список объектов из выражения, оцененных  eval 
 * @param obj1 - условие
 * @param obj2 - выражение по истине
 * @param obj3 - выражение по лжи
 * @return возвращает значение соответствующего выражения
 */ 
object_t IF(object_t obj1, object_t obj2, object_t obj3)
{
    object_t res = NULLOBJ;
    object_t env = current_env;
    object_t func = func_env;
    PROTECT3(obj1, obj2, obj3);
    if (eval(obj1, env, func) != nil)
	res = eval(obj2, env, func);
    else
	res = eval(obj3, env, func);
    UNPROTECT;
    return res;
}

/*  
 * Создаёт новую функцию 
 * 
 * @param name1 - имя_функции
 * @param list - список аргументов
 * @param body - тело_функции
 *
 * @return символ имени новой функции 
 */ 
object_t defun(object_t name1, object_t list, object_t body) 
{
    symbol_t *name = find_symbol(GET_SYMBOL(name1)->str); 
    name->lambda = new_pair(NEW_SYMBOL("LAMBDA"), new_pair(list, body));
    int rest = list_contains(list, rest_sym);
    if (rest == -1) {
        name->nary = 0;
        name->count = list_length(list);
    }
    else {
        name->nary = 1;
        name->count = rest;
    }
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
object_t defmacro(object_t name1, object_t list, object_t body) 
{ 
    symbol_t *name = find_symbol(GET_SYMBOL(name1)->str); 
    name->macro = new_pair(NEW_SYMBOL("LAMBDA"), TAIL(list));
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
    object_t body  = TAIL(TAIL(macro)); 
    object_t eval_res = NULLOBJ;
    object_t eval_res2 = NULLOBJ;
    
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
 * Вызов примитива или встроенной формы с переменным числом аргументов
 *
 * @param s символ формы
 * @param args список аргументов
 * @param args_count фактическое число аргументов
 * @param count фиксированное число аргументов для nary
 *
 * @return результат вычислений
 */
object_t call_form(func0_t f, object_t args, int nary, int args_count, int count)
{
    if (nary == 0)
	switch (args_count) {
	case 0:
	    return f();
	case 1:
	    return (func1_t)f(FIRST(args));
	case 2:
	    return (func2_t)f(FIRST(args), SECOND(args));
	case 3:
	    return (func3_t)f(FIRST(args), SECOND(args), THIRD(args));
	case 4:
	    return (func4_t)f(FIRST(args), SECOND(args), THIRD(args), TAIL(args));
	default:
	    error("call form with %d arguments", args_count);	
	}
    else
	switch (count) {
	case 0:
	    return (func1_t)f(args);
	case 1:
	    return (func2_t)f(FIRST(args), TAIL(args));
	case 2:
	    return (func3_t)f(FIRST(args), SECOND(args), TAIL(TAIL(args)));
	case 3:
	    return (func4_t)f(FIRST(args), SECOND(args), THIRD(args), TAIL(TAIL(TAIL(args))));
	case 4:
	    return (func5_t)f(FIRST(args), SECOND(args), THIRD(args), THIRD(TAIL(args)), TAIL(TAIL(TAIL(TAIL(args)))));
	default:
	    error("call nary form with %d arguments", count);
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
	int args_count = list_length(TAIL(obj));
	if (s->nary == 0) {
	    if (s->count != args_count)
		error("%s: invalid arguments count", s->str);
	} else {
	    if (s->count > args_count)
		error("%s: invalid arguments count", s->str);
	}	    
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
	    result = call_form(s->func, args, s->nary, args_count, s->count);
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
object_t funcall(object_t fun, object_t args) 
{ 
    function_t *f = GET_FUNCTION(fun);
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
    res =  eval(args, current_env, func_env);
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
    PRINT(args);
#ifndef VM    
    last_protected = 0;
    longjmp(repl_buf, 1);
#endif    
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
    
    cur_label = args;
    current_env = tagbody_buffers[tb_index_buf - 1].environment;
    func_env = tagbody_buffers[tb_index_buf - 1].func_environment;
    last_protected = tagbody_buffers[tb_index_buf - 1].last_protected;
    longjmp(tagbody_buffers[tb_index_buf - 1].buffer, 1);
}

/** 
 * Создает блок с динамическим именем, из которого можно выйти
 * с помощью throw и вычисляет внутренние формы как progn
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
 * Выходит из динамического блока, созданного catch
 * @param args (имя блока, результат)
 */ 
object_t throw(object_t tag, object_t res) 
{ 
    PROTECT1(res);
    cur_label = res;
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
object_t labels(object_t param, object_t forms) 
{ 
    
    
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
 * @param func - один параметр - функция
 *
 * @return объект функция. 
 */
object_t function(object_t func) 
{
    if (is_lambda(func)) ;
    else if (TYPE(func) == SYMBOL) {
	symbol_t *s = find_symbol(GET_SYMBOL(func)->str);
	object_t res;
	if (find_in_env(func_env, func, &res)) // поиск локальной функции
	    func = res;
	else if (s->lambda != NULLOBJ) // если символ - пользовательская функция
	    func = s->lambda;
	else if (s->func != NULL) // если символ - примитив
	    return new_prim_function(s->func, s->nary, s->count);
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
    register_func("ATOM", atom, 0, 1); 
    register_func("EQ", eq, 0, 2); 
    register_func("QUOTE", quote, 0, 1); 
    register_func("BACKQUOTE",backquote, 0, 1);
    register_func("IF", IF, 0, 3);
    register_func("DEFUN", defun, 1, 2); 
    register_func("DEFMACRO", defmacro, 1, 2); 
    register_func("PROGN", progn, 1, 0); 
    register_func("SETQ", setq, 1, 0); 
    register_func("FUNCALL", funcall, 1, 1); 
    register_func("APPLY", apply, 0, 2); 
    register_func("EVAL", lisp_eval, 0, 1);
    register_func("GC", print_gc_stat, 0, 0);
    register_func("DUMP-MEM", dump_mem, 0, 0);
    register_func("ERROR", error_func, 0, 1);
    register_func("TAGBODY", tagbody, 1, 0);
    register_func("GO", go, 0, 1);
    register_func("CATCH", catch, 1, 0); 
    register_func("THROW", throw, 0, 2);
    register_func("LABELS", labels, 1, 1);
    register_func("FUNCTION", function, 0, 1);
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
