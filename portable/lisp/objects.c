#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include "objects.h"
#include "symbols.h"
#include "alloc.h"
#include "cont.h"
#include "bind.h"
#include "eval.h"

/// Индекс последнего большого числа
int last_bignumber = 0;
/// Хранилище больших чисел
bignumber_t *bignumbers;
/// Список свободных больших чисел
bignumber_t *free_bignumbers = NULL;

/// Индекс последнего вещественного числа
int last_float = 0;
/// Хранилище вещественных чисел
float_t *floats;
/// Список свободных вещественных чисел
float_t *free_floats = NULL;

/// Индекс последней пары
int last_pair = 0;
/// Массив или хранилище пар
pair_t *pairs;
/// Список свободных пар
pair_t *free_pairs = NULL;

/// Индекс последнего символа
int last_symbol = 0;
/// Хранилище символов
symbol_t *symbols;
/// Список свободных символов
symbol_t *free_symbols = NULL;

/// Индекс последней строки
int last_string = 0;
/// Хранилище строк
string_t *strings;
/// Список свободных строк
string_t *free_strings = NULL;

/// Индекс последнего массива
int last_array = 0;
/// Хранилище массивов
array_t *arrays;
/// Список свободных массивов
array_t *free_arrays = NULL;

/// Хранилище функций
function_t *functions;
/// Список свободных функций
function_t *free_functions = NULL;
/// Индекс последней функции
int last_function = 0;

/// Индекс последнего продолжения
int last_continuation = 0;
/// Хранилище продолжений
continuation_t *continuations;
/// Список свободных продолжений
continuation_t *free_continuations = NULL;

///Количество используемых больших чисел
int total_bignumbers = 0;
///Количество используемых вещественных чисел
int total_floats = 0;
///Количество используемых пар
int total_pairs = 0;
///Количество используемых строк
int total_strings = 0;
///Количество используемых массивов
int total_arrays = 0;
///Количество используемых продолжений
int total_continuations = 0;
///Количество используемых функций
int total_functions = 0;
///Количество используемых символов
int total_symbols = 0;
///Количество созданных пар с момента последней сборки мусора
int allocated_pairs = 0;

/// текущее окружение
extern object_t current_env;
/// окружение функции
extern object_t func_env;

/// точка вычисления меток tagbody
continuation_t tagbody_buffers[MAX_TAGBODY_SIZE];
///текущий индекс-буфер для tagbody
int tb_index_buf = 0;
/// точка вычисления меток catch
catch_t catch_buffers[MAX_CATCH_SIZE];
///текущий индекс-буфер для catch
int ct_index_buf;

/**
 * Инициализация объектов
 */
void init_objects()
{
    symbols = (symbol_t *)alloc_region(MAX_SYMBOLS * sizeof(symbol_t));
    bignumbers = (bignumber_t *)alloc_region(MAX_NUMBERS * sizeof(bignumber_t));
    floats = (float_t *)alloc_region(MAX_FLOATS * sizeof(float_t));
    pairs = (pair_t *)alloc_region(MAX_PAIRS * sizeof(pair_t));
    strings = (string_t *)alloc_region(MAX_STRINGS * sizeof(string_t));
    arrays = (array_t *)alloc_region(MAX_ARRAYS * sizeof(array_t));
    continuations = (continuation_t *)alloc_region(MAX_CONTINUATIONS * sizeof(continuation_t));
    functions = (function_t *)alloc_region(MAX_FUNCTIONS * sizeof(function_t));
}

/**
 * Создание нового объекта большого целого числа
 *
 * @param num целое число
 *
 * @return указатель на объект числа
/*  */
object_t new_bignumber(int num)
{
    bignumber_t *number;
    if (last_bignumber == MAX_NUMBERS)
    {
	if (free_bignumbers == NULL)
	    error("Error: out of memory: numbers");
	number = free_bignumbers;
	free_bignumbers = free_bignumbers -> next;
    } else
	number = &bignumbers[last_bignumber++];
    number->next = NULL;
    number->free = 0;
    number->value = num;
    total_bignumbers++;
    return NEW_OBJECT(BIGNUMBER, number);
}

/**
 * Освобождение памяти для большого числа
 *
 * @param o  объект для освобождения
 */
void free_bignumber(bignumber_t *o)
{
    if (o == NULL)
    	error("free_bignumber: null pointer: obj");
    if (o->free)
	return;
    o->next = free_bignumbers;
    free_bignumbers = o;
    o->free = 1;
    total_bignumbers--;
}

/**
 * Создание нового объекта продолжения
 *
 * @param buf буфер jmp_buf
 *
 * @return указатель на объект продолжения
/*  */
object_t new_continuation(jmp_buf buf) {
	continuation_t *continuation;
	if (last_continuation == MAX_CONTINUATIONS) {
		if (free_continuations == NULL)
			error("Error: out of memory: continuations");
		continuation = free_continuations;
		free_continuations = free_continuations->next;
	} else
		continuation = &continuations[last_continuation++];
	// jmp_buf buf2;
	memcpy(continuation->buffer, buf, sizeof(jmp_buf));
	// continuation->buffer = buf2;
	continuation->environment = current_env;
	continuation->func_environment = func_env;
	continuation->last_protected = last_protected;
	total_continuations++;
	return NEW_OBJECT(CONTINUATION, continuation);
}

/**
 * Создание нового объекта функции
 *
 * @param args список аргументов функции
 * @param body тело функции
 * @param env окружение для переменных
 * @param func_env окружение для функций
 *
 * @return указатель на объект функции
/*  */
object_t new_function(object_t args, object_t body, object_t env, object_t func_env)
{
    function_t *func;
    if (last_function == MAX_FUNCTIONS)
    {
	if (free_functions == NULL)
	    error("Error: out of memory: funcs");
	func = free_functions;
	free_functions = free_functions->next;
    } else
	func = &functions[last_function++];
    func->next = NULL;
    func->free = 0;
    func->args = args;
    func->body = body;
    func->env = env;
    func->func_env = func_env;
    func->func = NULL;
    total_functions++;
    return NEW_OBJECT(FUNCTION, func);
}

/**
 * Создание нового объекта встроенной функции
 *
 * @param func имя примитива
 * @return указатель на объект функции
 */
object_t new_prim_function(func0_t f, int nary, int count)
{
    function_t *func;
    if (last_function == MAX_FUNCTIONS)
    {
	if (free_functions == NULL)
	    error("Error: out of memory: funcs");
	func = free_functions;
	free_functions = free_functions->next;
    } else
    func = &functions[last_function++];
    func->next = NULL;
    func->free = 0;
    func->args = NULLOBJ;
    func->body = NULLOBJ;
    func->func = f;
    func->nary = nary;
    func->count = count;
    total_functions++;
    return NEW_OBJECT(FUNCTION, func);
}

/**
 * Освобождение памяти для функции
 *
 * @param o  объект для освобождения
 */
void free_function(function_t *f)
{
    if (f == NULL)
    	error("free_function: null pointer: obj");
    if (f->free)
	return;
    f->next = free_functions;
    free_functions = f;
    f->free = 1;
    total_functions--;
}

void free_continuation(continuation_t *c) {
	if (c == NULL)
		error("free_continuation: null pointer: obj");
	if (c->free) return;
	c->next = free_continuations;
	free_continuations = c;
	c->free = 1;
	total_continuations--;
}

/**
 * Создание нового объекта вещественного числа
 *
 * @param  num вещественное число
 *
 * @return указатель на объект числа
/*  */
object_t new_float(float num)
{
    float_t *number;
    if (last_float == MAX_FLOATS)
    {
	if (free_floats == NULL)
	    error("Error: out of memory: floats");
	number = free_floats;
	free_floats = free_floats -> next;
    } else
	number = &floats[last_float++];
    number->next = NULL;
    number->free = 0;
    number->value = num;
    total_floats++;
    return NEW_OBJECT(FLOAT, number);
}

/**
 * Освобождение памяти для вещественного числа
 *
 * @param obj объект
 */
void free_float(float_t *f)
{
    if (f == NULL)
    	error("free_float: null pointer: obj");
    if (f->free)
	return;
    f->next = free_floats;
    free_floats = f;
    f->free = 1;
    total_floats--;
}

/**
 * Создание нового объекта целого числа
 * -1 11111111111111111111111...
 * GET_ADDR -> 00001111111111111111...
 * -MAX_NUM 11111000000000000000...
 * GET_ADDR -> 0000100000000000...
 *
 * @param num целое число
 *
 * @return указатель на объект числа
 */
object_t new_number(int num)
{
    unsigned int mask = (1 << ADDR_BITS - 1) - 1;
    int min_val = ((1 << (TYPE_BITS + 2)) - 1) << ADDR_BITS - 1;
    if (num >= 0 && num <= mask)
	return NEW_OBJECT(NUMBER, num << MARK_BIT);
    else if (num >= min_val && num < 0)
	return NEW_OBJECT(NUMBER, num << MARK_BIT);
    else
	return new_bignumber(num);
}

/**
 * Возвращает значение маленького числа из объекта
 * @param obj объект
 *
 * @return число
 */
int get_value(object_t obj)
{
    if (TYPE(obj) == BIGNUMBER)
	return GET_BIGNUMBER(obj)->value;
    if ((int)obj < 0)
	return (obj >> MARK_BIT) | (((1 << TYPE_BITS + 1) - 1) << ADDR_BITS);
    else
	return obj >> MARK_BIT;
}


/**
 * Создание нового объекта пары
 *
 * @param left левый объект
 * @param right правый объект
 *
 * @return указатель на объект пары
/*  */
object_t new_pair(object_t left, object_t right)
{
    pair_t *pair;
    if (last_pair == MAX_PAIRS) {
 	if (free_pairs == NULL)
 	    error("Error: out of memory: pairs");
 	pair = free_pairs;
 	free_pairs = free_pairs->next;
    } else
 	pair = &pairs[last_pair++];
    pair->next = NULL;
    pair->free = 0;
    pair->left = left;
    pair->right = right;
    total_pairs++;
    allocated_pairs++;
    return NEW_OBJECT(PAIR, pair);
}

/**
 * Освобождение памяти для пары
 *
 * @param p объект для освобождения
 */
void free_pair(pair_t *p)
{
    if (p == NULL)
    	error("free_pair: null pointer: obj");
    //printf("free pair: %d %x \n ", p - pairs, p->next);
    if (p->free)
	return;
    p->next = free_pairs;
    free_pairs = p;
    p->free = 1;
    total_pairs--;
}

/**
 * Создание нового объекта символа
 *
 * @param str имя символа
 *
 * @return указатель на объект символа
 */
symbol_t *new_symbol(char *str)
{
    symbol_t *symbol;
    if (*str == 0)
	return NULL;
    if (last_symbol == MAX_SYMBOLS) {
	if (free_symbols == NULL)
	    error("Error: out of memory: symbols");
	symbol = free_symbols;
	free_symbols = free_symbols->next;
    } else
	symbol = &symbols[last_symbol++];
    strcpy(symbol->str, str);
    symbol->next = NULL;
    symbol->value = NOVALUE;
    symbol->func = NULL;
    symbol->free = 0;
    symbol->lambda = NULLOBJ;
    symbol->macro = NULLOBJ;
    return symbol;
}

/**
 * Освобождение памяти для символа
 *
 * @param s объект для освобождения
 */
void free_symbol(symbol_t *s)
{
    if (s == NULL)
	error("free_symbol: null pointer: obj");
    if (s->free)
	return;
    hash_remove(s);
    s->free = 1;
    s->next = free_symbols;
    free_symbols = s;
    total_symbols--;
}

/**
 * Создание нового объекта строки
 *
 * @param str - строка
 *
 * @return указатель на объект строки
 */
string_t *new_string(char *str)
{
    string_t *string;
    if (last_string == MAX_STRINGS) {
	if (free_strings == NULL)
	    error("Error: out of memory: strings");
	string = free_strings;
	free_strings = free_strings->next;
    } else
	string = &strings[last_string++];
    string->length = strlen(str);
    string->data = alloc_region(string->length + 1);
    strcpy(string->data, str);
    string->next = NULL;
    string->free = 0;
    total_strings++;
    return string;
}

/**
 * Освобождение памяти для строки
 *
 * @param s объект для освобождения
 */
void free_string(string_t *s)
{
    if (s == NULL) {
    	error("free_string: null pointer");
    	return;
    }
    if (s->free)
	return;
    s->next = free_strings;
    free_strings = s;
    s->free = 1;
    free_region(s->data);
    total_strings--;
}

/** Создание нового объекта массива
 *
 * @param list - список
 *
 * @return указатель на объект

 */
array_t *new_array(object_t list)
{
    pair_t *pairs;
    array_t *array;
    if (last_array == MAX_ARRAYS) {
	if (free_arrays == NULL)
	    error("Error: out of memory: arrays");
	array = free_arrays;
	free_arrays = free_arrays->next;
    } else
	array = &arrays[last_array++];
    array->length = 0;
    array->next = NULL;
    array->free = 0;
    object_t a = list;
    while (a != NULLOBJ) {
	array->length++;
	a = TAIL(a);
    }
    array->data = alloc_region(array->length * sizeof(object_t));
    object_t *d = array->data;
    a = list;
    while (a != NULLOBJ) {
	*d++ = FIRST(a);
	a = TAIL(a);
    }
    total_arrays++;
    return array;
}

/**
/*  * Создание нового объекта пустого массива
 *
 * @param length - длина массива
 *
 * @return указатель на объект массива
 */
array_t *new_empty_array(int length)
{
    array_t *array;
    if (length < 0)
	error("make-array: negative length");
    if (last_array == MAX_ARRAYS) {
	if (free_arrays == NULL)
	    error("Error: out of memory: arrays");
	array = free_arrays;
	free_arrays = free_arrays->next;
    } else
	array = &arrays[last_array++];
    array->length = length;
    array->next = NULL;
    array->free = 0;
    array->data = alloc_region(array->length * sizeof(object_t));
    object_t *d = array->data;
    for (int i = 0; i < length; i++)
	*d++ = NULLOBJ;
    total_arrays++;
    return array;
}

/**
 * Освобождение памяти для массива
 *
 * @param s объект для освобождения
 */
void free_array(array_t *a)
{
    if (a == NULL) {
    	error("free_array: null pointer");
    	return;
    }
    if (a->free)
	return;
    a->next = free_arrays;
    free_arrays = a;
    a->free = 1;
    total_arrays--;
    free_region(a->data);
    //    printf("free array %d free = %x\n", a - arrays, free_arrays);
}
/**
 * Пометить объект как используемый
 * Для маленьких чисел - не хранится
 * Для больших чисел - старший бит в free
 * Для чисел с плавающей точкой - старший бит в free
 * Для функций - старший бит в free
 * Для символов - старший бит в hash_index
 * Для пар - markbit в left
 * Для строк - старший бит в length
 * Для массивов - старший бит в length
 * @param obj - помечаемый объект
 */
void mark_object(object_t obj)
{
    if (obj == NULLOBJ || obj == NOVALUE || obj == NULLOBJ + (1 << TYPE_BITS))
	return;
    int mask = 1 << 31;
    if (TYPE(obj) == PAIR) {
	pair_t *p = GET_PAIR(obj);
	if (GET_MARK(p->left) == 1)
	    return;
	SET_MARK(p->left);
	mark_object(p->left);
	mark_object(p->right);
    } else if (TYPE(obj) == SYMBOL) {
       symbol_t *s = GET_SYMBOL(obj);
       s->hash_index |= mask;
       mark_object(s->value);
       mark_object(s->lambda);
       mark_object(s->macro);
    } else if (TYPE(obj) == BIGNUMBER) {
	bignumber_t *b = GET_BIGNUMBER(obj);
	if (((b->free) & mask) != 0)
	    return;
	b->free |= mask;
    } else if (TYPE(obj) == FLOAT) {
	float_t *f = GET_FLOAT(obj);
	if (((f->free) & mask) != 0)
	    return;
	f->free |= mask;
    } else if (TYPE(obj) == FUNCTION) {
	function_t *f = GET_FUNCTION(obj);
        if ((f->free & mask) != 0)
	    return;
	mark_object(f->env);
	mark_object(f->func_env);
	if (f->func == NULL) {
	    mark_object(f->body);
	    mark_object(f->args);
	}
	f->free |= mask;
    } else if (TYPE(obj) == STRING) {
	string_t *s = GET_STRING(obj);
        if (((s->length) & mask) != 0)
	    return;
	s->length |= mask;
    } else if (TYPE(obj) == ARRAY) {
	array_t *a = GET_ARRAY(obj);
        if (((a->length) & mask) != 0)
	    return;
	for (int i = 0; i < a->length; i++)
	    mark_object(a->data[i]);
	a->length |= mask;
    }
}

/**
 * Освобождаем все непомеченные объекты, снимаем пометки
 */
void sweep()
{
    int mask = 1 << 31;
    bignumber_t *big_num = bignumbers;
    for (int i = 0; i < last_bignumber; i++, big_num++) {
        if ((big_num->free & mask) == 0)
	    free_bignumber(big_num);
	else
	    big_num->free &= ~mask;
    }
    float_t *flt = floats;
    for (int i = 0; i < last_float; i++, flt++) {
	if ((flt->free & mask) == 0)
	    free_float(flt);
	else flt->free &= ~mask;
    }
    function_t *func = functions;
    for (int i = 0; i < last_function; i++, func++) {
	if ((func->free & mask) == 0)
	    free_function(func);
	else func->free &= ~mask;
    }
    pair_t *pair = pairs;
    for (int i = 0; i < last_pair; i++, pair++) {
	if (GET_MARK(pair->left) != 1)
	    free_pair(pair);
	else CLEAR_MARK(pair->left);
    }
    string_t *str = strings;
    for (int i = 0; i < last_string; i++, str++) {
	if ((str->length & mask) == 0)
	    free_string(str);
	else str->length &= ~mask;
    }
    array_t *arr = arrays;
    for (int i = 0; i < last_array; i++, arr++) {
	if ((arr->length & mask) == 0)
	    free_array(arr);
	else arr->length &= ~mask;
    }
    symbol_t *symb = symbols;
    for (int i = 0; i < last_symbol; i++, symb++) {
       if ((symb->hash_index & mask) == 0)
           free_symbol(symb);
       else
           symb->hash_index &= ~mask;
    }
}

object_t dump_mem(object_t args)
{
    printf("dump_mem:\nbignumbers: ");
    for (int i = 0; i < last_bignumber; i++) {
        bignumber_t *big_num = &bignumbers[i];
    	printf("%d %d,", big_num->free, big_num->value);
    }
    printf("\nfloats: ");
    for (int i = 0; i < last_float; i++) {
    	float_t *flt = &floats[i];
    	printf("%d %f,", flt->free, flt->value);
    }
    printf("\nfunctions: ");
    for (int i = 0; i < last_function; i++) {
    	function_t *func = &functions[i];
    	printf("free: %d\n", func->free);
    	printf("args: "); PRINT(func->args);
    	printf("body: "); PRINT(func->body);
    	printf("env: "); PRINT(func->env);
    	printf("func_env: "); PRINT(func->func_env);
    	printf("----------\n");
    }
    printf("pairs: ");
    for (int i = 0; i < last_pair; i++) {
        pair_t *pair = &pairs[i];
    	printf("free: %d\n", pair->free);
    	printf("left: "); PRINT(pair->left);
    	printf("right: "); PRINT(pair->right);
    	printf("----------\n");
    }
    printf("strings: ");
    for (int i = 0; i < last_string; i++) {
    	string_t *str = &strings[i];
    	printf("%d %d %s\n,", str->free, str->length, str->data);
    }
    printf("\narrays: ");
    for (int i = 0; i < last_array; i++) {
    	array_t *arr = &arrays[i];
	printf("num: %d\n", i);
    	printf("free: %d\n", arr->free);
    	printf("length: %d\n", arr->length);
    	printf("data: ");
	if (!arr->free)
	    for (int j = 0; j < arr->length; j++) {
		print_obj(arr->data[j]);
		printf(" ");
	    }
    	printf("-----------\n");
    }
    printf("symbols: ");
    symbol_t *symb = symbols;
    for (int i = 0; i < last_symbol; i++, symb++)
    	printf("%d %s %x\n", symb->free, symb->str, (int)symb);
    printf("\n");
    return NULLOBJ;
}

/**
 * Сборка мусора
 */
void garbage_collect()
{
    /* printf("gc\n"); */
    object_t *cur = static_bind;
    for (int i = 0; i < last_static; i++, cur++)
	mark_object(*cur);
#ifdef DEBUG
    mark_object(debug_stack);
#endif
    mark_object(current_env);
    mark_object(func_env);
    for (int i = 0; i < tb_index_buf; i++) {
    	mark_object(tagbody_buffers[i].environment);
    	mark_object(tagbody_buffers[i].func_environment);
    }
    for (int i = ct_index_buf; i < MAX_CATCH_SIZE; i++) {
        mark_object(catch_buffers[i].tag);
        mark_object(catch_buffers[i].buff.environment);
        mark_object(catch_buffers[i].buff.func_environment);
    }
    object_t **temp = protected;
    for (int i = 0; i < last_protected; i++, temp++)
	mark_object(**temp);
    sweep();
    allocated_pairs = 0;
}

int print_counter = 0;

/**
 * Печать списка пар (5 . 7)
 */
void print_list(object_t obj)
{
    if (obj == NULLOBJ)
	return;
    pair_t *list = (pair_t*)(GET_ADDR(obj));
    if(list->print_counter == print_counter){
	printf("...");
	return;
    }
    list->print_counter = print_counter; //помечаем на текущем
    print_obj(list->left);
    if (list->right == NULLOBJ)
	return;
    if (TYPE(list->right) != PAIR) {
	printf(" . ");
	print_obj(list->right);
    } else {
	printf(" ");
	print_list(list->right);
    }
}

/**
 * Печать массива
 */
void print_array(object_t obj)
{
    array_t *a = (array_t*)(GET_ADDR(obj));
    int len;
    len = a->length;
    for (int i = 0; i < len; i++) {
    	print_obj(a->data[i]);
    	if (i < a->length - 1)
    	    printf(" ");
    }
}

/**
 * Печать объекта
 */
void print_obj(object_t obj)
{
    if (obj == NULLOBJ)
 	printf("()");
    else if (TYPE(obj) == NUMBER)
 	printf("%d", get_value(obj));
    else if (TYPE(obj) == BIGNUMBER)
	printf("%d", GET_BIGNUMBER(obj)->value);
    else if (TYPE(obj) == FUNCTION) {
	function_t *f = GET_FUNCTION(obj);
	printf("(Function ");
	if (f->func == NULL) {
	    print_obj(f->args);
	    printf(" ");
	    print_obj(f->body);
	    printf(" ");
	} else
	    printf("primitive %d %d", f->nary, f->count);
	printf(")");
    }
    else if (TYPE(obj) == FLOAT)
	printf("%f", (GET_FLOAT(obj))->value);
    else if (TYPE(obj) == STRING)
 	printf("\"%s\"", GET_STRING(obj)->data);
    else if (TYPE(obj) == SYMBOL)
 	printf("%s", ((symbol_t *)GET_ADDR(obj))->str);
    else if (TYPE(obj) == CHAR)
        printf("#\\%c", (int)GET_CHAR(obj));
    else if (TYPE(obj) == PAIR) {
 	    printf("(");
 	    print_list(obj);
 	    printf(")");
    }
    else if (TYPE(obj) == ARRAY) {
 	    printf("#(");
 	    print_array(obj);
 	    printf(")");
    }
}

/**
 * Печать статистики сборки мусора и памяти
 */
object_t print_gc_stat(object_t o)
{
    printf("bignumbers: %d(%d) of %d\n", last_bignumber, total_bignumbers, MAX_NUMBERS);
    printf("floats: %d(%d) of %d\n", last_float, total_floats, MAX_FLOATS);
    printf("pairs: %d(%d) of %d\n", last_pair, total_pairs, MAX_PAIRS);
    printf("symbols: %d(%d) of %d\n", last_symbol, total_symbols, MAX_SYMBOLS);
    printf("strings: %d(%d) of %d\n", last_string, total_strings, MAX_STRINGS);
    printf("arrays: %d(%d) of %d\n", last_array, total_arrays, MAX_ARRAYS);
    printf("functions: %d(%d) of %d\n", last_function, total_functions, MAX_FUNCTIONS);
    printf("used mem: %d of %d\n", regions_mem(), MAX_REGION_SIZE);
    return NULLOBJ;
}

/*
 * Проверка на необходимость в сборке мусора
 *
 * @return 1 - сборка мусора требуется, 0 - нет
 */
int need_grabage_collect()
{
    return allocated_pairs > GC_THRESHOLD;
}
