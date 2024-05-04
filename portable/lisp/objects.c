#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "objects.h" 
#include "parser.h"
#include "symbols.h"
#include "eval.h"
#include "alloc.h" 

/// Индекс последнего большого числа
int last_bignumber = 0;
/// Хранилище больших чисел
bignumber_t *bignumbers;
/// Список свободных больших чисел
bignumber_t *free_bignumbers = NULL;

/// Индекс последней пары
int last_pair = 0;
/// Массив или хранилище пар
pair_t *pairs;
/// Список свободных пар
pair_t *free_pairs = NULL;

/// Индекс последнего символа
int last_symbol = 0;
/// Хранилище символов
symbol_t symbols[MAX_SYMBOLS];

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

/**
 * Инициализация объектов
 */
void init_objects()
{
    alloc_region(1);
    bignumbers = (bignumber_t *)alloc_region(MAX_NUMBERS * sizeof(bignumber_t));
    pairs = (pair_t *)alloc_region(MAX_PAIRS * sizeof(pair_t));
    strings = (string_t *)alloc_region(MAX_STRINGS * sizeof(string_t));
    arrays = (array_t *)alloc_region(MAX_ARRAYS * sizeof(array_t));
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
    return NEW_OBJECT(BIGNUMBER, number);
}

/**
 * Освобождение памяти для большого числа
 *
 * @param o  объект для освобождения
 */
void free_bignumber(bignumber_t *o)
{
    if (o == NULL) {
    	error("free_bignumber: null pointer: obj");
    	return;
    }    
    if (o->free)
	return;
    o->next = free_bignumbers;
    free_bignumbers = o;
    o->free = 1;
}

/**
 * Создание нового объекта маленького целого числа
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
    if (last_symbol == MAX_SYMBOLS)
	error("Error: out of memory: symbols");
    if (*str == 0)
	return NULL;
    symbol_t *symbol = &symbols[last_symbol++];
    strcpy(symbol->str, str);
    symbol->next = NULL;
    symbol->value = NOVALUE;
    symbol->func = NULL;
    symbol->lambda = NULLOBJ;
    symbol->macro = NULLOBJ;
    return symbol;
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
    free_region(a->data);
}
/**
 *Пометить объект как используемый 
 * Для маленьких чисел - не хранится 
 * Для больших чисел - старший бит в free 
 * Для символов - нет 
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
	if (GET_MARK(GET_PAIR(obj)->left) == 1)
	    return;
	SET_MARK(GET_PAIR(obj)->left);
	mark_object(GET_PAIR(obj)->left);
	mark_object(GET_PAIR(obj)->right);
    }
    else if (TYPE(obj) == BIGNUMBER) {
	if (((GET_BIGNUMBER(obj)->free) & mask) != 0)
	    return;
	GET_BIGNUMBER(obj)->free |= mask;
    }
    else if (TYPE(obj) == STRING) {
        if (((GET_STRING(obj)->length) & mask) != 0)
	    return;
	GET_STRING(obj)->length |= mask;
    }
    else if (TYPE(obj) == ARRAY) {
        if (((GET_ARRAY(obj)->length) & mask) != 0)
	    return;
	GET_ARRAY(obj)->length |= mask;
    }
}

/**
 * Освобождаем все непомеченные объекты, снимаем пометки
 */
void sweep()
{
    int mask = 1 << 31;
    for (int i = 0; i < last_bignumber; i++) {
        bignumber_t *big_num = &bignumbers[i];
        if ((big_num->free & mask) == 0)
	    free_bignumber(big_num);
	else big_num->free &= ~mask;
    }
    for (int i = 0; i < last_pair; i++) {
        pair_t *pair = &pairs[i];
	if (GET_MARK(pair->left) != 1)
	    free_pair(pair);
	else CLEAR_MARK(pair->left);
    }
    for (int i = 0; i < last_string; i++) {
	string_t *str = &strings[i];
	if ((str->length & mask) == 0)
	    free_string(str);
	else str->length &= ~mask;
    }
    for (int i = 0; i < last_array; i++) {
	array_t *arr = &arrays[i];
	if ((arr->length & mask) == 0)
	    free_array(arr);
	else arr->length &= ~mask;
    }
}

/**
 * Сборка мусора
 */
void garbage_collect()
{
    for (int i = 0; i < last_symbol; i++) { 
         mark_object(symbols[i].value);
         mark_object(symbols[i].lambda);
         mark_object(symbols[i].macro);
     }
     sweep();
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
    for (int i = 0; i < a->length; i++) {
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
 	printf("NIL");
    else if (TYPE(obj) == NUMBER)
 	printf("%d", get_value(obj));
    else if (TYPE(obj) == BIGNUMBER)
	printf("%d", GET_BIGNUMBER(obj)->value);
    else if (TYPE(obj) == STRING)
 	printf("\"%s\"", ((string_t *)GET_ADDR(obj))->data);
    else if (TYPE(obj) == SYMBOL)
 	printf("%s", ((symbol_t *)GET_ADDR(obj))->str);
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

#define MK_FREE(name, type) \
int name##_count()\
{\
    type *f = name;\
    int c = 0;\
    while (f) {\
	f = f->next;\
	c++;\
    }\
    return c;\
}
MK_FREE(free_bignumbers, bignumber_t)
MK_FREE(free_pairs, pair_t)
MK_FREE(free_strings, string_t)
MK_FREE(free_arrays, array_t)

/** 
 * Печать статистики сборки мусора и памяти
 */
object_t print_gc_stat(object_t o)
{
    printf("bignumbers: %d(-%d) of %d\n", last_bignumber, free_bignumbers_count(), MAX_NUMBERS);
    printf("pairs: %d(-%d) of %d\n", last_pair, free_pairs_count(), MAX_PAIRS);
    printf("symbols: %d of %d\n", last_symbol, MAX_SYMBOLS);
    printf("strings: %d(-%d) of %d\n", last_string, free_strings_count(), MAX_STRINGS);
    printf("arrays: %d(-%d) of %d\n", last_array, free_arrays_count(), MAX_ARRAYS);
    printf("used mem: %d of %d\n", regions_mem(), MAX_REGION_SIZE);
    return NULLOBJ;
}
