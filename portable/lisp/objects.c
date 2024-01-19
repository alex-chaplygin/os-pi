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
    bignumbers = (bignumber_t *)alloc_region(MAX_NUMBERS * sizeof(bignumber_t));
    pairs = (pair_t *)alloc_region(MAX_PAIRS * sizeof(pair_t));
    strings = (string_t *)alloc_region(MAX_STRINGS * sizeof(string_t));
    arrays = (array_t *)alloc_region(MAX_ARRAYS * sizeof(array_t));
}
/* /\**  */
/*  * Освобождение памяти объекта */
/*  *  */
/*  * @param obj объект для освобождения */
/*  *\/ */
/* void free_object(object_t *obj) */
/* { */
/*     if (obj == NULL) { */
/*     	error("free_object: null pointer: obj"); */
/*     	return; */
/*     } */
/*     if (obj->free) */
/* 	 return; */
/*     if (obj->type == STRING) */
/* 	free_string(obj->u.str); */
/*     else if (obj->type == ARRAY) */
/* 	free_array(obj->u.arr); */
/*     obj->next = free_objs; */
/*     obj->free = 1; */
/*     free_objs = obj; */
/* } */

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
	if (free_bignumbers == NULL) {
	    error("Error: out of memory: numbers");
	    return ERROR;
	}
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
    unsigned int mask = (1 << ADDR_BITS) - 1;
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
 	if (free_pairs == NULL) { 
 	    error("Error: out of memory: pairs"); 
 	    return ERROR; 
 	} 
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
    if (p == NULL) {
    	error("free_pair: null pointer: obj");
    	return;
    }

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
    if (last_symbol == MAX_SYMBOLS) {
	error("Error: out of memory: symbols");
	return (symbol_t *)ERROR;
    }
    if (*str == 0)
	return NULL;
    symbol_t *symbol = &symbols[last_symbol++];
    strcpy(symbol->str, str);
    symbol->next = NULL;
    symbol->value = NOVALUE;
    symbol->func = NULL;
    symbol->lambda = NULL;
    symbol->macro = NULL;
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
	if (free_strings == NULL) {
	    error("Error: out of memory: strings");
	    return (string_t *)ERROR;
	}
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
	if (free_arrays == NULL) {
	    error("Error: out of memory: arrays");
	    return (array_t *)ERROR;
	}
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

/* /**
/*  * Создание нового объекта пустого массива  */
/*  * */
/*  * @param length - длина массива */
/*  * */
/*  * @return указатель на объект массива */
/*  *\/ */
/* array_t *new_empty_array(int length) */
/* { */
/*     array_t *array; */
/*     if (last_array == MAX_ARRAYS) { */
/* 	if (free_arrays == NULL) { */
/* 	    error("Error: out of memory: arrays"); */
/* 	    return (array_t *)ERROR; */
/* 	} */
/* 	array = free_arrays; */
/* 	free_arrays = free_arrays->next; */
/*     } else */
/* 	array = &arrays[last_array++]; */
/*     array->length = length; */
/*     array->next = NULL; */
/*     array->free = 0;    */
/*     array->data = alloc_region(array->length * sizeof(object_t *)); */
/*     object_t **d = array->data; */
/*     for (int i = 0; i < length; i++) */
/* 	*d++ = NULL; */
/*     return array; */
/* } */

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
    if (obj == NULLOBJ || obj == NOVALUE/* || GET_MARK(obj) == 1*/)
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

/* /\**  */
/*  * Освобождаем все непомеченные объекты, снимаем пометки */
/*  *\/ */
/* void sweep() */
/* { */
/*     object_t *obj = objects; */

/*     for (int i = 0; i < last_object; i++) { */
/* 	if (!obj->mark) { */
/* 	    if (obj->type == PAIR) */
/* 		free_pair(obj->u.pair); */
/* 	    free_object(obj); */
/* 	} */
/* 	else */
/* 	    obj->mark = 0; */
/* 	obj++; */
/*     } */
/* } */

/* /\** */
/*  * Сборка мусора */
/*  *\/ */
/* void garbage_collect() */
/* { */
/*     for (int i = 0; i < last_symbol; i++) { */
/*         mark_object(symbols[i].value); */
/*         mark_object(symbols[i].lambda); */
/*         mark_object(symbols[i].macro); */
/*     } */
/*     sweep(); */
/* } */

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

/* /\** */
/*  * Печать списка свободных объектов */
/*  *\/ */
/* void print_free_objs() */
/* { */
/*     object_t *f = free_objs; */
/*     while (f != NULL) { */
/*         printf("%d->", f - objects); */
/* 	f = f->next; */
/*     } */
/*     printf("\n"); */
/* } */

/* /\** */
/*  * Печать списка свободных пар */
/*  *\/ */
/* void print_free_pairs() */
/* { */
/*     pair_t *f = free_pairs; */
/*     while (f != NULL) { */
/*         printf("%d->", f - pairs); */
/* 	f = f->next; */
/*     } */
/*     printf("\n"); */
/* } */

/* object_t *dump_free(object_t *o) */
/* { */
/*     printf("free_objs: "); */
/*     print_free_objs(); */
/*     printf("free_pairs: "); */
/*     print_free_pairs(); */
/* } */

/* /\**  */
/*  * Печать подробной информации о объектах и цепях: */
/*  * @param p_obj - включить вывод об объектах (1) */
/*  * @param p_pair - включить вывод о цепях (1) */
/*  * @param p_free - включить вывод о свободных элементах (1) */
/*  * @param p_not_free - включить вывод о занятых элементах (1) */
/*  *\/ */
/* void print_mem(int p_obj, int p_pair, int p_free, int p_not_free) */
/* { */
/*     if (p_obj) */
/*         printf("-- OBJECTS --"); */
/*     int max = !p_obj ? MAX_OBJECTS : 0; */
/*     for (int i = MAX_OBJECTS - 1; i >= max; i--) { */
/* 	if (&objects[i] == NULL) */
/* 	    printf("\nO_%d-NULL", i); */
/* 	if (p_free && objects[i].free) */
/* 	    printf("\nO_%d-Free", i); */
/* 	else if (p_not_free && !objects[i].free) { */
/* 	    printf("\nO_%d-N: ", i); */
/* 	    print_obj(&objects[i]); */
/* 	} */
/*     } */
/*     if (p_pair) */
/*         printf("\n\n-- PAIRS --"); */
/*     max = !p_pair ? MAX_PAIRS : 0; */
/*     for (int i = MAX_PAIRS - 1; i >= 0; i--) { */
/* 	if (&pairs[i] == NULL) */
/* 	    printf("\nP_%d-NULL", i); */
/* 	if (p_free && pairs[i].free) */
/* 	    printf("\nP_%d-Free", i); */
/* 	else if (p_not_free && !pairs[i].free) { */
/* 	    printf("\nP_%d-N: ", i); */
/* 	    object_t *pair; */
/* 	    pair->u.pair = &pairs[i]; */
/* 	    print_list(pair); */
/* 	} */
/*     } */
/* } */
