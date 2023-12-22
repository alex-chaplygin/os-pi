#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "objects.h" 
#include "parser.h"
#include "symbols.h"
#include "eval.h"

/// Индекс последнего объекта массива
int last_object = 0;
/// Массив или хранилище объектов
object_t objects[MAX_OBJECTS];
/// Указывает на начало списка свободных объектов 
object_t *free_objs = NULL;

/// Индекс последней пары
int last_pair = 0;
/// Массив или хранилище пар
pair_t pairs[MAX_PAIRS];
/// Список свободных пар
pair_t *free_pairs = NULL;

/// Индекс последнего символа
int last_symbol = 0;
/// Хранилище символов
symbol_t symbols[MAX_SYMBOLS];

/// Индекс последней строки
int last_string = 0;
/// Хранилище строк
string_t strings[MAX_STRINGS];
/// Список свободных строк
string_t *free_strings = NULL;

/// Индекс последнего массива
int last_array = 0;
/// Хранилище массивов
array_t arrays[MAX_ARRAYS];
/// Список свободных массивов
array_t *free_arrays = NULL;

/// Хранилище для регионов
char region_data[MAX_REGION_SIZE];
/// Список регионов
struct region *regions;

/** 
 * Создание нового объекта из пула объектов
 *
 * @param type тип объекта
 * @param data указатель на данные
 *
 * @return указатель на созданный объект
 */
object_t *object_new(type_t type, void *data)
{
    object_t *new;
    if (last_object == MAX_OBJECTS) {
	    if (free_objs == NULL) {
		error("Error: out of memory: objects\n");
		return ERROR;
	    }
        new = free_objs;
        free_objs = free_objs->next;
    } else
        new = &objects[last_object++];
    new->type = type;
    new->mark = 0;
    new->next = NULL;
    new->free = 0;
    if (type == NUMBER)
        new->u.value = *(int *)data;
    else if (type == SYMBOL)
        // заполнить поле строки
      new->u.symbol = find_symbol((char *)data);
    else if (type == PAIR)
        new->u.pair = (pair_t *)data;
    else if (type == STRING)
	new->u.str = new_string((char *)data);
    else if (type == ARRAY) 
	new->u.arr = (array_t *)data;
    return new;
}

/** 
 * Освобождение памяти объекта
 * 
 * @param obj объект для освобождения
 */
void free_object(object_t *obj)
{
    if (obj == NULL) {
    	error("free_object: null pointer: obj");
    	return;
    }
    if (obj->free)
	 return;
    if (obj->type == STRING)
	free_string(obj->u.str);
    else if (obj->type == ARRAY)
	free_array(obj->u.arr);
    obj->next = free_objs;
    obj->free = 1;
    free_objs = obj;
}

/** 
 * Создание нового объекта пары
 * 
 * @param left левый объект
 * @param right правый объект
 * 
 * @return указатель на объект пары
 */
object_t *new_pair(object_t *left, object_t *right)
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
    return object_new(PAIR, pair);
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
	return (symbol_t*)ERROR;
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

/**
 * Создание нового объекта массива
 *
 * @param list - список
 *
 * @return указатель на объект массива
 */
array_t *new_array(object_t *list)
{
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
    object_t *a = list;
    while (a != NULL) {
	array->length++;
	a = TAIL(a);
    }
    array->data = alloc_region(array->length * sizeof(object_t *));
    object_t **d = array->data;
    a = list;
    while (a != NULL) {
	*d++ = FIRST(a);
	a = TAIL(a);
    }
    return array;
}

/**
 * Создание нового объекта пустого массива 
 *
 * @param length - длина массива
 *
 * @return указатель на объект массива
 */
array_t *new_empty_array(int length)
{
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
    array->length = length;
    array->next = NULL;
    array->free = 0;   
    array->data = alloc_region(array->length * sizeof(object_t *));
    object_t **d = array->data;
    for (int i = 0; i < length; i++)
	*d++ = NULL;
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
 * Пометить объект как используемый
 * 
 * @param obj - помечаемый объект
 */
void mark_object(object_t *obj)
{
    if (obj == NULL || obj == NOVALUE || obj->mark == 1)
	return;
    obj->mark = 1;
    if (obj->type == PAIR) {
	mark_object(obj->u.pair->left);
	mark_object(obj->u.pair->right);
    } else if (obj->type == ARRAY) {
	array_t *arr = obj->u.arr;
	for (int i = 0; i < arr->length; i++)
	    mark_object(arr->data[i]);
    }
}

/** 
 * Освобождаем все непомеченные объекты, снимаем пометки
 */
void sweep()
{
    object_t *obj = objects;

    for (int i = 0; i < last_object; i++) {
	if (!obj->mark) {
	    if (obj->type == PAIR)
		free_pair(obj->u.pair);
	    free_object(obj);
	}
	else
	    obj->mark = 0;
	obj++;
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

/**
 * Печать списка пар (5 . 7)
 */

int print_counter = 0;

void print_list(object_t *obj)
{
    if (obj == NULL)
	return;
    if(obj->u.pair->print_counter == print_counter){
	printf("...");
	return;
    }   
    obj->u.pair->print_counter = print_counter; //помечаем на текущем 
    print_obj(obj->u.pair->left);
    if (obj->u.pair->right == NULL)
	return;
    if (obj->u.pair->right->type != PAIR) {
	printf(" . ");
	print_obj(obj->u.pair->right);
    } else {
	printf(" ");
	print_list(obj->u.pair->right);
    }
}

/**
 * Печать массива
 */
void print_array(object_t *obj)
{
    array_t *a = obj->u.arr;
    for (int i = 0; i < a->length; i++) {
	print_obj(a->data[i]);
	if (i < a->length - 1)
	    printf(" ");
    }   
}
    
/**
 * Печать объекта
 */
void print_obj(object_t *obj)
{
    if (obj == NULL)
	printf("NIL");
    else if (obj->type == NUMBER)
	printf("%d", obj->u.value);
    else if (obj->type == STRING)
	printf("\"%s\"", obj->u.str->data);
    else if (obj->type == SYMBOL)
	printf("%s", obj->u.symbol->str);
    else if (obj->type == PAIR) {
	printf("(");
	print_list(obj);
	printf(")");
    } else if (obj->type == ARRAY) {
	printf("#(");
	print_array(obj);
	printf(")");
    }
}

/**
 * Печать списка свободных объектов
 */
void print_free_objs()
{
    object_t *f = free_objs;
    while (f != NULL) {
        printf("%d->", f - objects);
	f = f->next;
    }
    printf("\n");
}

/**
 * Печать списка свободных пар
 */
void print_free_pairs()
{
    pair_t *f = free_pairs;
    while (f != NULL) {
        printf("%d->", f - pairs);
	f = f->next;
    }
    printf("\n");
}

object_t *dump_free(object_t *o)
{
    printf("free_objs: ");
    print_free_objs();
    printf("free_pairs: ");
    print_free_pairs();
}

/** 
 * Печать подробной информации о объектах и цепях:
 * @param p_obj - включить вывод об объектах (1)
 * @param p_pair - включить вывод о цепях (1)
 * @param p_free - включить вывод о свободных элементах (1)
 * @param p_not_free - включить вывод о занятых элементах (1)
 */
void print_mem(int p_obj, int p_pair, int p_free, int p_not_free)
{
    if (p_obj)
        printf("-- OBJECTS --");
    int max = !p_obj ? MAX_OBJECTS : 0;
    for (int i = MAX_OBJECTS - 1; i >= max; i--) {
	if (&objects[i] == NULL)
	    printf("\nO_%d-NULL", i);
	if (p_free && objects[i].free)
	    printf("\nO_%d-Free", i);
	else if (p_not_free && !objects[i].free) {
	    printf("\nO_%d-N: ", i);
	    print_obj(&objects[i]);
	}
    }
    if (p_pair)
        printf("\n\n-- PAIRS --");
    max = !p_pair ? MAX_PAIRS : 0;
    for (int i = MAX_PAIRS - 1; i >= 0; i--) {
	if (&pairs[i] == NULL)
	    printf("\nP_%d-NULL", i);
	if (p_free && pairs[i].free)
	    printf("\nP_%d-Free", i);
	else if (p_not_free && !pairs[i].free) {
	    printf("\nP_%d-N: ", i);
	    object_t *pair;
	    pair->u.pair = &pairs[i];
	    print_list(pair);
	}
    }
}

/** 
 * Инициализация свободного региона
 */
void init_regions()
{
    regions = (struct region *)region_data;
    regions->free = 1;
    regions->next = NULL;
    regions->prev = NULL;
    regions->size = MAX_REGION_SIZE - sizeof(struct region) + sizeof(char *);
    //for(int i =0;i < regions->size;i++)regions->data[i] = 0;
}

/**
 * Выделение нового региона
 * @param size размер выделенного региона в байтах
 * 
 * @return указатель на данные выделенного региона
 */
void *alloc_region(int size)
{
    /// найти первый свободный регион подходящего размера
    struct region *r = regions;
    if ((size & 3) != 0)
	size = ((size >> 2) + 1) << 2;
    int offset_markup = 3 * sizeof(int) + 2 * sizeof(struct region *);
    int size2 = size + offset_markup;
    while (r != NULL) {
        if (r->free == 1 && r->size >= size2) {
            struct region *free_reg = (struct region *)(r->data + size);
            free_reg->free = 1;
            free_reg->next = r->next;
            free_reg->prev = r;
            free_reg->size = r->size - size2;
            r->next = free_reg;
            r->size = size;
            r->magic = MAGIC;
            r->free = 0;
            return r->data;
        }
	    r = r->next;
    }
    error("Alloc region: out of memory\n");
    return ERROR;
}

/**
 * Освобождение памяти региона
 * @param data адрес данных освобождаемой  области памяти
 * 
 */
void free_region(void *data)
{
    struct region *r, *rprev, *rnext;
    int offset = 3 * sizeof(int) + 2 * sizeof(struct region *);
    r = (struct region *)((char *)data - offset);
    if (r->magic != MAGIC) {
	error("Free region: no magic\n");
	return;
    }
    rnext = r->next;
    rprev = r->prev;
    r->free = 1;
    r->magic = 0;
    if (rnext != NULL && rnext->free == 1) {
        r->size += offset + rnext->size;
        r->next = rnext->next;
	if (r->next != NULL)
	    r->next->prev = r;
    }
    if (rprev != NULL && rprev->free == 1) {
        rprev->size += offset + r->size;
	rprev->next = r->next;
	if (rprev->next != NULL)
	    rprev->next->prev = rprev;
    } 
}
