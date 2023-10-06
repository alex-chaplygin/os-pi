#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "objects.h" 
#include "parser.h"
#include "symbols.h"

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
	    garbage_collect();
	    if (free_objs == NULL) {
		error("Error: out of memory: objects");
		return ERROR;
	    }
        }
        new = free_objs;
        free_objs = free_objs->next;
        new->next = NULL;
	new->free = 0;
    } else
        new = &objects[last_object++];
    new->type = type;
    new->mark = 0;
    if (type == NUMBER)
        new->u.value = *(int *)data;
    else if (type == SYMBOL)
        // заполнить поле строки
      new->u.symbol = find_symbol((char *)data);
    else if (type == PAIR)
        new->u.pair = (pair_t *)data;   
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
    
    //printf("free objects: %d %x \n ", obj - objects, obj->next);
    if (obj->free)
	 return;
    //printf("free\n");
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
	pair->next = NULL;
	pair->free = 0;
    } else
	pair = &pairs[last_pair++];        
    pair->left = left;
    pair->right = right;  
    return object_new(PAIR, pair);
}

/** 
 * Освобождение памяти для пары
 * 
 * @param obj объект для освобождения
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
  symbol_t *symbol = &symbols[last_symbol++];
  if (last_symbol == MAX_SYMBOLS) {
    error("Error: out of memory: symbols");
    return (symbol_t*)ERROR;
  }
  strcpy(symbol->str, str);
  symbol->next = NULL;
  symbol->value = NULL;
  symbol->func = NULL;
  return symbol;
}

/** 
 * Пометить объект как используемый
 * 
 * @param obj - помечаемый объект
 */
void mark_object(object_t *obj)
{
    if (obj == NULL)
	return;
    obj->mark = 1;
    if (obj->type == PAIR) {
	mark_object(obj->u.pair->left);
	mark_object(obj->u.pair->right);
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
    printf("garbage\n");
    for (int i = 0; i < last_symbol; i++)
	mark_object(symbols[i].value);
   
    sweep();
}

/**
 * Печать списка пар
 */
void print_list(object_t *obj)
{
    if (obj == NULL)
	return;
    print_obj(obj->u.pair->left);
    if (obj->u.pair->right == NULL)
	return;
    printf(" ");
    print_list(obj->u.pair->right);
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
    else if (obj->type == SYMBOL)
	printf("%s", obj->u.symbol->str);
    else if (obj->type == PAIR) {
	printf("(");
	print_list(obj);
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
