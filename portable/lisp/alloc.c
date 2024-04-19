#include <stdio.h>
#include <stdlib.h>
#include "alloc.h"
#include "objects.h"
#include "parser.h"

/// Список регионов
struct region *regions;

/**
 * Инициализация свободного региона
 */
void init_regions()
{
#ifndef OS
    regions = (struct region *)malloc(MAX_REGION_SIZE);
#else
    regions = (struct region *)MEM_START;
#endif   
    regions->free = 1;
    regions->next = NULL;
    regions->prev = NULL;
    regions->size = MAX_REGION_SIZE - sizeof(struct region) + 4;
    //    for (int i = 0; i < regions->size; i++)
    //	regions->data[i] = 0;
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
    char *p;
    struct region *r = regions;
    if ((size & 0xf) != 0)
	size = ((size >> 4) + 1) << 4;
    int offset_markup = sizeof(struct region) - 4;
    int size2 = size + offset_markup;
    while (r != NULL) {
        if (r->free == 1 && r->size >= size2) {
	    p = r->data;
            struct region *free_reg = (struct region *)(p + size);
            free_reg->free = 1;
            free_reg->next = r->next;
            free_reg->prev = r;
            free_reg->size = r->size - size2;
            r->next = free_reg;
            r->size = size;
            r->magic = MAGIC;
            r->free = 0;
            return p;
        }
	    r = r->next;
    }
    error("Alloc region: out of memory\n");
    return (void *)ERROR;
}

/**
 * Освобождение памяти региона
 * @param data адрес данных освобождаемой  области памяти
 *
 */
void free_region(void *data)
{
    struct region *r, *rprev, *rnext;
    int offset = sizeof(struct region) - 4;
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

/** 
 * Вычисление используемой памяти
 *
 * @return значение в байтах
 */
int regions_mem()
{
    int m = 0;
    struct region *r = regions;
    while (r != NULL) {
	if (r->free == 0)
	    m += r->size + sizeof(struct region);
	r = r->next;
    }
    return m;
}
