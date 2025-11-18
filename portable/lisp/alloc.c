#include <stdio.h>
#include <stdlib.h>
#include "alloc.h"
#include "objects.h"
#include "parser.h"

/// Список регионов
struct region *regions;
/// Указатель на свободный регион, с которого начнется поиск
struct region *rover;

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
#ifdef X32
    regions = (struct region *)((((int)regions >> MARK_BIT) + 1) << MARK_BIT);
#else
    regions = (struct region *)((((long long)regions >> MARK_BIT) + 1) << MARK_BIT);
#endif
    regions->free = 1;
    regions->next = regions;
    regions->prev = regions;
    regions->size = MAX_REGION_SIZE - sizeof(struct region) + sizeof(char *);
    //    for (int i = 0; i < regions->size; i++)
    //	regions->data[i] = 0;
    rover = regions;
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
    struct region *r = rover;
    if ((size & ((1 << MARK_BIT) - 1)) != 0)
	size = ((size >> MARK_BIT) + 1) << MARK_BIT;
    int offset_markup = sizeof(struct region) - sizeof(char *);
    int size2 = size + offset_markup;
    do {
        if (r->free == 1 && r->size >= size2) {
	    p = r->data;
            struct region *free_reg = (struct region *)(p + size);
            free_reg->free = 1;
            free_reg->next = r->next;
            free_reg->prev = r;
            free_reg->next->prev = free_reg;
            free_reg->size = r->size - size2;
            r->next = free_reg;
            r->size = size;
            r->magic = MAGIC;
            r->free = 0;
	    rover = free_reg;
            return p;
        }
	r = r->next;
    } while (r != rover);
    error("Alloc region: out of memory\n");
}

/**
 * Освобождение памяти региона
 * @param data адрес данных освобождаемой  области памяти
 *
 */
void free_region(void *data)
{
    struct region *r, *rprev, *rnext;
    int offset = sizeof(struct region) - sizeof(char *);
    r = (struct region *)((char *)data - offset);
    if (r->magic != MAGIC) {
	error("Free region: no magic\n");
	return;
    }
    rnext = r->next;
    rprev = r->prev;
    r->free = 1;
    r->magic = 0;
    if (r != regions && rprev->free == 1) {
        rprev->size += offset + r->size;
	rprev->next = r->next;
	r->next->prev = rprev;
	if (r == rover)
	    rover = rprev;
	r = rprev;
    }
    if (rnext != regions && rnext->free == 1) {
        r->size += offset + rnext->size;
        r->next = rnext->next;
	r->next->prev = r;
	if (rnext == rover)
	    rover = r;
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
    do {
	if (r->free == 0)
	    m += r->size + sizeof(struct region);
	r = r->next;
    } while (r != regions);
    return m;
}
