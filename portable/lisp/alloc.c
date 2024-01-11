#include <stdio.h>
#include "alloc.h"
#include "objects.h"
#include "parser.h"

/// Хранилище для регионов
char region_data[MAX_REGION_SIZE];
/// Список регионов
struct region *regions;

/**
 * Инициализация свободного региона
 */
void init_regions()
{
    printf("init regions\n");
    regions = (struct region *)region_data;
    regions->free = 1;
    regions->next = NULL;
    regions->prev = NULL;
    regions->size = MAX_REGION_SIZE - sizeof(struct region) + sizeof(char *);
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

