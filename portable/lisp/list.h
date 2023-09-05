#include "atom.h"

#define MAX_STR 50

typedef enum {
    NUMBER,
    ATOM,
    PAIR
} type_t;

struct pair_s;

typedef struct
{
    type_t type; // тип объекта
    union {
        int value; // если объект число, то его значение
        atom_t *atom; // если объект атом, то указатель на атом
        struct pair_s *pair; // Если объект пара из 2-х объектов (левый и правый) то указатель на пару
    } u;
} object_t; // Структура объекта

typedef struct pair_s
{
    object_t *left; // элемент 1
    object_t *right; // элемент 2
} pair_t;

void list_add(list_t **head, object_t *obj);
void print_elem(object_t *head);
object_t *object_new(type_t type, void *data);
