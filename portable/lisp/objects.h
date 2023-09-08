#ifndef SYMBOLS
#define SYMBOLS
#include "symbols.h"

#define MAX_STR 50

/// перечисление типов объектов
typedef enum {
  NUMBER, /// целое число
  SYMBOL, ///символ
  PAIR    ///пара
} type_t;

struct pair_s;
struct symbol_s;

typedef struct object_s
{
    type_t type; // тип объекта
    union {
        int value; // если объект число, то его значение
        struct symbol_s *symbol; // указатель на символ
        struct pair_s *pair; // Если объект пара из 2-х объектов (левый и правый) то указатель на пару
    } u;
} object_t; // Структура объекта

typedef struct pair_s
{
    object_t *left; // элемент 1
    object_t *right; // элемент 2
} pair_t;

void print_elem(object_t *head);
object_t *object_new(type_t type, void *data);
object_t *new_pair(object_t *left, object_t *right);
#endif
