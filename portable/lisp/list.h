#include "atom.h"

#define MAX_STR 50

typedef enum {
    NUMBER,
    ATOM,
    LIST
} type_t;

struct list_s;

typedef struct
{
    type_t type; // тип объекта
    union {
        int value; // если объект число, то его значение
        atom_t *atom; // если объект атом, то указатель на атом
        struct list_s *list; // Если объект список то указатель на список
    } u;
} object_t; // Структура объекта

typedef struct list_s
{
    object_t *elem; // элемент списка
    struct list_s *next;// указатель на следующий элемент
} list_t; // Структура списка

void list_add(list_t **head, object_t *obj);
void print_elem(object_t *head);
object_t *object_new(type_t type, void *data);