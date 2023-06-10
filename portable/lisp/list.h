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
    type_t type; // тип элемента
    union {
        int value; // если элемент число, то его значение
        atom_t *atom; // если элемент атом, то указатель на атом
        struct list_s *list; // Если элемент список то указатель на список
    } u;
} element_t;

typedef struct list_s
{
    element_t *elem; // элемент списка
    struct list_s *next;// указатель на следующий элемент
} list_t;

void list_add(list_t **head, type_t type, void *data);
void print_elem(element_t *head);
list_t *alloc();