#define MAX_STR 50

typedef enum {
    NUMBER,
    ATOM,
    LIST
} type_t;

typedef struct list_s
{
    type_t type; // тип элемента списка
    union {
        int value; // если элемент число, то его значение
        char atom[MAX_STR]; // если элемент атом, то строка атома
        struct list_s *list; // Если элемент список то указатель на список
    } u;
    struct list_s *next;// указатель на следующий элемент
} list_t;

void list_add(list_t **head, type_t type, void *data);
void print_list(list_t *head);
