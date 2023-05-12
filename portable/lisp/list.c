#define MAX_NUM 100
#define MAX_STR 50



typedef struct list_s
{
    type_t type; // тип элемента списка
    union {
        int value; // если элемент число, то его значение
        char atom[MAX_STR]; // если элемент атом, то строка атома
        struct list_s *list;
    } u;
    struct list_s *next;
} list_t;

list_t *head = NULL;//глобальный список
int count = 0; // счетчик чисел в массиве
list_t list_heap[MAX_NUM];

list_t *alloc()
{
    return &list_heap[count++];
}
// копирование строки str1 в строку str2
void str_copy (char *str1, char *str2)
{
    //int i = 0; //"abc" ['a', 'b', 'c', 0]
    while (*str1)
        *str2++ = *str1++;
    *str2 = 0;
}

void list_add(type_t type, void *data)
{
    list_t *new = alloc();
    new->type = type;
    if (type == NUMBER)
        new->value = data;
    else if (type == ATOM)
    new->next = NULL;
    if (*head == NULL)
        *head = new;
    else {
        list_t *last = *head;
        while (last->next != NULL)
            last = last->next;
        last->next = new;
    }
}
void print_list(){
    
}