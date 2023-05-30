#include "list.h" 
#include <stdio.h>
#include <stddef.h>
#define MAX_NUM 100

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

/** 
 * Добавление элемента в список
 * 
 * @param type тип элемента
 * @param data указатель на данные
 */
void list_add(list_t **head, type_t type, void *data)
{
    list_t *new = alloc();
    new->type = type;
    if (type == NUMBER)
        new->u.value = *(int *)data;
    else if (type == ATOM)
        // заполнить поле строки
        str_copy((char *)data, new->u.atom);
    else if (type == LIST)
        new->u.list = (list_t *)data;
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

void print_list(list_t *head)
{
    printf("(");
    for (list_t *cur = head; cur != NULL; cur = cur->next) {
        if (cur->type == NUMBER)
            printf("%d ", cur->u.value); // выводим содержимое массива в кавычках и разделяем запятой
        else if (cur->type == ATOM)
            printf("%s ", cur->u.atom);
        else if (cur->type == LIST)
            print_list(cur->u.list);
    }
    printf(") ");
}
