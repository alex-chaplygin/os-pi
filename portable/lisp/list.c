#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include "list.h" 

#define MAX_NUM 100

int count = 0; // счетчик чисел в массиве
list_t list_heap[MAX_NUM];

list_t *alloc()
{
    return &list_heap[count++];
}

/** 
 * Добавление элемента в список
 * 
 * @param type тип элемента
 * @param data указатель на данные
 */
void list_add(list_t **head, type_t type, void *data)
{
    element_t *new = malloc(sizeof(element_t));
    new->type = type;
    if (type == NUMBER)
        new->u.value = *(int *)data;
    else if (type == ATOM)
        // заполнить поле строки
        new->u.atom = find_atom((char *)data);
    else if (type == LIST)
        new->u.list = (list_t *)data;
    list_t *new2 = alloc();
    new2->elem = new;
    new2->next = NULL;
    if (*head == NULL)
        *head = new2;
    else {
        list_t *last = *head;
        while (last->next != NULL)
            last = last->next;
        last->next = new2;
    }
}

void print_elem(element_t *el)
{
    if (el->type == NUMBER)
        printf("%d ", el->u.value); // выводим содержимое массива в кавычках и разделяем запятой
    else if (el->type == ATOM)
        printf("%s ", el->u.atom->str);
    else if (el->type == LIST)
    {
        printf("(");
        for (list_t *cur = el->u.list; cur != NULL; cur = cur->next) 
            print_elem(cur->elem);
        printf(") ");
    }
}