#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include "list.h" 

#define MAX_OBJECTS 50

int last_object = 0;
object_t objects[MAX_OBJECTS];

//создать объект
// type - тип объекта
// data - указатель на данные
// Возвращает указатель на созданный объект
object_t *object_new(type_t type, void *data)
{
    object_t *new = &objects[last_object++];
    if (last_object == MAX_OBJECTS)
        return NULL;
    new->type = type;
    if (type == NUMBER)
        new->u.value = *(int *)data;
    else if (type == ATOM)
        // заполнить поле строки
        new->u.atom = find_atom((char *)data);
    else if (type == PAIR)
        new->u.pair = (pair_t *)data;   
    return new;
}



/** 
 * Добавление элемента в список
 * 
 * @param head указатель на список
 * @param obj указатель на объект
 */
void list_add(pair_t **head, object_t *obj)
{
    pair_t *new2 = malloc(sizeof(pair_t));
    new2->elem = obj;
    new2->next = NULL;
    if (*head == NULL)
        *head = new2;
    else {
        pair_t *last = *head;
        while (last->next != NULL)
            last = last->next;
        last->next = new2;
    }
}

void print_elem(object_t *el)
{
    if (el->type == NUMBER)
        printf("%d ", el->u.value); // выводим содержимое массива в кавычках и разделяем запятой
    else if (el->type == ATOM)
        printf("%s ", el->u.atom->str);
    else if (el->type == LIST)
    {
        printf("(");
        for (pair_t *cur = el->u.list; cur != NULL; cur = cur->next) 
            print_elem(cur->elem);
        printf(") ");
    }
}