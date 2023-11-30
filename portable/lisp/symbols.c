#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "objects.h"
#include "symbols.h"

#define HASH_SIZE 1000
#define MAX_SYMBOL_SIZE 80

symbol_t *hash_table[HASH_SIZE];

//хэш функция строки 
unsigned int hash(char *str)
{
    unsigned int res = 456886;//*str++; 
  
    while (*str)
        res = (res * 846431 + *str++)%257659;
    return res;
}

int compare_str(char *str1, char *str2)
{
    while (*str1 && *str2)
    {
        if (*str1++ != *str2++)
        return 0;
    }
    
    return *str1 == *str2;
}

/** 
 * функция ищет атом в хеш -таблице
 * если не найден, то возвращает NULL
 * @param str - строка атома
 
 * @return  указатель на структуру символа
 */
symbol_t *check_symbol(char *str)
{
    if (str[0] == '\0' || strlen(str) > MAX_SYMBOL_SIZE)
        return NULL;
    int i = hash(str) % HASH_SIZE;
    symbol_t *el = hash_table[i];
    if (el == NULL)
        return NULL;
    for (symbol_t *cur = el; cur != NULL; cur = cur->next)
            if (compare_str(cur->str, str))
                return cur;    
    return NULL;
}

/** 
 * функция ищет атом в хеш -таблице
 * если не найдена, то добавляет в таблицу
 * str - строка атома
 
 * @return  указатель на структуру символа
 */
symbol_t *find_symbol(char *str) 
{
    if (str[0] == '\0' || strlen(str) > MAX_SYMBOL_SIZE)
        return NULL;
    int i = hash(str) % HASH_SIZE;
    symbol_t *el = hash_table[i];
    if (el == NULL ){
        symbol_t *new = new_symbol(str);
        hash_table[i] = new;
        el = new;
    }
    else
    {
        for (symbol_t *cur = el; cur != 0; cur = cur->next)
            if (compare_str(cur->str, str))
                return cur;
                
        symbol_t *new = new_symbol(str);
        symbol_t *last = el;
        while (last->next != 0)
            last = last->next;
        last->next = new;
        return new;
    }    
    return el;
}

/**
 * устанавливает функцию для символа примитива
 * @param name имя примитива функции
 * @param func_ptr указатель на функцию 
 */
void register_func(char *name, func_t func_ptr)
{
    symbol_t *s = find_symbol(name);
    s->func = func_ptr;
}

void print_table() 
{
    for(int i = 0; i < HASH_SIZE; i++)
    {
        symbol_t *cur;
        printf("%d ", i);
        for (cur = hash_table[i]; cur->next != 0; cur = cur->next) 
            printf("%s->", cur->str);
        printf("%s->", cur->str);    
        printf("\n");
    }
}