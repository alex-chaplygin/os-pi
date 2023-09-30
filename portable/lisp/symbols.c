#include <stdio.h>
#include <stdlib.h>
#include "objects.h"
#include "symbols.h"

#define HASH_SIZE 1000

symbol_t *hash_table[HASH_SIZE];

//хэш функция строки 
unsigned int hash(char *str)
{
    unsigned int res = 456886;//*str++; 
  
    while (*str)
        res = (res * 846431 + *str++)%257659;
    return res;
}

// копирование строки str1 в строку str2
void str_copy (char *str1, char *str2)
{
    //int i = 0; //"abc" ['a', 'b', 'c', 0]
    while (*str1)
        *str2++ = *str1++;
    *str2 = 0;
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
 * если не найдена, то добавляет в таблицу
 * str - строка атома
 
 * @return  указатель на структуру символа
 */
symbol_t *find_symbol(char *str) 
{
    int i = hash(str) % HASH_SIZE;
    symbol_t *el = hash_table[i];
    if (*el->str == 0){
        symbol_t *new = new_symbol(str);
        hash_table[i] = new;
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
        printf("%d ", i);
        for (symbol_t *cur = hash_table + i; cur != 0; cur = cur->next) 
            printf("%s->", cur->str);
        printf("\n");
    }
}
