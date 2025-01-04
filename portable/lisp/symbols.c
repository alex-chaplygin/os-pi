#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "objects.h"
#include "symbols.h"
#include "bind.h"

symbol_t *hash_table[HASH_SIZE];

//хэш функция строки 
unsigned int hash(char *str)
{
    unsigned int res = 456886;//*str++; 
  
    while (*str)
        res = (res * 846431 + *str++) % 257659;
    return res;
}

/** 
 * функция ищет атом в хеш -таблице
 * если не найден, то возвращает NULL
 * @param str - строка атома
 
 * @return  указатель на структуру символа
 */
symbol_t *check_symbol(char *str)
{
    if (str[0] == '\0' || strlen(str) > MAX_SYM_STR)
        return NULL;
    int i = hash(str) % HASH_SIZE;
    symbol_t *el = hash_table[i];
    if (el == NULL)
        return NULL;
    for (symbol_t *cur = el; cur != NULL; cur = cur->next)
            if (!strcmp(cur->str, str))
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
    if (str[0] == '\0' || strlen(str) > MAX_SYM_STR)
        return NULL;
    int i = hash(str) % HASH_SIZE;
    symbol_t *el = hash_table[i];
    if (el == NULL ){
        symbol_t *new = new_symbol(str);
        hash_table[i] = new;
	new->hash_index = i;
        el = new;
    } else {
        for (symbol_t *cur = el; cur != 0; cur = cur->next)
            if (!strcmp(cur->str, str))
                return cur;
        symbol_t *new = new_symbol(str);
	new->hash_index = i;
        symbol_t *last = el;
        while (last->next != 0)
            last = last->next;
        last->next = new;
        return new;
    }    
    return el;
}

/** 
 * удаляет символ из таблицы
 * @param s - удаляемый символ
 */
void hash_remove(symbol_t *s)
{
    symbol_t *el = hash_table[s->hash_index];
    if (el == NULL)
       error("Hash remove error: got NULL on symbol %s index %d", s->str, s->hash_index);
    if (el == s)
        hash_table[s->hash_index] = el->next;
    else {
       for (symbol_t *cur = el; cur != NULL; cur = cur->next)
                   if (cur->next == s)
               cur->next = s->next;
    }    
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
    bind_static(NEW_OBJECT(SYMBOL, s));
}

void print_table() 
{
    for(int i = 0; i < HASH_SIZE; i++)
    {
        symbol_t *cur;
	cur = hash_table[i];
	if (cur != NULL) {
	    printf("%d ", i);
	    while (cur->next != NULL) {
		printf("%s -> ", cur->str);
		cur = cur->next;
	    }
	    printf("%s\n", cur->str);
	}
    }
}
