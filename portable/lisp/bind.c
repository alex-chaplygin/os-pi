#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "alloc.h"
#include "objects.h"
#include "bind.h"

/// Массив статических привязок - глобальных символов
object_t static_bind[MAX_STATIC];
/// Число статических объектов
int last_static = 0;
/// Временные объекты, защищенные от сборки мусора
object_t *protected[MAX_PROTECTED];
/// Текущее число временных объектов
int last_protected = 0;

/*  
 * Добавляет объект символ в глобальное окружение
 * 
 * @param symbol - символ, который добавляется
 */ 
void bind_static(object_t symbol)
{
    static_bind[last_static++] = symbol;
    if (last_static == MAX_STATIC)
       error("Maximum static objects");
}

/*
 * Добавляет элемент в глобальное окружение, если его нет в списке
 *
 * @param symbol - символ, который добавляется
 */
void set_global(symbol_t *symbol)
{
    object_t *cur = static_bind;
    for (int i = 0; i < last_static; i++, cur++)
       if (GET_SYMBOL(*cur) == symbol)
           return;
    bind_static(NEW_OBJECT(SYMBOL, symbol));    
}
