#include "alloc.h"
#include "objects.h"
#include "bind.h"

/// Глобальное окружение
bind_t *global_env = NULL;

/// Временные объекты, защищенные от сборки мусора
temp_bind_t *protected = NULL; // указатель на начало списка текущих временных объектов

/*  
 * Добавляет элемент в глобальное окружение
 * 
 * @param symbol - символ, который добавляется
 */ 
void bind_global(object_t symbol)
{
    bind_t *el = alloc_region(sizeof(bind_t));
    el->obj = symbol;
    el->next = global_env;
    global_env = el;
}

/*
 * Добавляет элемент в глобальное окружение, если его нет в списке
 * Если есть - ничего не происходит
 *
 * @param symbol - символ, который добавляется
 */
void set_global(symbol_t *symbol)
{
    bind_t *cur = global_env;
    
    while (cur != null) {
	if (GET_SYMBOL(cur->obj) == symbol) {
	    bind_global(NEW_OBJECT(SYMBOL, symbol));
	    break;
	}
	cur = cur->next;
    }
}
