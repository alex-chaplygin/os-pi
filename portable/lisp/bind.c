#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "alloc.h"
#include "objects.h"
#include "bind.h"

/// Глобальное окружение
extern bind_t global_env[];
extern int last_global;
/// Временные объекты, защищенные от сборки мусора
extern temp_bind_t protected[];
extern int last_protected;

/*  
 * Добавляет элемент в глобальное окружение
 * 
 * @param symbol - символ, который добавляется
 */ 
void bind_global(object_t symbol)
{
    // printf("bind_global: ");
    //PRINT(symbol);
    bind_t *el = &global_env[last_global++];
    if (last_global == MAX_GLOBALS)
	error("MAX GLOBALS");
    el->obj = symbol;
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
    /* printf("set_global: %s %x\n", symbol->str, symbol); */
    for (int i = 0; i < last_global; i++, cur++) {
	//	if (!strcmp(GET_SYMBOL(cur->obj)->str, symbol->str)) {
	if (GET_SYMBOL(cur->obj) == symbol) {
	    //GET_SYMBOL(cur->obj)->value = symbol->value;
	    return;
	}
    }
    bind_global(NEW_OBJECT(SYMBOL, symbol));    
}