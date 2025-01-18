#ifndef SYMBOLS
#define SYMBOLS
/// Размер хеш таблицы
#define HASH_SIZE 1000

symbol_t *check_symbol(char *str);
symbol_t *find_symbol(char *str);
void hash_remove(symbol_t *s);
void register_func(char *name, func_t func_ptr, int nary, int count);
#endif

