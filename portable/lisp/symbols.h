#ifndef SYMBOLS
#define SYMBOLS

#define HASH_SIZE 1000
#define MAX_SYMBOL_SIZE 80

symbol_t *check_symbol(char *str);
symbol_t *find_symbol(char *str);
void register_func(char *name, func_t func_ptr);
#endif

