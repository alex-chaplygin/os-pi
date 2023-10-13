#ifndef SYMBOLS
#define SYMBOLS

symbol_t *check_symbol(char *str);
symbol_t *find_symbol(char *str);
void register_func(char *name, func_t func_ptr);
#endif

