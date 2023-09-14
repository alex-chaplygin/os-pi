#ifndef OBJECTS
#define OBJECTS
#include "objects.h"
//максимальная длина имени символа
#define MAX_STR 50

struct object_s;

typedef  struct object_s *(*func_t)(struct object_s *);

//структура символа
typedef struct symbol_s
{
  //имя символа
  char str[MAX_STR];
  //указатель на следующий символ в цепочке хеш таблице
  struct symbol_s *next;
  // указатель на объект - значение символа 
  struct object_s *value;
  //указатель на функцию для примитивов
  func_t func;
} symbol_t;

symbol_t *find_symbol(char *str);
void register_func(char *name, func_t func_ptr);
#endif

