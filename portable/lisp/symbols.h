#ifndef OBJECTS
#define OBJECTS
#include "objects.h"
//максимальная длина имени символа
#define MAX_STR 50

struct object_s;

//структура символа
typedef struct symbol_s
{
  //имя символа
  char str[MAX_STR];
  //указатель на следующий символ в цепочке хеш таблице
  struct symbol_s *next;
  // указатель на объект - значение символа 
  struct object_s *value;
} symbol_t;

symbol_t *find_symbol(char *str);
#endif

