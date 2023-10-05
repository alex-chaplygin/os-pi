#ifndef OBJECTS
#define OBJECTS

/// Всего объектов
#define MAX_OBJECTS 200
/// Всего пар
#define MAX_PAIRS 100
/// Всего символов
#define MAX_SYMBOLS 100

#define MAX_STR 50
#define PRINT(o) print_obj(o); printf("\n");

/// перечисление типов объектов
typedef enum {
  NUMBER, /// целое число
  SYMBOL, ///символ
  PAIR    ///пара
} type_t;

struct pair_s;
struct symbol_s;

typedef struct object_s
{
    type_t type; // тип объекта
    union {
        int value; // если объект число, то его значение
        struct symbol_s *symbol; // указатель на символ
        struct pair_s *pair; // Если объект пара из 2-х объектов (левый и правый) то указатель на пару
    } u;
    struct object_s *next; // указатель на следующий свободный объект
    int mark; // пометка для сборки мусора
} object_t; // Структура объекта

/// Структура пары
typedef struct pair_s
{
    object_t *left; // левый элемент
    object_t *right; // правый элемент
    struct pair_s *next; // указатель на следующую свободную пару
} pair_t;

typedef  object_t *(*func_t)(object_t *);

//структура символа
typedef struct symbol_s
{
  //имя символа
  char str[MAX_STR];
  //указатель на следующий символ в цепочке хеш таблице
  struct symbol_s *next;
  // указатель на объект - значение символа 
  object_t *value;
  //указатель на функцию для примитивов
  func_t func;
} symbol_t;


object_t *object_new(type_t type, void *data);
object_t *new_pair(object_t *left, object_t *right);
struct symbol_s *new_symbol(char *str);
void print_obj(object_t *obj);
void free_object(object_t *obj);
void free_pair(pair_t *p);
void print_free_objs();
#endif
