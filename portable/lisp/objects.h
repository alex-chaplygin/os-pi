#ifndef OBJECTS
#define OBJECTS

/// Всего объектов
#define MAX_OBJECTS 200
/// Всего пар
#define MAX_PAIRS 200
/// Всего символов
#define MAX_SYMBOLS 100
/// Всего байт для регионов 
#define MAX_CHARS 200

#define MAX_STR 50
#define PRINT(o) print_obj(o); printf("\n");

/// перечисление типов объектов
typedef enum {
    NUMBER, /// целое число
    SYMBOL, ///символ
    PAIR,    ///пара
    STRING   ///строка
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
	char *str; // если объект строка
    } u;
    struct object_s *next; // указатель на следующий свободный объект
    int mark; // пометка для сборки мусора
    int free; // Если 1 - элемент в списке свободных объектов
} object_t; // Структура объекта

/// Структура пары
typedef struct pair_s
{
    object_t *left; // левый элемент (элемент списка)
    object_t *right; // правый элемент (следующая пара)
    struct pair_s *next; // указатель на следующую свободную пару
    int free; // Если 1 - пара свободна
} pair_t;

typedef  object_t *(*func_t)(object_t *);

//структура символа
typedef struct symbol_s
{
    //имя символа
    char str[MAX_STR];
    //указатель на следующий символ в цепочке хеш таблице
    struct symbol_s *next;
    // указатель на объект - значение переменной
    object_t *value;
    // указатель на объект для функций (lambda выражение)
    object_t *lambda;
    //указатель на функцию для примитивов
    func_t func;
} symbol_t;


object_t *object_new(type_t type, void *data);
object_t *new_pair(object_t *left, object_t *right);
struct symbol_s *new_symbol(char *str);
void garbage_collect();
object_t *dump_free(object_t *);
void print_obj(object_t *obj);
void free_object(object_t *obj);
void free_pair(pair_t *p);
void print_free_objs();
void print_free_pairs();
void init_regions();
void *alloc_region(int size);
#endif
