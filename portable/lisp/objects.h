#ifndef OBJECTS
#define OBJECTS

/// Всего объектов
#define MAX_OBJECTS 2000000
/// Всего пар
#define MAX_PAIRS 2000000
/// Всего символов
#define MAX_SYMBOLS 2000
/// Всего байт для регионов 
#define MAX_REGION_SIZE 500000
/// Всего строк
#define MAX_STRINGS 2000
/// Всего массивов
#define MAX_ARRAYS 100
/// Максимальная длина символа
#define MAX_STR 500

#define PRINT(o) print_obj(o); printf("\n");

/// Отсутствие значения у переменных
#define NOVALUE ((void *)1)
/// Метка региона
#define MAGIC 0xABCD1234

#pragma pack(4)
/// создаваемый или свободный регион памяти
struct region {
    int magic; /// метка своих регионов
    int free; /// свободен ли регион
    struct region *next; /// указатель на следующий регион
    struct region *prev; /// указатель на предыдущий регион
    int size; /// размер региона в байтах
    char data[1]; /// Данные региона
};

/// перечисление типов объектов
typedef enum {
    NUMBER, /// целое число
    SYMBOL, ///символ
    PAIR,    ///пара
    STRING,  ///строка
    ARRAY, ///массив
} type_t;

struct pair_s;
struct symbol_s;
struct string_s;
struct array_s;

typedef struct object_s
{
    type_t type; // тип объекта
    union {
        int value; // если объект число, то его значение
        struct symbol_s *symbol; // указатель на символ
        struct pair_s *pair; // Если объект пара из 2-х объектов (левый и правый) то указатель на пару
	struct string_s *str; // если объект строка
	struct array_s *arr; // Если объект массив
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
    int print_counter; // счетчик печати
} pair_t;

/// Структура строки
typedef struct string_s
{
    char *data; //данные строки
    int length; //длина строки
    struct string_s *next; //указатель на следующую свободную строку
    int free; // Если 1 - строка свободна
} string_t; //структура строки

/// Структура массива
typedef struct array_s
{
    object_t **data; // Данные массива
    int length; // Длина массива
    struct array_s *next; // Указатель на следующий свободный массив
    int free; // Если 1 - массив свободен
} array_t;

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
    // указатель на объект для макроса
    object_t *macro;
    //указатель на функцию для примитивов
    func_t func;
} symbol_t;

extern int print_counter;
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
void free_region(void *data);
string_t *new_string(char *str);
void free_string(string_t *s);
array_t *new_array(object_t *list);
array_t *new_empty_array(int length);
void free_array(array_t *a);
#endif
