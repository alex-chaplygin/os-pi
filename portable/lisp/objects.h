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
/// Всего больших чисел
#define MAX_NUMBERS 1000

#define PRINT(o) print_obj(o); printf("\n");
#define ERROR (-1)
#define NULLOBJ 3
/// Отсутствие значения у переменных
#define NOVALUE (ERROR)
/// Метка региона
#define MAGIC 0xABCD1234
/// Число бит на тип объекта
#define TYPE_BITS 3
/// Номер бита пометки для сборщика мусора
#define MARK_BIT (TYPE_BITS + 1)
/// Число бит в адресе
#define ADDR_BITS 28
/// Возвращение типа объекта в младших битах
#define TYPE(obj) ((obj) & ((1 << TYPE_BITS) - 1))
/// Установить бит пометки
#define SET_MARK(obj) ((obj) |= (1 << TYPE_BITS))
/// Возврат значения бита пометки
#define GET_MARK(obj) (((obj) >> TYPE_BITS) & 1)
/// Очистка бита пометки
#define CLEAR_MARK(obj) ((obj) &= ~(1 << TYPE_BITS))
/// Получить адрес объекта
#define GET_ADDR(obj) ((obj) >> (TYPE_BITS + 1))
//макрос, который строит указатель, состоящий из типа в младших битах и значения в остальных
#define NEW_OBJECT(type, val)  ((object_t )((((unsigned int)val) << (TYPE_BITS + 1)) | (type)))

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
    BIGNUMBER, /// большое целое число
    SYMBOL, ///символ
    PAIR,    ///пара
    STRING,  ///строка
    ARRAY, ///массив
} type_t;

/// Тип для объекта
typedef unsigned int object_t;  

/// Структура большого целого числа
typedef struct bignumber_s
{
    int value; // значение числа
    struct bignumber_s *next; // указатель на следующее свободное число
    int free; // Если 1 - число свободно
} bignumber_t;

/// Структура пары
typedef struct pair_s
{
    object_t left; // левый элемент (элемент списка)
    object_t right; // правый элемент (следующая пара)
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
    object_t *data; // Данные массива
    int length; // Длина массива
    struct array_s *next; // Указатель на следующий свободный массив
    int free; // Если 1 - массив свободен
} array_t;

typedef  object_t (*func_t)(object_t);

//структура символа
typedef struct symbol_s
{
    //имя символа
    char str[MAX_STR];
    //указатель на следующий символ в цепочке хеш таблице
    struct symbol_s *next;
    // указатель на объект - значение переменной
    object_t value;
    // указатель на объект для функций (lambda выражение)
    object_t lambda;
    // указатель на объект для макроса
    object_t macro;
    //указатель на функцию для примитивов
    func_t func;
} symbol_t;

extern int print_counter;

object_t new_bignumber(int num);
object_t new_number(int num);
int get_value(object_t obj);
object_t new_pair(object_t left, object_t right);
struct symbol_s *new_symbol(char *str);
void garbage_collect();
object_t *dump_free(object_t *);
void print_obj(object_t *obj);
void free_object(object_t *obj);
void free_bignumber(bignumber_t *o);
void free_pair(pair_t *p);
void print_free_objs();
void print_free_pairs();
void init_regions();
void *alloc_region(int size);
void free_region(void *data);
string_t *new_string(char *str);
void free_string(string_t *s);
array_t *new_array(object_t list);
array_t *new_empty_array(int length);
void free_array(array_t *a);
#endif
