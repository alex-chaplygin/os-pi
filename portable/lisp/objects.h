#ifndef OBJECTS
#define OBJECTS

/// Всего пар
#define MAX_PAIRS 600000
/// Всего символов
#define MAX_SYMBOLS 500
/// Всего строк
#define MAX_STRINGS 10000
/// Всего массивов
#define MAX_ARRAYS 200
/// Максимальная длина символа
#define MAX_SYM_STR 32
/// Всего больших чисел
#define MAX_NUMBERS 50

#define PRINT(o) print_counter++; print_obj(o); printf("\n");
#define ERROR (object_t)(-1)
#define NULLOBJ 3
/// Отсутствие значения у переменных
#define NOVALUE (0xF)
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
#define GET_ADDR(obj) (((obj) >> MARK_BIT) << MARK_BIT)
//макрос, который строит указатель, состоящий из типа в младших битах и значения в остальных
#define NEW_OBJECT(type, val) ((object_t)(val) + (type))
//Получение пары из объекта
#define GET_PAIR(obj) ((pair_t *)(GET_ADDR(obj)))
//Получение 32-х битного числа
#define GET_BIGNUMBER(num) ((bignumber_t *)(GET_ADDR(num)))
//Получение строки
#define GET_STRING(str) ((string_t *)(GET_ADDR(str)))
//Получение символа
#define GET_SYMBOL(sym) ((symbol_t *)(GET_ADDR(sym)))
//Получение массива
#define GET_ARRAY(arr) ((array_t *)(GET_ADDR(arr)))
//Создание массива
#define NEW_ARRAY(a) (NEW_OBJECT(ARRAY, new_array(a)))
//Создание символа
#define NEW_SYMBOL(s) (NEW_OBJECT(SYMBOL, find_symbol(s)))
//Создание строки
#define NEW_STRING(s) (NEW_OBJECT(STRING, new_string(s)))
// Первый элемент списка
#define FIRST(o) (GET_PAIR(o)->left)
// Второй элемент списка
#define SECOND(o) (GET_PAIR(GET_PAIR(o)->right)->left)
// Третий элемент списка
#define THIRD(o) (GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->right)->left)
// Хвост списка
#define TAIL(o) (GET_PAIR(o)->right)
// Проверка объекта на число
#define IS_NUMBER(o) (TYPE(o) == NUMBER || TYPE(o) == BIGNUMBER)

/// перечисление типов объектов
typedef enum {
    NUMBER, /// целое число, которое умещается в ADDR_BITS
    BIGNUMBER, /// целое число - 32 бита
    SYMBOL, ///символ
    PAIR,    ///пара
    STRING,  ///строка
    ARRAY, ///массив
} type_t;
/// Тип для объекта
#ifndef X32
typedef long long object_t;
#else
typedef unsigned int object_t;
#endif

/// Структура большого целого числа
typedef struct bignumber_s
{
    int value; // значение числа
    struct bignumber_s *next; // указатель на следующее свободное число
    int free; // Если 1 - число свободно
#ifdef X32
    int pad;
#endif
} bignumber_t;

/// Структура пары
typedef struct pair_s
{
    object_t left; // левый элемент (элемент списка)
    object_t right; // правый элемент (следующая пара)
    struct pair_s *next; // указатель на следующую свободную пару
    int free; // Если 1 - пара свободна
    int print_counter; // счетчик печати
#ifdef X32
    int pad[3];
#endif
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
    char str[MAX_SYM_STR];
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
#ifdef X32
    int pad[3];
#else
    int pad[2];
#endif
} symbol_t;

extern int print_counter;

void init_objects();
object_t new_bignumber(int num);
object_t new_number(int num);
int get_value(object_t obj);
object_t new_pair(object_t left, object_t right);
struct symbol_s *new_symbol(char *str);
void garbage_collect();
object_t *dump_free(object_t *);
void print_obj(object_t obj);
void free_object(object_t *obj);
void free_bignumber(bignumber_t *o);
void free_pair(pair_t *p);
void print_free_objs();
void print_free_pairs();
string_t *new_string(char *str);
void free_string(string_t *s);
array_t *new_array(object_t list);
array_t *new_empty_array(int length);
void free_array(array_t *a);
object_t print_gc_stat(object_t o);
#endif
