#ifndef OBJECTS
#define OBJECTS

/// Всего пар
#define MAX_PAIRS 800000
/// Всего символов
#define MAX_SYMBOLS 1000
/// Всего строк
#define MAX_STRINGS 20000
/// Всего массивов
#define MAX_ARRAYS 200
/// Максимальная длина символа
#define MAX_SYM_STR 32
/// Всего больших чисел
#define MAX_NUMBERS 350
/// Всего вещественных чисел
#define MAX_FLOATS 350
/// Всего продолжений
#define MAX_CONTINUATIONS 100
/// Всего функций 
#define MAX_FUNCTIONS 100

/// Печать объекта с переводом строки и учетом рекурсии
#define PRINT(o) print_counter++; print_obj(o); printf("\n");

#define ERROR (object_t)(-1)
/// Объект пустой список(пустой объект)
#define NULLOBJ PAIR 
/// Отсутствие значения у переменных
#define NOVALUE (0xF)
/// Число бит на тип объекта
#define TYPE_BITS 4
/// Номер бита пометки для сборщика мусора
#define MARK_BIT (TYPE_BITS + 1)
/// Число бит в адресе
#define ADDR_BITS (32 - MARK_BIT)
/// Возвращение типа объекта в младших битах
#define TYPE(obj) ((obj) & ((1 << TYPE_BITS) - 1))
/// Установить бит пометки
#define SET_MARK(obj) ((obj) |= (1 << TYPE_BITS))
/// Возврат значения бита пометки
#define GET_MARK(obj) (((obj) >> TYPE_BITS) & 1)
/// Очистка бита пометки
#define CLEAR_MARK(obj) ((obj) &= ~(1 << TYPE_BITS))
/// Получить адрес объекта
#ifdef X32
#define GET_ADDR(obj) ((obj) & (0xFFFFFFFF << MARK_BIT))
#else
#define GET_ADDR(obj) ((obj) & (0xFFFFFFFFFFFFFFFF << MARK_BIT))
#endif
//макрос, который строит указатель, состоящий из типа в младших битах и значения в остальных
#define NEW_OBJECT(type, val) ((object_t)(val) + (type))
//Получение указателя на структуру пары из объекта
#define GET_PAIR(obj) ((pair_t *)(GET_ADDR(obj)))
//Получение указателя на структуру 32-х битного числа
#define GET_BIGNUMBER(num) ((bignumber_t *)(GET_ADDR(num)))
//Получение указателя на структуру функции из объекта
#define GET_FUNCTION(obj) ((function_t *)(GET_ADDR(obj)))
//Получение указателя на структуру вещественного числа
#define GET_FLOAT(num) ((float_t *)(GET_ADDR(num)))
//Получение указателя на структуру строки
#define GET_STRING(str) ((string_t *)(GET_ADDR(str)))
//Получение указателя на структуру символа
#define GET_SYMBOL(sym) ((symbol_t *)(GET_ADDR(sym)))
//Получение указателя на структуру массива
#define GET_ARRAY(arr) ((array_t *)(GET_ADDR(arr)))
//Получение одиночного символа
#define GET_CHAR(obj) ((obj) >> TYPE_BITS + 1)
//Создание массива
#define NEW_ARRAY(a) (NEW_OBJECT(ARRAY, new_array(a)))
//Создание символа
#define NEW_SYMBOL(s) (NEW_OBJECT(SYMBOL, find_symbol(s)))
//Создание строки
#define NEW_STRING(s) (NEW_OBJECT(STRING, new_string(s)))
//Создание одиночного символа
#define NEW_CHAR(s) (NEW_OBJECT(CHAR, ((s) << TYPE_BITS + 1)))
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
    FLOAT, /// число с плавающей точкой
    SYMBOL, ///символ
    PAIR,    ///пара
    STRING,  ///строка
    ARRAY, ///массив
    CHAR, ///одиночный символ
    FUNCTION, /// lambda функция
    CONTITUATION, ///продолжение
} type_t;
/// Тип для объекта
#ifndef X32
typedef long long object_t;
#else
typedef unsigned int object_t;
#endif

typedef  object_t (*func_t)(object_t); // указатель на функцию примитив

/* Структуры объектов должны иметь размер, кратный 2^MARKBIT (сейчас 32 байт) */
/* Структура региона должна иметь размер, кратный 2^MARKBIT (сейчас 32 байт) */

/// Структура большого целого числа
typedef struct bignumber_s
{
    int value; // значение числа
    struct bignumber_s *next; // указатель на следующее свободное число
    int free; // Если 1 - число свободно
#ifdef X32
    int pad[5]; // выравнивание 12 + 20
#else
    int pad[2]; // выравнивание 24 + 8
#endif
} bignumber_t;

/// Структура функции
typedef struct function_s
{
    object_t args; // аргументы функции
    object_t body; // тело функции
    func_t func; // указатель на функцию примитив, если = NULL, то функция пользовательская, иначе встроенная
    object_t env; // окружение для переменных
    object_t func_env; // окружение для функций
    struct function_s *next;// указатель на следующую свободную функцию
    int free; // Если 1 - функция свободна
#ifdef X32
    int pad; // выравнивание 28 + 4
#else
    int pad[2]; // выравнивание 56 + 8
#endif
} function_t;

typedef struct float_s
{
    float value; // значение вещественного числа
    struct float_s *next; // указатель на следующее свободное число
    int free; // Если число свободно
#ifdef X32
    int pad[5]; // выравнивание 12 + 20
#else
    int pad[2]; // выравнивание 24 + 8
#endif
} float_t;

/// Структура пары
typedef struct pair_s
{
    object_t left; // левый элемент (элемент списка)
    object_t right; // правый элемент (следующая пара)
    struct pair_s *next; // указатель на следующую свободную пару
    int free; // Если 1 - пара свободна
    int print_counter; // счетчик печати
#ifdef X32
    int pad[3]; // выравнивание 20 + 12
#else
    int pad[8]; // выравнивание 40 + 24
#endif
} pair_t;

/// Структура строки
typedef struct string_s
{
    char *data; //данные строки
    int length; //длина строки
    struct string_s *next; //указатель на следующую свободную строку
    int free; // Если 1 - строка свободна
#ifdef X32
    int pad[4]; // выравнивание 16 + 16
#endif
} string_t; //структура строки

/// Структура массива
typedef struct array_s
{
    object_t *data; // Данные массива
    int length; // Длина массива
    struct array_s *next; // Указатель на следующий свободный массив
    int free; // Если 1 - массив свободен
#ifdef X32
    int pad[4]; // выравнивание 16 + 16
#endif
} array_t;


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
    //список функций у метки
    object_t tag_value;
#ifdef X32
    int pad[2]; // выравнивание 32 + 24 + 8
#else
    int pad[4]; // выравнивание 32 + 48 + 16
#endif
} symbol_t;

extern int print_counter;

void init_objects();
object_t new_bignumber(int num);
object_t new_number(int num);
object_t new_float(float nuw);
object_t new_function(object_t args, object_t body, object_t env, object_t func_env);
object_t new_prim_function(func_t func);
int get_value(object_t obj);
object_t new_pair(object_t left, object_t right);
struct symbol_s *new_symbol(char *str);
void garbage_collect();
object_t *dump_free(object_t *);
void print_obj(object_t obj);
void free_object(object_t *obj);
void free_bignumber(bignumber_t *o);
void free_float(float_t *f);
void free_pair(pair_t *p);
void free_function(function_t *f);
void print_free_objs();
void print_free_pairs();
string_t *new_string(char *str);
void free_string(string_t *s);
array_t *new_array(object_t list);
array_t *new_empty_array(int length);
void free_array(array_t *a);
object_t print_gc_stat(object_t o);
int need_grabage_collect();
void mark_object(object_t obj);
#endif
