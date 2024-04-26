#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <setjmp.h>
#include "test.h"
#include "objects.h"
#include "parser.h"
#include "alloc.h"

extern bignumber_t *bignumbers;
extern bignumber_t *free_bignumbers;
extern pair_t *pairs;
extern string_t *strings;
extern array_t *arrays;
extern int last_bignumber;
extern int last_pair;
extern int last_symbol;
extern pair_t *free_pairs;
extern char *region_data;
extern string_t *free_strings;
extern array_t *free_arrays;
extern struct region *regions;
extern int last_string;
extern int last_array;

void mark_object(object_t obj);
void sweep();
void garbage_collect();
void free_string(string_t *s);
object_t make_list(int num);

jmp_buf jmp_env;

void error(char *str, ...)
{
    printf("%s\n", str);
    longjmp(jmp_env, 1);
}

symbol_t *find_symbol(char *str)
{
    return NULL;
}

/**
 * Создать объект большое число и проверить, что оно правильно записалось
 */
void test_new_bignumber(int number)
{
    printf("test_new_bignumber: ");
    object_t bn = new_bignumber(number);
    ASSERT(GET_BIGNUMBER(bn)->value, number);
    //    ASSERT(GET_ADDR(bignumbers[last_number - 1].value), GET_ADDR(number));
}


/**
 * Создать объект маленькое число и проверить, что оно правильно записалось
 */
void test_new_number(int number, int type)
{
    printf("test_new_number: ");
    object_t o = new_number(number);
    ASSERT(TYPE(o), type);
    ASSERT(get_value(o), number);
}

/**
 * Создать объект маленькое число и проверить получение значения
 */
void test_get_value(int number)
{
    printf("test_get_value: ");
    object_t o = new_number(number);
    ASSERT(get_value(o), number);
}

/**
 * Создать две пары и проверить массив пар
 */
void test_new_pair()
{
    object_t o1,o2,o3,o4;
    o1 = new_number(1);
    o2 = new_number(2);
    o3 = new_number(3);
    o4 = new_number(4);
    
    printf("test_new_pair: ");
    object_t p1 = new_pair(o1, o2);
    object_t p2 = new_pair(o3, o4);
    //    printf("p1 = %x, p2 = %x pairs = %x\n", p1, p2, pairs);
    ASSERT(GET_PAIR(p1)->left, o1);
    ASSERT(GET_PAIR(p1)->right, o2);
    ASSERT(GET_PAIR(p2)->left, o3);
    ASSERT(GET_PAIR(p2)->right, o4);
    free_pair(GET_PAIR(p1));
    free_pair(GET_PAIR(p2));
}

/**
 * Проверка правильной печати объекта (1 2)
 */
void test_print_obj(object_t obj, const char *expected_output)
{
    printf("test_print_obj: ");
    
    int outdes = dup(1);
    FILE *file = freopen("/tmp/test.txt", "w", stdout);
    print_counter++;
    print_obj(obj);
    fclose(file);

    stdout = fdopen(outdes, "w");
    
    FILE *output_file = fopen("/tmp/test.txt", "r");
    char output_buffer[20];
    fgets(output_buffer, sizeof(output_buffer), output_file);
    fclose(output_file);

    ASSERT(strcmp(output_buffer, expected_output), 0);
}

/**
 * Сброс памяти в начальное состояние
 *
 */
void reset_mem()
{
    last_symbol = 0;
    last_bignumber = 0;
    last_pair = 0;
    last_string = 0;
    last_array = 0;
    free_pairs = NULL;
    free_strings = NULL;
    free_bignumbers = NULL;
    free_arrays = NULL;
}

/** 
 *Освобождение большого числа из объекта
 */

void test_free_bignumber()
{
    printf("test_free_bignumber: ");
    reset_mem();
    object_t o = new_number(800000000);
    printf("%x\n", o);
    ASSERT(TYPE(o), BIGNUMBER);
    free_bignumber((bignumber_t *)GET_ADDR(o));
    ASSERT(free_bignumbers, (bignumber_t *)GET_ADDR(o));
}

/**
 * Проверка корректности работы сборщика мусора
 * относительно больших чисел.
 * Заполнить хранилище больших чисел, освободить два объекта,
 * создать два больших числа, проверить
 * что адреса новых и освобождённых идентичны
 */
void test_free_bignumber2()
{
    printf("test_free_bignumber2: ");
    reset_mem();
    for (int i = last_bignumber; i < MAX_NUMBERS; i++)
        new_bignumber(i);
    bignumber_t *o1 = &bignumbers[0];
    bignumber_t *o2 = &bignumbers[5];
    free_bignumber(&bignumbers[0]);
    free_bignumber(&bignumbers[5]);
    ASSERT(free_bignumbers, &bignumbers[5]);
    ASSERT(free_bignumbers->next, &bignumbers[0]);
    ASSERT(free_bignumbers->next->next, NULL);
    object_t r_o1 = new_bignumber(1);
    object_t r_o2 = new_bignumber(2);
    ASSERT(o2, GET_BIGNUMBER(r_o1));
    ASSERT(o1, GET_BIGNUMBER(r_o2));
}

/**
 * Проверка корректности работы сборщика мусора
 * относительно массивов.
 * Заполнить хранилище массивов, освободить два объекта,
 * создать два массива, проверить
 * что адреса новых и освобождённых идентичны
 */
void test_free_array()
{
    printf("test_free_array: ");
    reset_mem();
    
    for (int i = last_array; i < MAX_ARRAYS; i++)
        new_array(make_list(2));

    array_t *a1 = &arrays[0];
    array_t *a2 = &arrays[5];
    free_array(a1);
    free_array(a2);
    ASSERT(free_arrays, a2);
    ASSERT(free_arrays->next, a1);
    ASSERT(free_arrays->next->next, NULL);
    array_t *a_1 = new_array(make_list(3));
    array_t *a_2 = new_array(make_list(3));
    ASSERT(a2, a_1);
    ASSERT(a1, a_2);
}

/**
 * Проверка корректности функции освобождения пары
 *  объектов и их дальнейшего переиспользования
 */
void test_free_pair()
{
    printf("test_free_pair: ");
    reset_mem();
    int last_num = last_pair;
    object_t o1,o2;
    for (int i = last_pair; i < MAX_PAIRS; i++)
    {
        o1 = new_number(i);
	o2 = new_number(i-1);
	new_pair(o1, o2);
    }
    pair_t *pair1 = &pairs[0];
    pair_t *pair2 = &pairs[1];
    free_pair(pair1);
    //    printf("pair1 = %x, pair2 = %x\n", pair1, pair2);
    //free_pair(pair2);
    ASSERT(free_pairs, pair1);
    ASSERT(free_pairs->next, NULL);
    /* ASSERT(free_pairs->next->next, NULL); */
}

 /**
  * Создание нового символа и проверка его
  */
void test_new_symbol() 
{
    printf("test_new_symbol: ");
    symbol_t *s = new_symbol("abc");
    ASSERT(strcmp(s->str, "abc"), 0);
}

object_t p1;

/**
 * Создать объект (12 A 5)
 */
void test_mark()
{
    printf("test_mark :");
    reset_mem();
    int mask = 1 << 31;
    object_t n = new_bignumber(2147483658);
    object_t s = NEW_STRING("abc");
    object_t a = NEW_ARRAY(make_list(3));	
    object_t inp2 = new_pair(a, NULLOBJ);
    object_t inp1 = new_pair(s, inp2);
    object_t p2 = new_pair(inp1, NULLOBJ);
    p1 = new_pair(n, p2);
    printf("p1="); PRINT(p1);
    mark_object(p1);
    ASSERT(GET_MARK(GET_PAIR(inp1)->left), 1);
    ASSERT((GET_STRING(GET_PAIR(inp1)->left)->length) & mask, mask);
    ASSERT(GET_MARK(GET_PAIR(inp2)->left), 1);
    ASSERT((GET_ARRAY(GET_PAIR(inp2)->left)->length) & mask, mask);
    ASSERT(GET_MARK(GET_PAIR(p1)->left), 1);
    ASSERT((GET_BIGNUMBER(GET_PAIR(p1)->left)->free) & mask, mask);
    ASSERT(GET_MARK(GET_PAIR(p2)->left), 1);
}

/*
 * Используем помеченные объекты с предыдущего теста.
 * Проверить снятие пометок
 * Проверить, что помеченные ранее объекты не находятся в списке свободных
 */
void test_sweep() 
{ 
    printf("test_sweep: "); 
    sweep();
    int mask = 1 << 31;
    object_t n = GET_PAIR(p1)->left;
    object_t p2 = GET_PAIR(p1)->right;
    object_t inp1 = GET_PAIR(p2)->left;
    object_t s = GET_PAIR(inp1)->left;
    object_t inp2 = GET_PAIR(inp1)->right;
    object_t a = GET_PAIR(inp2)->right;
    ASSERT(GET_MARK(GET_PAIR(inp1)->left), 0);
    ASSERT((GET_STRING(GET_PAIR(inp1)->left)->length) & mask, 0);
    ASSERT(GET_MARK(GET_PAIR(inp2)->left), 0);
    ASSERT((GET_ARRAY(GET_PAIR(inp2)->left)->length) & mask, 0);
    ASSERT(GET_MARK(GET_PAIR(p1)->left), 0);
    ASSERT((GET_BIGNUMBER(GET_PAIR(p1)->left)->free) & mask, 0);
    ASSERT(GET_MARK(GET_PAIR(p2)->left), 0);
} 

/*
 * Создать символ A
 * Присвоить ему значение - объект 52
 * Проверить, что объект не в списке свободных
 */
void test_garbage_collect()
{
    printf("test_garbage_collect: ");

    int num1 = 52;
    symbol_t *s = new_symbol("A");
    object_t obj1 = new_bignumber(num1);
    s->value = obj1;
    garbage_collect();

    // Проверяем наличие obj1 в списке свободных объектов
    bignumber_t *f = free_bignumbers;
    while (f != NULL) {
        if ((object_t)f == obj1) {
            printf("fail_object\n");
            return;
        }
        f = f->next;
    }
    printf("OK\n");
}



/*
 * Создать символ B
 * Присвоить ему значение - объект (1 2)
 * Проверить, что объект не в списке свободных
 */
void test_garbage_collect_list()
{
    printf("test_garbage_collect_list: ");
    symbol_t *s = new_symbol("B");
    object_t obj1 = new_number(1);
    object_t obj2 = new_number(2);
    object_t p1 = new_pair(obj1, new_pair(obj2, NULLOBJ));
    s->value = p1;
    printf("\nbefore gc: ");
    print_obj(free_pairs);
    garbage_collect();
    printf("\nafter gc: ");
    print_obj(free_pairs);
    pair_t *p = free_pairs;
    while (p != NULL)
    {
        if (p == GET_PAIR(p1) || p == GET_PAIR(GET_PAIR(p1)->right))
        {
            printf("\nfail_pair\n");
            return;
        }
        p = p->next;
    }
    printf("\npairs: OK\n");
}

/*
 * Создать две п
 * Присвоить левым частям этих пар - числа, а правые ссылались друг на друга
 * Создать два символа, которые ссылаются на эти пары
 * Проверить работу сборщика мусора, у пар поле free должно быть равно нулю.
 */
void test_garbage_collect_cycle()
{
    printf("test_garbage_collect_cycle: ");
    fflush(stdout);
    int num1 = 1;
    symbol_t *s1 = new_symbol("A");
    symbol_t *s2 = new_symbol("B");
    object_t p1 = new_pair(new_number(1), NULLOBJ);
    object_t p2 = new_pair(new_number(2), NULLOBJ);
    GET_PAIR(p1)->right = p2;
    GET_PAIR(p2)->right = p1;
    s1->value = p1;
    s2->value = p2;
    garbage_collect();
    ASSERT(GET_PAIR(p1)->free, 0);
    ASSERT(GET_PAIR(p2)->free, 0);
}
	
/**
 * Выделить 2 региона с размерами 5 и 64
 * Проверить указатели регионов относительно начала
 * Проверить указатели данных
 */
void test_alloc_region()
{
    printf("test_alloc_region: ");
    char *reg1 = alloc_region(5);
    char *reg2 = alloc_region(64);
    int size = sizeof(struct region) - 4;
    ASSERT((int)reg1 & 0xf, 0);
    ASSERT((int)reg2 & 0xf, 0);
    //    ASSERT((reg1 - region_data), size);
    //    ASSERT((reg2 - region_data), size + 16 + size);
    free_region(reg1);
    free_region(reg2);
}

/**
 * Создать 3 региона
 * Удалить второй и первый регион
 * Проверить объединённый регион
 * Освободить третий регион
 */
void test_free_region()
{
    printf("test_free_region: ");
    char *reg1 = alloc_region(8);
    char *reg2 = alloc_region(12);
    char *reg3 = alloc_region(16);
    char *reg4 = alloc_region(10);
    int offset = sizeof(struct region) - 4;
    struct region *r1 = (struct region *)(reg1 - offset);
    struct region *r2 = (struct region *)(reg2 - offset);
    struct region *r3 = (struct region *)(reg3 - offset);
    ASSERT((int)reg1 & 0xf, 0);
    ASSERT((int)reg2 & 0xf, 0);
    ASSERT((int)reg3 & 0xf, 0);
    ASSERT((int)reg4 & 0xf, 0); 
    free_region(reg2);
    ASSERT(r1->next, r2);
    ASSERT(r2->prev, r1);
    free_region(reg1);
    ASSERT(r1->next, r3);
    ASSERT(r3->prev, r1);
    free_region(reg3);
}

/*
 * Проверка на переполнение хранилищ объектов разных
 * типов: bignumber, pair, symbol, string, array
 */
void test_objects_new_null()
{
    printf("test_objects_new_null: ");
    reset_mem();
    int i;
    if (setjmp(jmp_env) == 0) {
        for (i = 0; i < MAX_NUMBERS; i++)
            new_bignumber(i);
        object_t bn = new_bignumber(i);
        FAIL;
    } else 
        OK;
    reset_mem();
    if (setjmp(jmp_env) == 0) {
        for (i = last_pair; i < MAX_PAIRS; i++)
            new_pair(new_number(i), NULLOBJ);
        object_t p = new_pair(new_number(1), NULLOBJ);
        FAIL;
    } else 
        OK;
    reset_mem();
    if (setjmp(jmp_env) == 0) {
        for (i = last_symbol; i < MAX_SYMBOLS; i++)
            new_symbol("s");
        symbol_t *s = new_symbol("s");
        FAIL;
    } else 
        OK;
    reset_mem();
    if (setjmp(jmp_env) == 0) {
        for (i = last_string; i < MAX_STRINGS; i++)
            new_string("s");
        string_t *str = new_string("s");
        FAIL;
    } else 
        OK;
    reset_mem();
    if (setjmp(jmp_env) == 0) {
        for (i = last_array; i < MAX_ARRAYS; i++)
            new_array(make_list(2));
        array_t *arr = new_array("s");
        FAIL;
    } else 
	OK;
}

/**
 * Создать новую строку
 * Проверить длину строи и данные
 */
void test_new_string()
{
    printf("test_new_string: ");
    string_t *s = new_string("abc");
    ASSERT(s->length, 3);
    ASSERT(strcmp(s->data, "abc"), 0);
}

/*
 * Освободить пустую пару
 */
void test_free_pair_empty()
{
    printf("test_free_pair_empty: ");
    if (setjmp(jmp_env) == 0) {
	free_pair(NULL);
        FAIL;
    } else 
        OK;
}

/**
 * Проверка корректности функции освобождения строки
 * Создать строку, освободить её, проверить список свбодных строк
 */
void test_free_string()
{
    printf("test_free_string: ");
    reset_mem();
    string_t *str = new_string("ffffff");
    free_string(str);
    ASSERT(free_strings, str);
    ASSERT(free_strings->next, NULL);
}

/*
 * Создать символ B
 * Присвоить ему значение - строка "abc"
 * Создать ещё две строки
 * Выполнить сборку мусора
 * Проверить, что объект не в списке свободных строк
 */
void test_garbage_collect_strings() 
{ 
    printf("test_garbage_collect_strings: "); 
    reset_mem(); 
    symbol_t *s = new_symbol("B"); 
    string_t *obj1 = new_string("abc"); 
    string_t *obj2 = new_string("ff"); 
    string_t *obj3 = new_string("cc"); 
    s->value = NEW_OBJECT(STRING, obj1);
    garbage_collect(); 
    string_t *fs = free_strings; 
    while (fs != NULL) { 
	printf("%s->", fs->data); 
	if (!strcmp(fs->data, "abc")) { 
	    printf("fail_string\n"); 
	    return; 
	} 
	fs = fs->next; 
    } 
    printf("strings_OK\n"); 
    ASSERT(regions->free, 0); 
    ASSERT((regions->next != NULL), 1); 
}

/**
 * Создать список с числом элементов num
 *
 */
object_t make_list(int num)
{
    object_t o = new_number(num);
    if (num == 1)
	return new_pair(o, NULLOBJ);
    return new_pair(o, make_list(num - 1));
}

/**
 *@param length = 8
 */
void test_new_empty_array()
{
    printf("test_new_empty_array: ");
    int length = 8;
    object_t arr = new_empty_array(length);
    for(int i = 0; i < length; i++) 
	ASSERT(GET_ARRAY(arr)->data[i], NULLOBJ);	   
}


/*
 * Создать символ B
 * Присвоить ему значение - массив из трех элементов
 * Создать ещё два массива - 10 элементов и 20 элементов
 * Выполнить сборку мусора
 * Проверить, что объект не в списке свободных массивов
 */
void test_garbage_collect_arrays()
{
    printf("test_garbage_collect_arrays: ");
    reset_mem();
    symbol_t *s = new_symbol("B");
    array_t *obj1 = new_array(make_list(3));
    array_t *obj2 = new_array(make_list(10));
    array_t *obj3 = new_array(make_list(20));
    s->value = NEW_OBJECT(ARRAY, obj1);
    garbage_collect();
    array_t *fa = free_arrays;
    while (fa != NULL)
    {
        if (fa == GET_ARRAY(s->value))
        {
            printf("fail_array\n");
            return;
        }
        fa = fa->next;
    }
    printf("array_OK\n");
    ASSERT(GET_ARRAY(s->value)->length, 3);
    ASSERT(regions->free, 0);
    ASSERT((regions->next != NULL), 1);
}

/* /\** */
/*  * Печать объекта */
/*  * Создать объект (4 . 5) */
/*  *\/ */
/* void test_print() */
/* { */
/*     printf("test_print: "); */
/*     int n1 = 4; */
/*     int n2 = 5; */
    
/*     object_t *list = new_pair(object_new(NUMBER, &n1), object_new(NUMBER, &n2)); */
/*     print_obj(list); */
/*     printf("\n"); */
/* } */

/**
 *
 */
void test_return_type()
{
    printf("test_return_type: ");
    reset_mem();
    object_t obj = (0xFFFA << TYPE_BITS) + SYMBOL;
    ASSERT(TYPE(obj), SYMBOL);
}

void test_return_set_mark()
{
    printf("test_return_set_mark: ");
    object_t obj = 0xf0;
    SET_MARK(obj); // 1000
    ASSERT(obj, 0xf8);
}

void test_return_get_mark()
{
    printf("test_return_get_mark : ");
    object_t obj = 0xf0;
    ASSERT(GET_MARK(obj), 0);
    obj = 0xf8;
    ASSERT(GET_MARK(obj), 1);
}

void test_return_clear_mark()
{
    printf("test_return_clear_mark: ");
    object_t obj = 0xf8;
    CLEAR_MARK(obj); // 1000
    ASSERT(obj, 0xf0);
    obj = 0xf1;
    CLEAR_MARK(obj); // 1000
    ASSERT(obj, 0xf1);
}
/**
 * тест: создаём объект и с помощью макроса получаем адрес объекта
 */
void test_return_get_addr()
{
    printf("test_return_get_addr: ");
    object_t obj = 0xABC8;   
    ASSERT(GET_ADDR(obj), 0xABC0);
}

/**
 * тест: создаём объект с помощью макроса и проверяем его тип
 */
void test_new_object(int type, void *val)
{
    printf("test_new_object: ");
    object_t object = NEW_OBJECT(type, val);
    ASSERT(TYPE(object), type);
}


/*
object_new
|условие               |правильный класс                    |неправильный класс                       |
|Создание объекта      |1) NUMBER, 1                        |6) Несовпадение типа и данных (NUMBER, Y)|
|                      |2) SYMBOL, T                        |7) Пустой тип                            |
|                      |3) PAIR, 0,0                        |8) Пустой указатель на данные            |
|                      |4) STRING, "qwerty"                 |                                         |
|                      |5) ARRAY, (1,2,3)                   |                                         |

new_pair
|условие               |правильный класс                    |неправильный класс                       |
|Указатель на объекты  |9) 0,1                              |11) Нет                                  |
left и right           |10) Нулевой указатель               |12) Пустой тип                           |


new_array
|условие               |правильный класс                    |неправильный класс                       |
|Указатель на объект   |13) (0,1,2)                         |15) Нулевой указатель                    |
|                      |14) (q,w,e)                         |16) Пустой тип                           |


new_empty_array
|условие               |правильный класс                    |неправильный класс                       |
|Длина массива         |17) NUMBER > 0                      |18) NUMBER ≤ 0                           |

garbage_collect
|условие               |правильный класс                    |неправильный класс                       |
|Существование         |19)NUMBER, 1                        |                                         |
объекта/ов             |20) SYMBOL, T                       |                                         |
с ссылкой на символ    |21) PAIR, 0,0                       |                                         |
|                      |22) STRING, "qwerty"                |                                         |
|Существование         |23) ARRAY, (1,2,3)                  |                                         |
свободного объекта/ов  |24)Да                               |                                         |
*/
void main()
{
    printf("--------------test objects---------------------\n");
    init_regions();
    init_objects();
    test_mark();
    test_sweep();    //19,24
    test_garbage_collect();     //19,24   
    test_garbage_collect_list();    //21,24
    test_alloc_region();
    test_free_region();
    test_new_string();
    test_free_string();
    test_free_array();
    reset_mem();
    test_new_empty_array();
    test_objects_new_null();
    test_free_pair_empty();
    test_garbage_collect_strings(); //22,24
    test_garbage_collect_arrays();  //23,24
    test_garbage_collect_cycle();
    test_return_type();
    test_return_set_mark();
    test_return_get_mark();
    test_return_clear_mark();
    test_return_get_addr();
    reset_mem();
    test_new_bignumber(1100);
    test_new_bignumber(0);
    test_new_bignumber(-1520);
    test_free_bignumber();
    test_free_bignumber2();
    reset_mem();
    test_new_number(-677, NUMBER);
    test_new_number(56, NUMBER);
    test_new_number(0xfffffff, BIGNUMBER);
    test_new_number(0xffffff8, BIGNUMBER);
    test_new_number(0, NUMBER);
    test_new_number((1<<27)-1, NUMBER);
    test_new_number(-((1<<27)-1), NUMBER);
    test_get_value(13);
    test_get_value(0);
    test_get_value(-6);
    reset_mem();
    test_new_pair();
    test_free_pair();
    reset_mem();
    PRINT(new_number(10));
    PRINT(new_bignumber(2000000000));
    PRINT(make_list(4));
    PRINT(NEW_ARRAY(make_list(2)));
    PRINT(NEW_OBJECT(SYMBOL, new_symbol("asd")));
    PRINT(NEW_STRING("Pasha"));
}
