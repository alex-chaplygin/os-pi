#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <setjmp.h>
#include "test.h"
#include "objects.h"
#include "cont.h"
#include "parser.h"
#include "alloc.h"
#include "bind.h"

extern bignumber_t *bignumbers;
extern bignumber_t *free_bignumbers;
extern float_t *floats;
extern float_t *free_floats;
extern pair_t *pairs;
extern string_t *strings;
extern array_t *arrays;
extern symbol_t *symbols;
extern int last_bignumber;
extern int last_pair;
extern int last_symbol;
extern pair_t *free_pairs;
extern char *region_data;
extern string_t *free_strings;
extern array_t *free_arrays;
extern continuation_t *free_continuations;
extern symbol_t *free_symbols;
extern struct region *regions;
extern int last_string;
extern int last_array;
extern int last_function;
extern int last_continuation;
extern function_t *functions;
extern function_t *free_functions;
extern continuation_t *continuations;
extern continuation_t *free_continuations;
void mark_object(object_t obj);
void sweep();
void garbage_collect();
void free_string(string_t *s);
object_t make_list(int num);

jmp_buf jmp_env;
/// текущее окружение
object_t current_env = NULLOBJ;
/// окружение функции
object_t func_env = NULLOBJ;

void error(char *str, ...)
{
    printf("%s\n", str);
    longjmp(jmp_env, 1);
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
    last_function = 0;
    last_continuation = 0;
    free_continuations = NULL;
    free_pairs = NULL;
    free_strings = NULL;
    free_bignumbers = NULL;
    free_arrays = NULL;
    free_functions = NULL;
    free_symbols = NULL;
}


symbol_t *find_symbol(char *str)
{
    return NULL;
}

void hash_remove(symbol_t *s)
{
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
 * Создать объект вещественное число и проверить, что оно правильно записалось
 */
void test_new_float(float number)
{
    printf("test_new_float: ");
    object_t fl = new_float(number);
    ASSERT(GET_FLOAT(fl)->value, number);
}

/**
 * Создать объект функции, напечатать и проверить тип объекта
 * args: (X Y)
 * body: (+ X Y)
 * 
 */
void test_new_function()
{
    object_t args;
    object_t body;
    printf("test_new_function: ");
    object_t xsym = NEW_OBJECT(SYMBOL, new_symbol("X"));
    object_t ysym = NEW_OBJECT(SYMBOL, new_symbol("Y"));
    object_t plussym = NEW_OBJECT(SYMBOL, new_symbol("+"));
    args = new_pair(xsym, new_pair(ysym, NULLOBJ));
    body = new_pair(plussym, new_pair(xsym, new_pair(ysym, NULLOBJ)));
    object_t nf = new_function(args, body, NULLOBJ, NULLOBJ);
    ASSERT(TYPE(nf), FUNCTION);
    PRINT(nf);
}

void test_new_continuation()
 {
    printf("test_new_continuation: ");
    reset_mem();

    jmp_buf buf;
    if (setjmp(buf) == 0) {
        object_t cont = new_continuation(buf);
        ASSERT(TYPE(cont), CONTINUATION);
        continuation_t *c = (continuation_t *)GET_ADDR(cont);
        ASSERT(memcmp(c->buffer, buf, sizeof(jmp_buf)), 0);
        ASSERT(last_continuation, 1);
        free_continuation(c);
        if (free_continuations == c)
            printf("Free OK\n");
        else
            printf("Free Fail\n");
    }
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
    char *s = fgets(output_buffer, sizeof(output_buffer), output_file);
    fclose(output_file);

    ASSERT(strcmp(output_buffer, expected_output), 0);
}

/** 
 * Освобождение большого числа из объекта
 */
void test_free_bignumber()
{
    printf("test_free_bignumber: ");
    reset_mem();
    object_t o = new_number(800000000);
    printf("%x\n", (int)o);
    ASSERT(TYPE(o), BIGNUMBER);
    free_bignumber((bignumber_t *)GET_ADDR(o));
    ASSERT(free_bignumbers, (bignumber_t *)GET_ADDR(o));
}

/** 
 * Создать объект функцию, освободить этот объект и проверить,
 * что он попал в голову списка свободных функций
 */
void test_free_function()
{
    printf("test_free_function: ");
    reset_mem();
    object_t xsym = NEW_OBJECT(SYMBOL, new_symbol("X"));
    object_t ysym = NEW_OBJECT(SYMBOL, new_symbol("Y"));
    object_t plussym = NEW_OBJECT(SYMBOL, new_symbol("+"));
    object_t args = new_pair(xsym, new_pair(ysym, NULLOBJ));
    object_t body = new_pair(plussym, new_pair(xsym, new_pair(ysym, NULLOBJ)));
    object_t o = new_function(args, body, NULLOBJ, NULLOBJ);
    ASSERT(TYPE(o), FUNCTION);
    free_function((function_t *)GET_ADDR(o));
    ASSERT(free_functions, (function_t *)GET_ADDR(o));
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
 * Освобождение вещественного числа из объекта
 */

void test_free_float()
{
    printf("test_free_float: ");
    reset_mem();
    object_t o = new_float(800000.1232f);
    printf("%x\n", (int)o);
    ASSERT(TYPE(o), FLOAT);
    free_float((float_t *)GET_ADDR(o));
    ASSERT(free_floats, (float_t *)GET_ADDR(o));
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
    object_t n = new_bignumber(214748365);
    object_t s = NEW_STRING("abc");
    object_t l = make_list(3);
    printf("make_list:");
    PRINT(l);
    object_t a = NEW_ARRAY(l);
    printf("array: ");
    PRINT(a);
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
    bind_static(NEW_OBJECT(SYMBOL, s));
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
    bind_static(NEW_OBJECT(SYMBOL, s));
    garbage_collect();
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
    bind_static(NEW_OBJECT(SYMBOL, s1));
    bind_static(NEW_OBJECT(SYMBOL, s2));
    garbage_collect();
    ASSERT(GET_PAIR(p1)->free, 0);
    ASSERT(GET_PAIR(p2)->free, 0);
}

/**
 * Создать символ B
 * Присвоить ему значение - большое число 123456789
 * Создать ещё два больших числа
 * Выполнить сборку мусора
 * Проверить, что объект не в списке свободных больших чисел
 */
void test_garbage_collect_bignumbers() 
{ 
    printf("test_garbage_collect_bignumbers: "); 
    reset_mem(); 
    symbol_t *s = new_symbol("B");
    bind_static(NEW_OBJECT(SYMBOL, s));    
    object_t obj1 = new_bignumber(1234567890); 
    object_t obj2 = new_bignumber(-987654321); 
    object_t obj3 = new_bignumber(555555555); 
    s->value = obj1;
    garbage_collect();
    bignumber_t *fb = free_bignumbers; 
    while (fb != NULL) {
       printf("%d ", fb->value);
       if (fb == (bignumber_t *)GET_ADDR(obj1)) { 
           printf("fail_bignumbers\n"); 
           return; 
       } 
       fb = fb->next; 
    } 
    printf("bignumbers_OK\n");
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
    //free_region(reg1);
    //free_region(reg2);
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
    int offset = sizeof(struct region) - sizeof(char *);
    printf("offset = %d\n", offset);
    int mask = (1 << MARK_BIT) - 1;
    struct region *r1 = (struct region *)(reg1 - offset);
    struct region *r2 = (struct region *)(reg2 - offset);
    struct region *r3 = (struct region *)(reg3 - offset);
    ASSERT((int)reg1 & mask, 0);
    ASSERT((int)reg2 & mask, 0);
    ASSERT((int)reg3 & mask, 0);
    ASSERT((int)reg4 & mask, 0); 
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
    bind_static(NEW_OBJECT(SYMBOL, s));    
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

/*
 * Создать символ B
 * Присвоить ему значение - вещественное число 12.837
 * Создать ещё два числа
 * Выполнить сборку мусора
 * Проверить, что объект не в списке свободных вещественных чисел
 */
void test_garbage_collect_floats() 
{ 
    printf("test_garbage_collect_floats: "); 
    reset_mem(); 
    symbol_t *s = new_symbol("B"); 
    object_t obj1 = new_float(12.837f); 
    object_t obj2 = new_float(0.5f); 
    object_t obj3 = new_float(28391.9213f); 
    s->value = obj1;
    bind_static(NEW_OBJECT(SYMBOL, s));    
    garbage_collect();
    
    float_t *fs = free_floats; 
    while (fs != NULL) {
	printf("%f\n", fs->value);
	if (fs == (float_t *)GET_ADDR(obj1)) { 
	    printf("fail_floats\n"); 
	    return; 
	} 
	fs = fs->next; 
    } 
    printf("floats_OK\n");
    ASSERT(regions->free, 0);
    ASSERT((regions->next != NULL), 1);
}

/*
 * Создать две функции
 * Создать символ и присвоить ему функцию суммирования
 * Выполнить сборку мусора
 * Проверить, что функция суммирования не в списке свободных строк
 */
void test_garbage_collect_functions()
{
    printf("test_garbage_collect_functions: ");
    
    object_t args1;
    object_t body1;
    object_t xsym = NEW_OBJECT(SYMBOL, new_symbol("X"));
    object_t ysym = NEW_OBJECT(SYMBOL, new_symbol("Y"));
    object_t plussym = NEW_OBJECT(SYMBOL, new_symbol("+"));
    args1 = new_pair(xsym, new_pair(ysym, NULLOBJ));
    body1 = new_pair(plussym, new_pair(xsym, new_pair(ysym, NULLOBJ)));
    object_t funsum = new_function(args1, body1, NULLOBJ, NULLOBJ);

    object_t args2;
    object_t body2;
    object_t asym = NEW_OBJECT(SYMBOL, new_symbol("A"));
    object_t bsym = NEW_OBJECT(SYMBOL, new_symbol("B"));
    object_t mulsym = NEW_OBJECT(SYMBOL, new_symbol("*"));
    args2 = new_pair(asym, new_pair(bsym, NULLOBJ));
    body2 = new_pair(mulsym, new_pair(asym, new_pair(bsym, NULLOBJ)));
    object_t funmultiply = new_function(args2, body2, NULLOBJ, NULLOBJ);

    symbol_t *s = new_symbol("F");
    s->value = funsum;
    bind_static(NEW_OBJECT(SYMBOL, s));    

    garbage_collect();

    function_t *ff = free_functions;
    while (ff != NULL) {
	if (ff == (function_t *)GET_ADDR(funsum)) {
	    printf("fail_functions\n");
	    return;
	}
	ff = ff->next;
    }
    printf("functions_OK\n");
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
 * Создать пустой массив заданной длины и проверить все ли элементы с пустым значением 
 *
 * @param length = 8
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
    bind_static(NEW_OBJECT(SYMBOL, s));    
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

/*
 * Создать символ B
 * Присвоить ему значение - символ "AB"
 * Создать ещё два символа
 * Выполнить сборку мусора
 * Проверить, что объект не в списке свободных символов
 */
void test_garbage_collect_symbols() 
{ 
    printf("test_garbage_collect_symbols: "); 
    reset_mem(); 
    symbol_t *s = new_symbol("B"); 
    symbol_t *obj1 = new_symbol("AB"); 
    symbol_t *obj2 = new_symbol("TEST"); 
    symbol_t *obj3 = new_symbol("T2"); 
    s->value = NEW_OBJECT(SYMBOL, obj1);
    bind_static(NEW_OBJECT(SYMBOL, s));    
    garbage_collect();
    symbol_t *fs = free_symbols; 
    while (fs != NULL) { 
       if (!strcmp(fs->str, "AB")) { 
           printf("fail_symbol\n"); 
           return; 
       } 
       fs = fs->next; 
    } 
    printf("symbols_OK\n"); 
}

/**
 * Создать максимальное количество символов,
 * освободить первый символ, проверить, что он находится в списке свободных символов
 * Создать еще один символ и проверить, что его адрес совпадает с первым
 */
void test_free_symbol()
{
    char str[10];
    printf("test_free_symbol: ");
    reset_mem();
    for (int i = last_symbol; i < MAX_SYMBOLS;i++) {
       sprintf(str, "s%d", i);
       new_symbol(str);
    }
    free_symbol(&symbols[0]);
    ASSERT(free_symbols, &symbols[0]);
    symbol_t *s = new_symbol("1");
    ASSERT(s, &symbols[0]);
}

/**
 * Проверить, правильно ли макрос извлекает закодированный тип из объекта
 */
void test_return_type()
{
    printf("test_return_type: ");
    reset_mem();
    object_t obj = (0xFFFA << TYPE_BITS) + SYMBOL;
    ASSERT(TYPE(obj), SYMBOL);
}

/**
 * Проверить, что макрос правильно устанавливает маркерные биты в объекте
 */
void test_return_set_mark()
{
    printf("test_return_set_mark: ");
    object_t obj = 0xe0;
    SET_MARK(obj); // 1000
    ASSERT(obj, 0xf0);
}

/**
 * Проверить, что макрос правильно определяет маркерные биты в объекте
 */
void test_return_get_mark()
{
    printf("test_return_get_mark : ");
    object_t obj = 0x20;
    ASSERT(GET_MARK(obj), 0);
    obj = 0x10;
    ASSERT(GET_MARK(obj), 1);
}

/**
 * Проверить, что макрос корректно очищает маркерные биты в объекте
 */
void test_return_clear_mark()
{
    printf("test_return_clear_mark: ");
    object_t obj = 0xff;
    CLEAR_MARK(obj); // 1000
    ASSERT(obj, 0xef);
    obj = 0x10;
    CLEAR_MARK(obj); // 1000
    ASSERT(obj, 0x0);
}

/**
 * Создаем объект и с помощью макроса получаем адрес объекта
 */
void test_return_get_addr()
{
    printf("test_return_get_addr: ");
    object_t obj = 0xABC8;   
    ASSERT(GET_ADDR(obj), 0xABC0);
}

/**
 * Создаём объект с помощью макроса и проверяем его тип
 */
void test_new_object(int type, void *val)
{
    printf("test_new_object: ");
    object_t object = NEW_OBJECT(type, val);
    ASSERT(TYPE(object), type);
}

/** 
 * Тест создания и получения одиночного символа
 *
 * @param s символ
 */
void test_get_char(char s)
{
    printf("test_get_char: ");
    object_t obj = NEW_CHAR(s);
    ASSERT(GET_CHAR(obj), s);
}

/** 
 * Тест проверки размера занимаемой памяти региона, после выделения
 */
void test_regions_mem()
{
    printf("test_regions_mem: ");
    alloc_region(128);
    int expected_memory = 128 + sizeof(struct region);
    ASSERT(expected_memory, regions_mem());
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
    if (setjmp(jmp_env) == 1)
	return;
    printf("--------------test objects---------------------\n");
    // Инициализация
    init_regions();
    test_regions_mem(); // тест памяти
    init_objects();

    // Тесты сборки мусора
    test_mark();
    test_sweep();    //19,24
    test_garbage_collect();     //19,24
    test_garbage_collect_bignumbers();
    test_garbage_collect_list();    //21,24
    test_garbage_collect_strings(); //22,24
    test_garbage_collect_arrays();  //23,24
    test_garbage_collect_symbols();
    test_garbage_collect_cycle();
    test_garbage_collect_floats();
    test_garbage_collect_functions();
    test_objects_new_null();
    reset_mem();

    // Тесты для массивов
    test_new_empty_array();
    test_free_array();
    reset_mem();

    // Тесты для строк
    test_new_string();
    test_free_string();
    reset_mem();

    // Тесты для чисел (малые числа)
    test_new_number(-677, NUMBER);
    test_new_number(56, NUMBER);
    test_new_number(0xfffffff, BIGNUMBER);
    test_new_number(0xffffff8, BIGNUMBER);
    test_new_number(0, NUMBER);
    test_new_number((1<<ADDR_BITS-1)-1, NUMBER); //fail
    test_new_number(-((1<<ADDR_BITS-1)-1), NUMBER); //fail
    test_get_value(13);
    test_get_value(0);
    test_get_value(-6);
    reset_mem();

    // Тесты для чисел (большие числа)
    test_new_bignumber(1100);
    test_new_bignumber(0);
    test_new_bignumber(-1520);
    test_free_bignumber();
    test_free_bignumber2();
    reset_mem();

    // Тесты для чисел (числа с плавающей точкой)
    test_new_float(-13.23f);
    test_new_float(0.0f);
    test_new_float(78.34f);
    test_free_float();
    reset_mem();

    // Тесты для функций
    test_new_function();
    test_new_continuation();
    test_free_function();
    reset_mem();

    // Тесты для пар
    test_new_pair();
    test_free_pair();
    test_free_pair_empty();
    reset_mem();
    
    // Тест символов
    test_free_symbol();

    // Тесты для регионов памяти
    test_alloc_region();
    test_free_region();
    reset_mem();

    // Тесты для работы с маркерами
    test_return_set_mark();
    test_return_get_mark();
    test_return_clear_mark();
    reset_mem();

    // Тесты для извлечения и преобразования данных
    test_return_type();
    test_return_get_addr();
    test_get_char('a');
    test_get_char(' ');
    reset_mem();

    // Вывод
    PRINT(new_number(10));
    PRINT(new_bignumber(2000000000));
    PRINT(make_list(4));
    PRINT(NEW_ARRAY(make_list(2)));
    PRINT(NEW_OBJECT(SYMBOL, new_symbol("asd")));
    PRINT(NEW_STRING("Pasha"));
    PRINT(NEW_CHAR('1'));
    PRINT(NEW_CHAR('a'));
    PRINT(new_float(1.25));
}
