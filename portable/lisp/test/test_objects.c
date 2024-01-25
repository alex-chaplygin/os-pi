#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "test.h"
#include "objects.h"
#include "parser.h"

extern object_t objects[];
extern pair_t pairs[];
extern object_t *free_objs;
extern int last_object;
extern int last_pair;
extern int last_symbol;
extern pair_t *free_pairs;
extern char region_data[];
extern string_t *free_strings;
extern array_t *free_arrays;
extern struct region *regions;
extern int last_string;

object_t *mobject;

void mark_object(object_t *obj);

void sweep();

void garbage_collect();

void free_string(string_t *s);

void error(char *str, ...)
{
    printf("%s\n", str);
}

symbol_t *find_symbol(char *str)
{
    return NULL;
}

/** 
 * Создать два обьекта числа и провериить массив обьектов
 */
void test_object_new_number()
{
    int number = 10;
    printf("test_object_new: ");
    object_new(NUMBER, &number);
    number = 11;
    object_new(NUMBER, &number);
    ASSERT(objects[0].u.value, 10);
    ASSERT(objects[1].u.value, 11);
}

/** 
 * Создать две пары и проверить массив пар 
 */
void test_new_pair()
{
    object_t o1, o2, o3, o4;
    o1.type = NUMBER;
    o1.u.value = 5;
    printf("test_new_pair: ");
    new_pair(&o1, &o2);
    new_pair(&o3, &o4);
    ASSERT(pairs[0].left, &o1);
    ASSERT(pairs[0].left->u.value, 5);
    ASSERT(pairs[0].right, &o2);
    ASSERT(pairs[1].left, &o3);
    ASSERT(pairs[1].right, &o4);
}

/** 
 * Проверка правильной печати объекта (1 2)
 */
void test_print_obj(object_t *obj, const char *expected_output)
{
    printf("test_print_obj: ");
    
    int outdes = dup(1);
    FILE *file = freopen("/tmp/test.txt", "w", stdout);
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
    last_object = 0;
    free_objs = NULL;
    last_pair = 0;
    free_pairs = NULL;
    last_string = 0;
    free_strings = NULL;
}

/**
 * Проверка корректности функции освобождения
 *  объектов и их дальнейшего переиспользования
 */
void test_free_object()
{
    printf("test_free_object: ");
    reset_mem();
    int last_num = last_object;
    for (int i = last_object; i < MAX_OBJECTS; i++)
	object_new(NUMBER, &i);
    object_t *o1 = &objects[0];
    object_t *o2 = &objects[5];
    free_object(o1);
    free_object(o2);
    ASSERT(free_objs, o2);
    ASSERT(free_objs->next, o1);
    ASSERT(free_objs->next->next, NULL);
    object_t *r_o1 = object_new(SYMBOL, "1");
    object_t *r_o2 = object_new(SYMBOL, "2");
    ASSERT(o2, r_o1);
    ASSERT(o1, r_o2);
    ASSERT(free_objs, NULL);
}

/** 
 * Проверка на получение объекта, равного NULL
 */
void test_free_object_null()
{
    printf("test_free_object_null: ");
    reset_mem();
    int n = 5;
    object_t *o3 = NULL;
    object_t *o4 = object_new(NUMBER, &n);    
    free_object(o4);
    free_object(o3);    
    ASSERT(free_objs, o4);
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
    for (int i = last_object; i < MAX_PAIRS; i++)
    {   
        object_t o1, o2;
        o1.type = NUMBER;
        o1.u.value = i;
        o2.type = NUMBER;
        o2.u.value = i-1;
	new_pair(&o1, &o2);
    }
    pair_t *pair1 = &pairs[0];
    pair_t *pair2 = &pairs[1];
    free_pair(pair1);
    free_pair(pair2);
    ASSERT(free_pairs, pair2);
    ASSERT(free_pairs->next, pair1);
    ASSERT(free_pairs->next->next, NULL);
}

/**
 * Создать объект (12 A 5)
 */
void test_mark()
{
    printf("test_mark :");
    reset_mem();
    print_free_objs();
    int n1 = 12;
    int n2 = 5;
    object_t *num1 = object_new(NUMBER, &n1);
    object_t *sym = object_new(SYMBOL, "A");
    object_t *num2 = object_new(NUMBER, &n2);
    object_t *p3 = new_pair(num2, NULL);
    object_t *p2 = new_pair(sym, p3);
    mobject = new_pair(num1, p2);
    mark_object(mobject);
    ASSERT(mobject->mark, 1);
    ASSERT(num1->mark, 1);
    ASSERT(p2->mark, 1);
    ASSERT(sym->mark, 1);
    ASSERT(p3->mark, 1);
    ASSERT(num2->mark, 1);
}

/**
 * Используем помеченные объекты с предыдущего теста.
 * Проверить снятие пометок
 * Проверить, что помеченные ранее объекты не находятся в списке свободных
 */
void test_sweep()
{
    printf("test_sweep: ");
    print_free_objs();
    object_t *num1 = mobject->u.pair->left;
    object_t *p2 = mobject->u.pair->right;
    object_t *sym = p2->u.pair->left;
    object_t *p3 = p2->u.pair->right;
    object_t *num2 = p3->u.pair->left;
    sweep();
    printf("after_sweep: ");
    print_free_objs();
    ASSERT(mobject->mark, 0);
    ASSERT(num1->mark, 0);
    ASSERT(p2->mark, 0);
    ASSERT(sym->mark, 0);
    ASSERT(p3->mark, 0);
    ASSERT(num2->mark, 0); 
    object_t *f = free_objs;
    while (f != NULL) {
	//	printf("f type =%d\n", f->type);
	if (f == mobject || f == num1 || f == p2 || f == sym || f == p3 || f == num2) {
	    printf("fail_object\n");
	    return;
	}
	f = f->next;
    }
    printf("objects: OK\n");
}

/**
 * Создать символ A
 * Присвоить ему значение - объект 52
 * Проверить, что объект не в списке свободных
 */
void test_garbage_collect()
{
    printf("test_garbage_collect: ");
    int num1 = 52;
    symbol_t *s = new_symbol("A");
    object_t *obj1 = object_new(NUMBER, &num1);
    s->value = obj1;
    garbage_collect();

    object_t *f = free_objs;
    while (f != NULL) {
	if (f == obj1) {
	    printf("fail_object\n");
	    return;
	}
	f = f->next;
    }
    printf("OK\n");
}

/**
 * Создать символ B
 * Присвоить ему значение - объект (1 2)
 * Проверить, что объект не в списке свободных
 */
void test_garbage_collect_list()
{
    printf("test_garbage_collect_list: ");
    int num1 = 1;
    int num2 = 2;
    symbol_t *s = new_symbol("B");
    object_t *obj1 = object_new(NUMBER, &num1);
    object_t *obj2 = object_new(NUMBER, &num2);
    object_t *p1 = new_pair(obj1, new_pair(obj2, NULL));
    s->value = p1;
    printf("before gc: \n");
    printf("objects: ");
    print_free_objs();
    printf("\npairs: ");
    print_free_pairs();
    garbage_collect();
    printf("after gc: ");
    print_free_objs();

    object_t *f = free_objs;
    while (f != NULL) {
	if (f == obj1 || f == obj2) {
	    printf("fail_object\n");
	    return;
	}
	f = f->next;
    }
    printf("objects_OK\n");
    pair_t *p = free_pairs;
    while (p != NULL) {
	if (p == p1->u.pair || p == p1->u.pair->right->u.pair) {
	    printf("fail_pair\n");
	    return;
	}
	p = p->next;
    } 
    printf("pairs: OK\n");
}

/**
 * Создать две пары
 * Присвоить левым частям этих пар - числа, а правые ссылались друг на друга
 * Создать два символа, которые ссылаются на эти пары
 * Проверить работу сборщика мусора, у пар поле free должно быть равно нулю.
 */
void test_garbage_collect_cycle()
{
    printf("test_garbage_collect_cycle: ");
    int num1 = 1;
    symbol_t *s1 = new_symbol("A");
    symbol_t *s2 = new_symbol("B");
    object_t *p1 = new_pair(object_new(NUMBER, &num1), NULL);
    object_t *p2 = new_pair(object_new(NUMBER, &num1), NULL);
    p1->u.pair->right = p2;
    p2->u.pair->right = p1;
    s1->value = p1;
    s2->value = p2;
    printf("p1 = ");
    PRINT(p1);
    garbage_collect();

    ASSERT(p1->free, 0);
    ASSERT(p2->free, 0);
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
    int size = 3 * sizeof(int) + 2 * sizeof(int *);
    ASSERT((reg1 - region_data), size);
    ASSERT((reg2 - region_data), size + 8 + size);
    //    free_region(reg1);
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
    int offset = 3 * sizeof(int) + 2 * sizeof(int *);
    struct region *r1 = (struct region *)(reg1 - offset);
    struct region *r2 = (struct region *)(reg2 - offset);
    struct region *r3 = (struct region *)(reg3 - offset);
    free_region(reg2);
    ASSERT(r1->next, r2);
    ASSERT(r2->prev, r1);
    free_region(reg1);
    ASSERT(r1->next, r3);
    ASSERT(r3->prev, r1);
    ASSERT(r1->size, 20 + offset);
    free_region(reg3);
    ASSERT(r1->size, 36 + 2 * offset);
}

/**
 * Проверка на переполнение памяти объектами 
 */
void test_objects_new_null()
{   
    printf("test_objects_new_null: ");
    reset_mem();
    int i;
    for (i = 0; i < MAX_OBJECTS; i++)
	object_new(NUMBER, &i);
	
    ASSERT(object_new(NUMBER, &i), ERROR);
}

/** 
 * Тест на переполение памяти пар
 */
void test_pairs_overflow()
{
    reset_mem();
    symbol_t *symbs[MAX_SYMBOLS];
    printf("test_pairs_overflow: ");
    for (int i = 0; i < MAX_SYMBOLS; i++) {
        char str[4];
        snprintf(str, sizeof(str), "a%d", i);
        symbs[i] = new_symbol(str);
	symbs[i]->value = new_pair(NULL, NULL);
    }
    for (int i = last_pair; i < MAX_PAIRS; i++)
	new_pair(NULL, NULL);
    object_t *pair = new_pair(NULL, NULL);
    ASSERT(pair, ERROR);
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

/**
 * Переполнить память пар
 * Вызвать сборщик мусора
 * Создать ещё 1 пару
 */
void test_free_pair_max_memory()
{
    printf("test_free_pair_max_memory: ");
    reset_mem();
    for (int i = last_pair; i < MAX_PAIRS; i++)
	new_pair(NULL, NULL);
    garbage_collect();
    object_t *pair = new_pair(NULL, NULL);
    ASSERT(pair, pair);
}

/**
 * Освободить пустую пару
 */
void test_free_pair_empty()
{
    printf("test_free_pair_empty: ");
    free_pair(NULL);    
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

/**
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
    object_t *obj1 = object_new(STRING, "abc");
    object_t *obj2 = object_new(STRING, "ff");
    object_t *obj3 = object_new(STRING, "cc");
    s->value = obj1;
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
object_t *make_list(int num)
{
    object_t *o = object_new(NUMBER, &num);
    if (num == 1)
	return new_pair(o, NULL);
    return new_pair(o, make_list(num - 1));
}

/**
 * Создать символ B
 * Присвоить ему значение - массив из трех элементов
 * Создать ещё два массива - 10 элементов и 20 элементов
 * Выполнить сборку мусора
 * Проверить, что объект не в списке свободных массивов
 */
void test_garbage_collect_arrays()
{
    printf("test_garbage_collect_array: ");
    reset_mem();
    symbol_t *s = new_symbol("B");
    object_t *obj1 = object_new(ARRAY, new_array(make_list(3)));
    object_t *obj2 = object_new(ARRAY, new_array(make_list(10)));
    object_t *obj3 = object_new(ARRAY, new_array(make_list(20)));
    s->value = obj1;
    garbage_collect();
    array_t *fs = free_arrays;
    while (fs != NULL) {
	if (fs == obj1->u.arr) {
	    printf("fail_array\n");
	    return;
	}
	fs = fs->next;
    }
    printf("array_OK\n");
    ASSERT(obj1->u.arr->length, 3);
    ASSERT(regions->free, 0);
    ASSERT((regions->next != NULL), 1);
}
/**
 * Создать символ B
 * Присвосить ему массив в котором будет пара
 * Проверить после сборки мусора, что эта пара не очистилась
 */
void test_garbage_collect_array()
{
    printf("test_garbage_collect_array: ");
    reset_mem();
    symbol_t *s = new_symbol("B");
    int i = 1;
    object_t *num = object_new(NUMBER, &i);
    object_t *p = new_pair(num, num);
    object_t *arr = object_new(ARRAY, new_empty_array(i));
    arr->u.arr->data[0] = p;
    s->value = arr;
    garbage_collect();
    ASSERT(p->u.pair->free, 0);
}

/**
 * Печать объекта
 * Создать объект (4 . 5)
 */
void test_print()
{
    printf("test_print: ");
    int n1 = 4;
    int n2 = 5;
    
    object_t *list = new_pair(object_new(NUMBER, &n1), object_new(NUMBER, &n2));
    print_obj(list);
    printf("\n");
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
    test_object_new_number();   // 1
    test_new_pair();            //3, 9
    test_free_object();
    test_free_object_null();
    test_free_pair();
    test_mark();
    test_sweep();
    test_garbage_collect();     //19,24
    test_garbage_collect_list();    //21,24
    test_alloc_region();
    test_free_region();
    test_new_string();
    test_objects_new_null();
    test_pairs_overflow();
    test_free_pair_max_memory();
    test_free_pair_empty();
    test_free_string();
    test_garbage_collect_strings(); //22,24
    test_garbage_collect_arrays();  //23,24
    test_garbage_collect_cycle();
    test_garbage_collect_array();
    test_print();
    int i = 10;
    test_print_obj(object_new(NUMBER, &i), "10");
}
