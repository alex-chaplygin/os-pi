#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "test.h"
#include "objects.h"

extern object_t objects[];
extern pair_t pairs[];
extern object_t *free_objs;
extern int last_object;
extern int last_pair;
extern pair_t *free_pairs;
extern char region_data[];

object_t *mobject;

void mark_object(object_t *obj);

void sweep();

void garbage_collect();

void error(char *str)
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
void test_object_new()
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
 * Проверка корректности функции освобождения
 *  объектов и их дальнейшего переиспользования
 */
void test_free_object()
{
    printf("test_free_object: ");
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
    last_object = 0;
    free_objs = NULL;
    last_pair = 0;
    free_pairs = NULL;
}

/** 
 * Проверка на получение объекта, равного NULL
 */
void test_free_object_null()
{
    printf("test_free_object_null: ");
    int n = 5;
    object_t *o3 = NULL;
    object_t *o4 = object_new(NUMBER, &n);    
    free_object(o4);
    free_object(o3);    
    ASSERT(free_objs, o4);
    free_objs = NULL;
}

/**
 * Проверка корректности функции освобождения пары
 *  объектов и их дальнейшего переиспользования
 */
void test_free_pair()
{
    printf("test_free_pair: ");
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
    last_pair = 0;
    free_pairs = NULL;
    last_object = 0;
    free_objs = NULL;
}

/**
 * Создать объект (12 A 5)
 */
void test_mark()
{
    printf("test_mark :");
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
 * Выделить 2 региона с размерами 5 и 64
 * Проверить указатели регионов относительно начала
 * Проверить указатели данных
 */
void test_alloc_region()
{
    printf("test_alloc_region: ");
    char *reg1 = alloc_region(5);
    char *reg2 = alloc_region(64);
    ASSERT((reg1 - region_data), 20+8);
    ASSERT((reg2 - region_data), 20 + 8 + 20 + 64);
}

void main()
{
    printf("--------------test objects---------------------\n");
    init_regions();
    test_object_new();
    test_new_pair();
    int i = 10;
    test_free_object();
    test_free_object_null();
    test_free_pair();
    test_mark();
    test_sweep();
    test_garbage_collect();
    test_garbage_collect_list();
    test_print_obj(object_new(NUMBER, &i), "10");
    test_alloc_region();
}
