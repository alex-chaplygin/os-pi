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
}

void main()
{
    printf("--------------test objects---------------------\n");
    test_object_new();
    test_new_pair();
    int i = 10;
    test_print_obj(object_new(NUMBER, &i), "10");
    test_free_object();
    test_free_pair();
}
