#include <stdio.h>
#include "test.h"
#include "objects.h"

extern object_t objects[];
extern pair_t pairs[];

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
 * Создать две парыи проверить массив пар 
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

void main()
{
  test_object_new();
  test_new_pair();
}