#include <stdio.h>
#include <setjmp.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "test.h"
#include "parser.h"
#include "string.h"
#include "alloc.h"

extern object_t t;
extern object_t nil;

object_t add(object_t list);
object_t add_float(object_t list, float sum);
object_t sub(object_t first, object_t list);
object_t sub_float(object_t list, float sub);
object_t mul(object_t list);
object_t mul_float(object_t list, float mul);
object_t DIV(object_t first, object_t list);
object_t div_float(object_t list, float div);
object_t mod(object_t obj1, object_t obj2);
object_t bitwise_and(object_t list);
object_t bitwise_or(object_t list);
object_t shift_left(object_t obj1, object_t obj2);
object_t equal(object_t obj1, object_t obj2);
object_t shift_right(object_t obj1, object_t obj2);
object_t less(object_t obj1, object_t obj2);
object_t gt(object_t obj1, object_t obj2);
object_t SIN(object_t obj1);
object_t COS(object_t obj1);
object_t ROUND(object_t obj1);
object_t SQRT(object_t obj1);

jmp_buf jmp_env;

void error(char *str, ...)
{
  printf("%s", str);
  longjmp(jmp_env, 1);
}

/**
 * Тест сложения
 */
void test_add()
{
    printf("test_add: ");
    int num1 = 1;
    int num2 = 2;
    int num3 = 3;
    object_t list = new_pair(new_number(num1),
                        new_pair(new_number(num2), 
                            new_pair(new_number(num3), NULLOBJ)));
    object_t res = add(list);
    ASSERT(get_value(res), 6);
}
/**
 * Тест сложения - проверка на NULL.
 */
void test_add_null()
{
    printf("test_add_null: ");
    object_t list = NULLOBJ;
    object_t res = add(list);
    ASSERT(get_value(res), 0); 
}

/**
 * Тест сложения передача значения не число
 */
void test_add_no_number()
{
    printf("test_add_no_number: ");
    int num = 2;
    if (setjmp(jmp_env) == 0) {
        object_t list = new_pair(new_number(num),
                            new_pair(new_pair(NULL, NULL), NULL));
        object_t res = add(list);
        FAIL;
    } else
        OK;
}
/**
 * Тест сложения чисел с плавающей точкой
 */
void test_add_float()
{
    printf("test_add_float: ");
    float num1 = 1.56f;
    float num2 = 2.67f;
    int num3 = 3;
    object_t list = new_pair(new_float(num1), 
                            new_pair(new_float(num2),
                                new_pair(new_number(num3), NULLOBJ)));
    object_t res = add(list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, 7.23f);
}

/**
 * Тест сложения чисел с плавающей запятой - передача значения не число
 */
void test_add_float_no_number()
{
    printf("test_add_float_no_number: ");
    float num = 2.0f;
    if (setjmp(jmp_env) == 0) {
        float sum = 0.0f;
        object_t list = new_pair(new_float(num),
                            new_pair(new_pair(NULL, NULL), NULL));
        object_t res = add_float(list, sum);
        FAIL;
    } else
        OK;
}

/**
 * Тест вычитания (- 10 3 2)
 */
void test_sub()
{
    printf("test_sub: ");
    int num1 = 10;
    int num2 = 3;
    int num3 = 2;
    object_t first = new_number(num1);
    object_t list = new_pair(new_number(num2), 
			     new_pair(new_number(num3), NULLOBJ));
    object_t res = sub(first, list);
    ASSERT(get_value(res), 5);
}

/**
 * Тест вычитания (- 10)
 */
void test_sub_one()
{
    printf("test_sub_one: ");
    int num = 10;
    object_t first = new_number(num);
    object_t list = NULLOBJ;
    object_t res = sub(first, list);
    ASSERT(get_value(res), -10);
}

/**
 * Тест вычитания (- 10)
 */
void test_sub_float_one()
{
    printf("test_sub_one: ");
    object_t first = new_float(10.0f);
    object_t list = NULLOBJ;
    object_t res = sub(first, list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, -10.0f);
}

//Тест вычитания - проверка на NULL.
void test_sub_null()
{
    printf("test_sub_null: ");
    if (setjmp(jmp_env) == 0) {
	object_t first = NULLOBJ;
        object_t list = NULLOBJ;
        object_t res = sub(first, list);
        FAIL;
    } else
        OK;
}

//Тест вычитания передача значения не число
void test_sub_no_number()
{
    printf("test_sub_no_number: ");
    int num = 2;
    if (setjmp(jmp_env) == 0) {
	object_t first = new_number(num);
        object_t list = new_pair(new_pair(NULLOBJ, NULLOBJ), NULLOBJ);
	object_t res = sub(first, list);
        FAIL;
    } else 
        OK;
}

/**
 * Тест вычитания чисел с плавающей запятой 
 */
void test_sub_float()
{
    printf("test_sub_float: ");
    float num1 = 10.26f;
    float num2 = 2.67f;
    float num3 = 5.11f;
    object_t first = new_float(num1);
    object_t list = new_pair(new_float(num2), 
			     new_pair(new_float(num3), NULLOBJ));
    object_t res = sub(first, list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, 2.48f);
}

//Тест вычитания чисел с плавающей запятой, целочисленное число
void test_sub_float_number()
{
    printf("test_sub_float_number: ");
    float num1 = 10.22f;
    float num2 = 2.11f;
    int num3 = 5;
    object_t first = new_float(num1);
    object_t list = new_pair(new_float(num2), 
			     new_pair(new_number(num3), NULLOBJ));
    object_t res = sub(first, list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, 3.11f)
}

//Тест вычитания чисел с плавающей запятой передача значения не число
void test_sub_float_no_number()
{
    printf("test_sub_float_no_number: ");
    float num = 2.11f;
    if (setjmp(jmp_env) == 0) {
	object_t first = new_float(num);
        object_t list = new_pair(new_pair(NULLOBJ, NULLOBJ), NULLOBJ);
        object_t res = sub(first, list);
        FAIL;
    } else 
        OK;
}

//Тест умножения.
void test_mul()
{
    printf ("test_mul:");
    int num1 = 1;
    int num2 = 2;
    int num3 = 3;

    object_t list = new_pair(new_number(num1),
			      new_pair(new_number(num2),
				       new_pair(new_number(num3), NULLOBJ)));
    
    object_t res = mul(list);
    ASSERT(get_value(res), 6); 
}


//Тест умножения, передача пустого списка.
void test_mul_empty_list()
{
    printf ("test_mul_empty_list:");
    object_t empty_list = NULLOBJ;
    object_t res = mul(empty_list);
    ASSERT(get_value(res), 1); 
}


// Тест умножения, передача списка содержащего символ.
void test_mul_list_with_symbol()
{
    printf ("test_mul_list_with_symbol :");
    int num = 1;
    if (setjmp(jmp_env) == 0) {
        object_t list_with_symbol = new_pair(new_number(num),
                                         new_pair(NEW_SYMBOL("a"), NULLOBJ));
        object_t res = mul(list_with_symbol);
        FAIL;
    } else 
        OK;
}

// Тест умножения чисел с плавающей точкой
void test_mul_float()
{
    printf ("test_mul_float:");
    float num1 = 1.0f;
    float num2 = 2.3f;
    int num3 = 3;

    object_t list = new_pair(new_float(num1),
			      new_pair(new_float(num2),
				       new_pair(new_number(num3), NULLOBJ)));
    
    object_t res = mul(list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, 6.9f); 
}

//Тест умножения, передача символа после числа с плавающей запятой.
void test_mul_symbol_after_float()
{
    float num1 = 5.5f;
    
    printf ("test_mul_symbol_after_float:");
    if (setjmp(jmp_env) == 0) {
        object_t list_with_symbol_after_float = new_pair(new_float(num1),
                                         new_pair(NEW_SYMBOL("a"), NULLOBJ));
        object_t res = mul(list_with_symbol_after_float);
        FAIL;
    } else 
        OK;
}

//Тест умножения, передача nullobj после числа с плавающей запятой.
void test_mul_nullobj_after_float()
{
    float num1 = 7.7f;
    
    printf ("test_mul_symbol_after_float:");
    if (setjmp(jmp_env) == 0) {
        object_t list_with_symbol_after_float = new_pair(new_float(num1),
                                         new_pair(new_pair(NULLOBJ, NULLOBJ), NULLOBJ));
        object_t res = mul(list_with_symbol_after_float);
        FAIL;
    } else 
        OK;
}

//Тест умножения, первый элемент списка number, следующий float.
void test_mul_float_after_integer()
{
    printf ("test_mul_float_after_integer:");
    int num1 = 2;
    float num2 = 2.34f;

    object_t list = new_pair(new_number(num1),
			     new_pair(new_float(num2), NULLOBJ));
     
    object_t res = mul(list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, 4.68f);
}

//Тест умножения, передача nullobj после целого числа.
void test_mul_nullobj_after_integer()
{
    int num1 = 2;
    
    printf ("test_mul_nullobj_after_integer:");
    if (setjmp(jmp_env) == 0) {
        object_t list_with_nullobj_after_integer = new_pair(new_number(num1),
                                         new_pair(new_pair(NULLOBJ, NULLOBJ), NULLOBJ));
        object_t res = mul(list_with_nullobj_after_integer);
        FAIL;
    } else 
        OK;
}

//Тест умножения, передача символа после целого числа.
void test_mul_symbol_after_integer()
{
    int num1 = 6;
    
    printf ("test_mul_symbol_after_integer:");
    if (setjmp(jmp_env) == 0) {
        object_t list_with_symbol_after_integer = new_pair(new_number(num1),
                                         new_pair(NEW_SYMBOL("a"), NULLOBJ));
        object_t res = mul(list_with_symbol_after_integer);
        FAIL;
    } else 
        OK;
}

//Тест умножения, передача символа в качестве первого элемента списка.
void test_mul_not_number_symbol()
{   
    printf ("test_mul_not_number:");

    float num1 = 3.28f;
    int num2 = 12;
    
    if (setjmp(jmp_env) == 0) {
        object_t list_with_symbol_only = new_pair(NEW_SYMBOL("G"),
						  new_pair(new_float(num1),
							   new_pair(new_number(num2), NULLOBJ)));
        object_t res = mul(list_with_symbol_only);
        FAIL;
    } else 
        OK;
}

//Тест умножения, передача NULLOBJ в качестве первого элемента списка.
void test_mul_not_number_nullobj()
{   
    printf ("test_mul_not_number:");

    int num1 = 4;
    float num2 = 8.4f;
    
    if (setjmp(jmp_env) == 0) {
        object_t list_with_nullobj = new_pair(new_pair(NULLOBJ, NULLOBJ),
					      new_pair(new_number(num1),
							   new_pair(new_float(num2), NULLOBJ)));
        object_t res = mul(list_with_nullobj);
        FAIL;
    } else 
        OK;
 }

 //Тест деления
void test_div()
{
    printf("test_div: \n");
    int num1 = 8;
    int num2 = 2;
    object_t first = new_number(num1);
    object_t list = new_pair(new_number(num2), NULLOBJ);
    object_t res = DIV(first, list);
    ASSERT(get_value(res), 4); 
}


/**
 * Тест пустого списка деления
 */
void test_div_nulllist()
{
    printf("test_div_nulllist: \n");
    if (setjmp(jmp_env) == 0) {
	object_t first = NULLOBJ;
        object_t list = NULLOBJ;
        object_t res = DIV(first, list); 
        FAIL;
    } else 
        OK;
}

/**
 * Тест нулевого делителя
 */

void test_div_zerodivisor()
{
    printf("test_div_zerodivisor: \n");
    int num1 = 8;
    int num2 = 0;
    if (setjmp(jmp_env) == 0) {
	object_t first = new_number(num1);
        object_t list = new_pair(new_number(num2),NULLOBJ);
        object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}


//Тест пустого делителя
 
void test_div_nulldivisor()
{
    printf("test_div_nulldivisor: \n");
    object_t first = new_float(8.0f);
    object_t list = NULLOBJ;
    object_t res =  DIV(first, list);
    ASSERT(GET_FLOAT(res)->value, 0.125f); 
}

 //Тест деления чисел с плавающей запятой, делимое - float, делитель - integer
void test_div_float_by_int()
{
    printf("test_div_float_by_int: \n");
    int num = 2;
    object_t first = new_float(7.5f);
    object_t list = new_pair(new_number(num), NULLOBJ);
    object_t res = DIV(first, list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, 3.75f);
}

 //Тест деления чисел с плавающей запятой, делимое - integer, делитель - float
void test_div_int_by_float()
{
    printf("test_div_int_by_float: \n");
    float num = 0.8f;
    object_t first = new_number(2);
    object_t list = new_pair(new_float(num), NULLOBJ);
    object_t res = DIV(first, list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, 2.5f);
}


//Тест деления чисел с плавающей запятой, делимое - float, делитель - float
void test_div_float_by_float()
{
    printf("test_div_float_by_float: \n");
    float num = 3.2f;
    object_t first = new_float(9.4f);
    object_t list = new_pair(new_float(num), NULLOBJ);
    object_t res = DIV(first, list);
    ASSERT_FLOAT(GET_FLOAT(res)->value, 2.9375f);
}

//Тест деления чисел, делимое - символ
void test_div_divisible_is_symbol()
{
    printf("test_div_divisible_is_symbol: \n");
    int num = 8;
    if (setjmp(jmp_env) == 0) {
	object_t first = NEW_SYMBOL("a");
        object_t list = new_pair(new_number(num), NULLOBJ);
        object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}

//Тест деления чисел, делитель - символ
void test_div_divider_is_symbol()
{
    printf("test_div_divider_is_symbol: \n");
    if (setjmp(jmp_env) == 0) {
	object_t first = new_number(9);
        object_t list = new_pair(NEW_SYMBOL("b"), NULLOBJ);
        object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}

//Тест деления чисел, делимое - nullobj
void test_div_divisible_is_nullobj()
{
    printf("test_div_divisible_is_nullobj: \n");
    int num = 7;
    if (setjmp(jmp_env) == 0) {
	object_t first = NULLOBJ;
        object_t list = new_pair(new_number(num), NULLOBJ);
        object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}

//Тест деления чисел, делитель - nullobj
void test_div_divider_is_nullobj()
{
    printf("test_div_divider_is_nullobj: \n");
    if (setjmp(jmp_env) == 0) {
	object_t first = new_number(9);
        object_t list = new_pair(new_pair(NULLOBJ, NULLOBJ), NULLOBJ);
	object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}

//Тест деления чисел с плавающей запятой, делитель - символ
void test_div_float_divider_is_symbol()
{
    printf("test_div_float_divider_is_symbol: \n");
    float num1 = 5.54f;
    if (setjmp(jmp_env) == 0) {
	object_t first = new_float(5.54f);
        object_t list = new_pair(NEW_SYMBOL("a"), NULLOBJ);
        object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}

//Тест деления чисел с плавающей запятой, делимое - символ
void test_div_float_divisible_is_symbol()
{
    printf("test_div_float_divisible_is_symbol: \n");
    float num1 = 2.34f;
    if (setjmp(jmp_env) == 0) {
	object_t first = NEW_SYMBOL("b");
	object_t list = new_pair(new_float(num1), NULLOBJ);
        object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}

//Тест деления чисел с плавающей запятой, делитель - nullobj
void test_div_float_divider_is_nullobj()
{
    printf("test_div_float_divider_is_nullobj: \n");
    float num1 = 3.76f;
    if (setjmp(jmp_env) == 0) {
	object_t first = new_float(3.76f);
        object_t list = new_pair(new_pair(NULLOBJ, NULLOBJ), NULLOBJ);
        object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}

//Тест деления чисел с плавающей запятой, делимое - nullobj
void test_div_float_divisible_is_nullobj()
{
    printf("test_div_float_divisible_is_nullobj: \n");
    float num1 = 2.34f;
    if (setjmp(jmp_env) == 0) {
	object_t first = NULLOBJ;
	object_t list = new_pair(new_float(num1), NULLOBJ);
        object_t res = DIV(first, list);
        FAIL;
    } else 
        OK;
}

/**
 * Тест побитового И
 */
void test_bitwise_and(int num1, int num2, int res)
{
    printf("test_bitwise_and: %d %d", num1, num2);
    object_t list = new_pair(new_number(num1),
			     new_pair(new_number( num2), NULLOBJ));
    object_t obj_res = bitwise_and(list);
    ASSERT(get_value(obj_res), res);
}

/**
* Тест проверка на NULL.
*/
void test_bitwise_and_null()
{
    printf("test_bitwise_and_null: ");
    if (setjmp(jmp_env) == 0) {
        object_t list = new_pair(NULLOBJ,
			     new_pair(NULLOBJ, NULLOBJ));
        object_t res = bitwise_and(list);
        FAIL;
    } else 
        OK;
}

/**1
 * Тест передача значения не число
 */
void test_bitwise_and_no_number()
{
    printf("test_bitwise_and_no_number: ");
    if (setjmp(jmp_env) == 0) {
        object_t list = new_pair(NEW_SYMBOL("G"), NULLOBJ);
        object_t res = bitwise_and(list);
        FAIL;
    } else 
        OK;
}

void test_bitwise_and_number_sym()
{
    printf("test_bitwise_and_number_sym: ");
    int num1 = 2;
    if (setjmp(jmp_env) == 0) {
        object_t list = new_pair(new_number(num1),
			     new_pair(NEW_SYMBOL("C"), NULLOBJ));
        object_t res = bitwise_and(list);
        FAIL;
    } else 
        OK;
}

/**
 * Тест побитового ИЛИ
 */
void test_bitwise_or(int num1, int num2, int res)
{
    printf("test_bitwise_or: %d %d", num1, num2);
    object_t list = new_pair(new_number(num1),
			     new_pair(new_number(num2), NULLOBJ));
    object_t obj_res = bitwise_or(list);
    ASSERT(get_value(obj_res), res);
}

/**
 * Тест побитового ИЛИ для пустого ввода
 */
void test_bitwise_or_null()
{
    printf("test_bitwise_or_null:");
    if (setjmp(jmp_env) == 0) {
        object_t list = NULLOBJ;
        object_t obj_res = bitwise_or(list);
        FAIL;
    } else 
        OK;
}

/**
 * Тест побитового ИЛИ для неверного ввода
 */
void test_bitwise_or_no_number()
{
    printf("test_bitwise_or_no_number:");
    int num1 = 8;
    if (setjmp(jmp_env) == 0) {
        object_t list = new_pair(new_number(num1),
			     new_pair(NEW_SYMBOL("T"), NULLOBJ));
        object_t obj_res = bitwise_or(list);
        FAIL;
    } else 
        OK;
}

/**
 * Тест сдвига влево
 */
void test_shift_left(int num1, int num2, int res)
{
    printf("test_shift_left: %d %d", num1, num2);
    object_t obj_res = shift_left(new_number(num1), new_number(num2));
    ASSERT(get_value(obj_res), res);
}

/**
 * Тест сдвига вправо
 */
void test_shift_right(int num1, int num2, int res)
{
    printf("test_shift_right: %d %d", num1, num2);
    object_t obj_res = shift_right(new_number(num1), new_number(num2));
    ASSERT(get_value(obj_res), res);
}


/**
 * Тест сдвига вправо, передача пустого списка
 */
void test_shift_right_empty_list()
{
    printf("test_shift_right_empty_list: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = shift_right(NULLOBJ, NULLOBJ);
        FAIL;
    } else 
        OK;
}

/** 
 * Сравнение чисел
 */
void test_equal()
{
    printf("test_equal: ");
    object_t res = equal(new_number(1), new_number(1));
    ASSERT(res, t);
}

/** 
 * Сравнение объектов, передача пустого списка
 */
void test_equal_empty_list()
{
    printf("test_equal_empty_list: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = equal(NULLOBJ, NULLOBJ);
        FAIL;
    } else 
        OK;
}

/** 
 * Сравнение объектов, список из одного объекта
 */
void test_equal_no_second_param()
{
    printf("test_equal_no_second_param: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = equal(new_number(1), NULLOBJ);
        FAIL;
    } else 
        OK;
}

/** 
 * Сравнение символов
 */
void test_equal_symbols()
{
    printf("test_equal_symbols: ");
    object_t res = equal(NEW_SYMBOL("A"), NEW_SYMBOL("A"));
    ASSERT(res, t)
}

/** 
 * Сравнение строк
 */
void test_equal_strings()
{
    printf("test_equal_strings: ");
    object_t res = equal(NEW_STRING("abc"), NEW_STRING("abc"));
    ASSERT(res, t);
}

/** 
 * Сравнение пар
 */
/* void test_equal_pairs() */
/* { */
/*     printf("test_equal_pairs: "); */
/*     object_t res = equal(new_pair(NEW_SYMBOL("A"), NULLOBJ), new_pair(NEW_SYMBOL("A"), NULLOBJ)); */
/*     ASSERT(res, t); */
/* } */

/** 
 * Сравнение пар с разными значениями
 */
void test_equal_pairs_with_different_values()
{
    printf("test_equal_pairs_with_different_values: ");
    object_t res = equal(new_pair(NEW_SYMBOL("A"), NULLOBJ), new_pair(NEW_SYMBOL("B"), NULLOBJ));
    ASSERT(res, nil);
}

/** 
 * Сравнение массивов
 */
void test_equal_array()
{
    printf("test_equal_array: ");
    array_t *arr = new_empty_array(2);
    arr->data[0] = new_number(1);
    arr->data[1] = new_number(1);
    object_t res = equal(NEW_OBJECT(ARRAY, arr), NEW_OBJECT(ARRAY, arr));
    ASSERT(res, t);
}

/** 
 * Сравнение массивов с неравными значениями элементов
 */
void test_equal_arrays_with_different_values()
{
    printf("test_equal_arrays_with_different_values: ");
    array_t *arr1 = new_empty_array(2);
    arr1->data[0] = new_number(1);
    array_t *arr2 = new_empty_array(2);
    arr2->data[0] = new_number(2);
    object_t res = equal(NEW_OBJECT(ARRAY, arr1), NEW_OBJECT(ARRAY, arr2));
    ASSERT(res, nil);
}

/** 
 * Сравнение массивов разной длины
 */
void test_equal_arrays_with_different_length()
{
    printf("test_equal_arrays_with_different_length: ");
    array_t *arr1 = new_empty_array(1);
    arr1->data[0] = new_number(1);
    array_t *arr2 = new_empty_array(2);
    arr2->data[0] = new_number(1);
    arr2->data[1] = new_number(1);
    object_t res = equal(NEW_OBJECT(ARRAY, arr1), NEW_OBJECT(ARRAY, arr2));
    ASSERT(res, nil);
}

/** 
 * Сравнение пустых объектов
 */
void test_equal_null_objects()
{
    printf("test_equal_array: ");
    object_t res = equal(NULLOBJ, NULLOBJ);
    ASSERT(res, t)
}

/** 
 * Сравнение объектов, один из объектов пустой
 */
void test_equal_one_object_is_null()
{
    printf("test_equal_one_object_is_null: ");
    object_t list = new_pair(new_number(1),
			     new_pair(NULLOBJ, NULLOBJ));
    object_t res = equal(new_number(1), NULLOBJ);
    ASSERT(res, nil);
}

/** 
 * Сравнение объектов разных типов
 */
void test_equal_different_types()
{
    printf("test_equal_different_types: ");
    object_t res = equal(new_number(1), NEW_SYMBOL("A"));
    ASSERT(res, nil);
}

/** 
 * Сравнение объектов char
 */
void test_equal_objects_are_char()
{
    printf("test_equal_objects_are_char: ");
    object_t res = equal(NEW_CHAR('A'), NEW_CHAR('B'));
    ASSERT(res, nil);
}

/** 
 * Сравнение объектов float
 */
void test_equal_objects_are_float()
{
    printf("test_equal_objects_are_float: ");
    float num1 = 5.2f;
    float num2 = 5.2f;
    object_t res = equal(new_float(num1), new_float(num2));
    ASSERT(res, t);
}

/**
 * Тест сравнения неравенства меньше для двух чисел
 */
void test_less()
{
    printf("test_less: ");
    int first1 = 1;
    int second1 = 2;
    object_t res = less(new_number(first1), new_number(second1));
    ASSERT(res, t);
}

/**
 * Тест сравнения неравенства меньше для двух чисел - проверка если первое число будет больше
 */
void test_less_great()
{
    printf("test_less_great: ");
    int first1 = 2;
    int second1 = 1;
    object_t list = new_pair(new_number(first1),
			     new_pair(new_number(second1), NULLOBJ));
    object_t res = less(new_number(first1), new_number(second1));
    ASSERT(res, NULLOBJ);
}

/**
 * Тест сравнения неравнества меньше для двух чисел, первое - int, второе - float, проверка если первое число будет больше
 */
void test_less_float_great()
{
    printf("test_less_great: ");
    float first1 = 2.6f;
    float second1 = 1.5f;
    
    object_t list = new_pair(new_float(first1),
			     new_pair(new_float(second1), NULLOBJ));
    object_t res = less(new_float(first1), new_float(second1));
    ASSERT(res, NULLOBJ);
}


/*Тест сравнения чисел на больше*/
void test_gt(int num1, int num2, object_t token) 
{
    printf("test_gt:");
    object_t res = gt(new_number(num1), new_number(num2));
    ASSERT(res, token);
}

/*
arith
---------------------------+------------------------------------------------+------------------------------------------------------+
| Условие                   | Правильный                                     | Неправильный                                         |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция add               |1)список целых чисел                            |53)нет аргументов                                     |
|                           |2)список десятичных чисел                       |55)не число                                           |
|                           |                                                |                                                      |
|                           |                                                |57)один аргумент                                      |
|                           |                                                |58)слишком большое число                              |
|                           |                                                |59)слишком маленькое число                            |
|                           |                                                |60)числа, сумма которых слишком большая               |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция sub               |3)список целых чисел                            |68)нет аргументов                                     |
|                           |4)список десятичных чисел                       |70)не число                                           |
|                           |                                                |                                                      |
|                           |                                                |72)один аргумент                                      |
|                           |                                                |73)слишком большое число                              |
|                           |                                                |74)слишком маленькое число                            |
|                           |                                                |75)числа, разность которых слишком маленькая          |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция mul               |5)список целых чисел                            |83)нет аргументов                                     |
|                           |6)список десятичных чисел                       |85)не число                                           |
|                           |                                                |                                                      |
|                           |                                                |87)один аргумент                                      |
|                           |                                                |88)слишком большое число                              |
|                           |                                                |89)слишком маленькое число                            |
|                           |                                                |90)числа, произведение которых слишком большое        |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция div               |7)список целых чисел                            |97)нет аргументов                                     |
|                           |8)список десятичных чисел                       |99)не число                                           |
|                           |                                                |                                                      |
|                           |                                                |101)один аргумент                                     |
|                           |                                                |102)слишком большое число                             |
|                           |                                                |103)слишком маленькое число                           |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция mod               |9)список целых чисел                            |104)нет аргументов                                    |
|                           |                                                |105)десятичное число                                  |
|                           |                                                |106)не число                                          |
|                           |                                                |                                                      |
|                           |                                                |108)один аргумент                                     |
|                           |                                                |109)слишком большое число                             |
|                           |                                                |110)слишком маленькое число                           |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция gt                |10)список целых чисел                           |111)нет аргументов                                    |
|                           |                                                |112)десятичное число                                  |
|                           |                                                |113)не число                                          |
|                           |                                                |                                                      |
|                           |                                                |115)один аргумент                                     |
|                           |                                                |116)слишком большое число                             |
|                           |                                                |117)слишком маленькое число                           |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция less              |11)список целых чисел                           |118)нет аргументов                                    |
|                           |                                                |119)десятичное число                                  |
|                           |                                                |120)не число                                          |
|                           |                                                |                                                      |
|                           |                                                |122)один аргумент                                     |
|                           |                                                |123)слишком большое число                             |
|                           |                                                |124)слишком маленькое число                           |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция compare_obj       |12)список целых чисел                           |125)меньше двух аргументов                            |
|                           |13)список строк                                 |126)слишком большая строка                            |
|                           |14)список символов                              |                                                      |
|                           |                                                |128)слишком большое число                             |
|                           |                                                |129)слишком маленькое число                           |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция equal             |15)список целых чисел                           |130)меньше двух аргументов                            |
|                           |16)список строк                                 |131)больше двух аргументов                            |
|                           |17)список символов                              |                                                      |
|                           |                                                |133)слишком большое число                             |
|                           |                                                |134)слишком маленькое число                           |
|                           |                                                |135)слишком большая строка                            |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция bitwise_and       |18)список целых чисел                           |136)меньше двух аргументов                            |
|                           |                                                |137)десятичное число                                  |
|                           |                                                |138)не число                                          |
|                           |                                                |                                                      |
|                           |                                                |                                                      |
|                           |                                                |141)слишком большое число                             |
|                           |                                                |142)слишком маленькое число                           |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция bitwise_or        |19)список целых чисел                           |143)меньше двух аргументов                            |
|                           |                                                |144)десятичное число                                  |
|                           |                                                |145)не число                                          |
|                           |                                                |                                                      |
|                           |                                                |                                                      |
|                           |                                                |148)слишком большое число                             |
|                           |                                                |149)слишком маленькое число                           |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция shift_left        |20)список целых чисел                           |150)меньше двух аргументов                            |
|                           |                                                |151)десятичное число                                  |
|                           |                                                |152)не число                                          |
|                           |                                                |                                                      |
|                           |                                                |                                                      |
|                           |                                                |155)слишком большое число                             |
|                           |                                                |156)слишком маленькое число                           |
|                           |                                                |157)больше двух аргументов                            |
---------------------------+------------------------------------------------+------------------------------------------------------+
| функция shift_right       |21)список целых чисел                           |158)меньше двух аргументов                            |
|                           |                                                |159)десятичное число                                  |
|                           |                                                |160)не число                                          |
|                           |                                                |                                                      |
|                           |                                                |                                                      |
|                           |                                                |163)слишком большое число                             |
|                           |                                                |164)слишком маленькое число                           |
|                           |                                                |165)больше двух аргументов                            |
---------------------------+------------------------------------------------+------------------------------------------------------+
*/

int main()
{
    printf("------------test_arith---------\n");
    init_regions();
    init_objects();
    t = NEW_SYMBOL("T");
    nil = NULLOBJ;
    test_add();
    test_add_null();
    test_add_no_number();
    test_add_float();
    test_add_float_no_number();
    test_sub();
    test_sub_one();
    test_sub_float_one();
    test_sub_null();
    test_sub_no_number();
    test_sub_float();
    test_sub_float_number();
    test_sub_float_no_number();
    test_mul();
    test_mul_empty_list();
    test_mul_list_with_symbol();
    test_mul_float();
    test_mul_symbol_after_float();
    test_mul_nullobj_after_float();
    test_mul_float_after_integer();
    test_mul_nullobj_after_integer();
    test_mul_symbol_after_integer();
    test_mul_not_number_symbol();
    test_mul_not_number_nullobj();
    test_div();
    test_div_nulllist();
    test_div_zerodivisor();
    test_div_nulldivisor();
    test_div_float_by_int();
    test_div_int_by_float();
    test_div_float_by_float();
    test_div_divisible_is_symbol();
    test_div_divider_is_symbol();
    test_div_divisible_is_nullobj();
    test_div_divider_is_nullobj();
    test_div_float_divisible_is_symbol();
    test_div_float_divider_is_symbol();
    test_div_float_divisible_is_nullobj();
    test_div_float_divider_is_nullobj();
    test_bitwise_and(0xA, 2, 2);
    test_bitwise_and(0xB, 2, 2);
    test_bitwise_and(0xA, 5, 0);
    test_bitwise_and_null();
    test_bitwise_and_no_number();
    test_bitwise_and_number_sym();
    test_bitwise_or(0xA, 5, 0xF);//1010|101
    test_bitwise_or(0, 0, 0);
    test_bitwise_or_null();
    test_bitwise_or_no_number();
    test_shift_left(2, 3, 16); //10000
    test_shift_right(10, 2, 2);
    test_equal();
    test_equal_symbols();
    test_equal_strings();
    test_equal_array();
    test_equal_arrays_with_different_length();
    test_equal_arrays_with_different_values();
    test_equal_null_objects();
    test_equal_one_object_is_null();
    test_equal_different_types();
    test_equal_objects_are_char();
    test_equal_objects_are_float();
    test_less();
    test_less_great();
    test_less_float_great();
    test_gt(5, 3, t);
    test_gt(3, 5, NULLOBJ);
    return 0;
}
