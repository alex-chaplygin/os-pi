#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "test.h"
#include "parser.h"
#include "string.h"

extern object_t *t;
extern object_t *nil;

object_t *add(object_t *list);
object_t *sub(object_t *list);
object_t *mul(object_t *list);
object_t *int_div(object_t *list);
object_t *num_eq(object_t *list);
object_t *bitwise_and(object_t *list);
object_t *bitwise_or(object_t *list);
object_t *shift_left(object_t *list);
object_t *shift_right(object_t *list);
object_t *equal(object_t *list);
object_t *less(object_t *list);
object_t *gt(object_t *list);

void error(char *str, ...)
{
  printf("%s", str);
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
    object_t *list = new_pair(object_new(NUMBER, &num1),
                        new_pair(object_new(NUMBER, &num2), 
                            new_pair(object_new(NUMBER, &num3), NULL)));
    object_t *res = add(list);
    ASSERT(res->u.value, 6);
}

/**
 * Тест сложения - проверка на NULL.
 */
void test_add_null()
{
    printf("test_add_null: ");
    object_t *list = NULL;
    object_t *res = add(list);
    ASSERT(res, ERROR);
}

/**
 * Тест сложения передача значения не число
 */
void test_add_no_number()
{
    printf("test_add_no_number: ");

    int num = 2;
    object_t *list = new_pair(object_new(NUMBER, &num),
			      new_pair(new_pair(NULL, NULL), NULL));
    object_t *res = add(list);
    ASSERT(res, ERROR);
}

/**
 * Тест вычитания 
 */
void test_sub()
{
    printf("test_sub: ");
    int num1 = 10;
    int num2 = 3;
    int num3 = 2;
    object_t *list = new_pair(object_new(NUMBER, &num1),
                        new_pair(object_new(NUMBER, &num2), 
                            new_pair(object_new(NUMBER, &num3), NULL)));
    object_t *res = sub(list);
    ASSERT(res->u.value, 5);
}

/**
 * Тест вычитания - проверка на NULL.
 */
void test_sub_null()
{
    printf("test_sub_null: ");
    object_t *list = NULL;
    object_t *res = sub(list);
    ASSERT(res, ERROR);
}

/**
 * Тест вычитания передача значения не число
 */
void test_sub_no_number()
{
    printf("test_sub_no_number: ");

    int num = 2;
    object_t *list = new_pair(object_new(NUMBER, &num),
			      new_pair(new_pair(NULL, NULL), NULL));
    object_t *res = sub(list);
    ASSERT(res, ERROR);
}

/**
 * Тест умножения.
 */
void test_mul()
{
    printf ("test_mul:");
    int num1 = 1;
    int num2 = 2;
    int num3 = 3;

    object_t *list = new_pair(object_new(NUMBER, &num1),
			      new_pair(object_new(NUMBER, &num2),
				       new_pair(object_new(NUMBER, &num3), NULL)));
    
    object_t *res = mul(list);
    ASSERT(res->u.value, 6); 
}

/**
 * Тест умножения, передача пустого списка.
 */
void test_mul_empty_list()
{
    printf ("test_mul_empty_list:");
    object_t *empty_list = NULL;
    object_t *res = mul(empty_list);
    ASSERT(res, ERROR);
}

/**
 * Тест умножения, передача списка содержащего символ.
 */
void test_mul_list_with_symbol()
{
    printf ("test_mul_list_with_symbol :");
    int num = 1;
    object_t *list_with_symbol = new_pair(object_new(NUMBER, &num),
                                         new_pair(object_new(SYMBOL, "A"), NULL));
    object_t *res = mul(list_with_symbol);
    ASSERT(res, ERROR);
}

/**
 * Тест сравнения чисел
 */
void test_num_eq(int num1, int num2, object_t *token)
{

    printf ("test_num_eq:");

    object_t *list = new_pair(object_new(NUMBER, &num1),
			      new_pair(object_new(NUMBER, &num2), NULL));
    
    object_t *res = num_eq(list);
    ASSERT(res, token);   
}

/**
 * Тест деления
 */
void test_div()
{
    printf("test_div: \n");
    int num1 = 8;
    int num2 = 2;
    object_t *list = new_pair(object_new(NUMBER, &num1),
                        new_pair(object_new(NUMBER, &num2), NULL));
    object_t *res = int_div(list);
    ASSERT(res->u.value, 4);
}


/**
 * Тест пустого списка деления
 */
void test_div_nulllist()
{
    printf("test_div_nulllist: \n");
    object_t *list = NULL;
    object_t *res = int_div(list);
    ASSERT(res,ERROR);
}

/**
 * Тест нулевого делителя
 */
void test_div_zerodivisor()
{
    printf("test_div_zerodivisor: \n");
    int num1 = 8;
    int num2 = 0;
    object_t *list =  new_pair(object_new(NUMBER,&num1),
			      new_pair(object_new(NUMBER,&num2),NULL));
    object_t *res =  int_div(list);
    ASSERT(res,ERROR);
}

/**
 * Тест пустого делителя
 */
void test_div_nulldivisor()
{
    printf("test_div_nulldivisor: \n");
    int num1 = 8;
    object_t *list = new_pair(object_new(NUMBER, &num1), NULL);
    object_t *res =  int_div(list);
    ASSERT(res,ERROR);
}

/**
 * Тест побитового И
 */
void test_bitwise_and(int num1, int num2, int res)
{
    printf("test_bitwise_and: %d %d", num1, num2);
    object_t *list = new_pair(object_new(NUMBER, &num1),
			      new_pair(object_new(NUMBER, &num2), NULL));
    object_t *obj_res = bitwise_and(list);
    ASSERT(obj_res->u.value, res);
}

/**
* Тест проверка на NULL.
*/
void test_bitwise_and_null()
{
    printf("test_bitwise_and_null: ");
    object_t *list = NULL;
    object_t *res = bitwise_and(list);
    ASSERT(res, ERROR);
}

/**1
 * Тест передача значения не число
 */
void test_bitwise_and_no_number()
{
    printf("test_bitwise_and_no_number: ");
    object_t *list = new_pair(object_new(SYMBOL, "G"), NULL);
    object_t *res = bitwise_and(list);
    ASSERT(res, ERROR);
}

void test_bitwise_and_number_sym()
{
    printf("test_bitwise_and_number_sym: ");
    int num1 = 2;
    object_t *list = new_pair(object_new(NUMBER, &num1),
			      new_pair(object_new(SYMBOL, "C"), NULL));
    object_t *res = bitwise_and(list);
    ASSERT(res, ERROR);
}

/**
 * Тест побитового ИЛИ
 */
void test_bitwise_or(int num1, int num2, int res)
{
    printf("test_bitwise_or: %d %d", num1, num2);
    object_t *list = new_pair(object_new(NUMBER, &num1),
			      new_pair(object_new(NUMBER, &num2), NULL));
    object_t *obj_res = bitwise_or(list);
    ASSERT(obj_res->u.value, res);
}

/**
 * Тест побитового ИЛИ для пустого ввода
 */
void test_bitwise_or_null()
{
    printf("test_bitwise_or_null:");
    object_t *list = NULL;
    object_t *obj_res = bitwise_or(list);
    ASSERT(obj_res, ERROR);
}

/**
 * Тест побитового ИЛИ для неверного ввода
 */
void test_bitwise_or_no_number()
{
    printf("test_bitwise_or_no_number:");
    int num1 = 8;
    object_t *list = new_pair(object_new(NUMBER, &num1),
			      new_pair(object_new(SYMBOL, "T"), NULL));
    object_t *obj_res = bitwise_or(list);
    ASSERT(obj_res, ERROR);
}

/**
 * Тест сдвига влево
 */
void test_shift_left(int num1, int num2, int res)
{
    printf("test_shift_left: %d %d", num1, num2);
    object_t *list = new_pair(object_new(NUMBER, &num1),
			      new_pair(object_new(NUMBER, &num2), NULL));
    object_t *obj_res = shift_left(list);
    ASSERT(obj_res->u.value, res);
}

/**
 * Тест сдвига влево, передача пустого списка
 */
void test_shift_left_empty_list()
{
    printf("test_shift_left_empty_list: ");
    object_t *res = shift_left(NULL);
    ASSERT(res, ERROR);
}

/**
 * Тест сдвига влево, передача списка без второго параметра
 */
void test_shift_left_no_second_param()
{
    printf("test_shift_left_no_second_param: ");
    int num = 1;
    object_t *list = new_pair(object_new(NUMBER, &num), NULL);
    object_t *res = shift_left(list);
    ASSERT(res, ERROR);
}

/**
 * Тест сдвига вправо
 */
void test_shift_right(int num1, int num2, int res)
{
    printf("test_shift_right: %d %d", num1, num2);
    object_t *list = new_pair(object_new(NUMBER, &num1),
			      new_pair(object_new(NUMBER, &num2), NULL));
    object_t *obj_res = shift_right(list);
    ASSERT(obj_res->u.value, res);
}

/**
 * Тест сдвига вправо, передача списка без второго параметра
 */
void test_shift_right_no_second_param()
{
    printf("test_shift_right_no_second_param: ");
    int num = 1;
    object_t *list = new_pair(object_new(NUMBER, &num), NULL);
    object_t *res = shift_right(list);
    ASSERT(res, ERROR);
}

/**
 * Тест сдвига вправо, передача пустого списка
 */
void test_shift_right_empty_list()
{
    printf("test_shift_right_empty_list: ");
    object_t *res = shift_right(NULL);
    ASSERT(res, ERROR);
}

/** 
 * Сравнение чисел
 */
void test_equal()
{
    printf("test_equal: ");
    int first1 = 1;
    int second1 = 1;
    object_t *list = new_pair(object_new(NUMBER, &first1),
			      new_pair(object_new(NUMBER, &second1), NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение объектов, передача пустого списка
 */
void test_equal_empty_list()
{
    printf("test_equal_empty_list: ");
    object_t *list = NULL;
    object_t *res = equal(list);
    ASSERT(res, ERROR);
}

/** 
 * Сравнение объектов, список из одного объекта
 */
void test_equal_no_second_param()
{
    printf("test_equal_no_second_param: ");
    int first1 = 1;
    object_t *list = new_pair(object_new(NUMBER, &first1), NULL);
    object_t *res = equal(list);
    ASSERT(res, ERROR);
}

/** 
 * Сравнение объектов, недопустимая длина списка
 */
void test_equal_invalid_list_length()
{
    printf("test_equal_invalid_list_length: ");
    int first1 = 1;
    int second1 = 1;
    object_t *list = new_pair(object_new(NUMBER, &first1),
			      new_pair(object_new(NUMBER, &second1), 
			      new_pair(object_new(NUMBER, &second1), NULL)));
    object_t *res = equal(list);
    ASSERT(res, ERROR);
}

/** 
 * Сравнение символов
 */
void test_equal_symbols()
{
    printf("test_equal_symbols: ");
    object_t *list = new_pair(object_new(SYMBOL, "A"),
			      new_pair(object_new(SYMBOL, "A"), NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение строк
 */
void test_equal_strings()
{
    printf("test_equal_strings: ");
    object_t *list = new_pair(object_new(STRING, "abc"),
			      new_pair(object_new(STRING, "abc"), NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение пар
 */
void test_equal_pairs()
{
    printf("test_equal_pairs: ");
    object_t *pair = new_pair(object_new(SYMBOL, "A"), NULL);
    object_t *list = new_pair(pair,
                    new_pair(pair, NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение пар с разными значениями
 */
void test_equal_pairs_with_different_values()
{
    printf("test_equal_pairs_with_different_values: ");
    object_t *pair1 = new_pair(object_new(SYMBOL, "A"), NULL);
    object_t *pair2 = new_pair(object_new(SYMBOL, "B"), NULL);
    object_t *list = new_pair(pair1,
                    new_pair(pair2, NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение массивов
 */
void test_equal_array()
{
    printf("test_equal_array: ");
    int num = 1;
    array_t *arr = new_empty_array(2);
    arr->data[0] = object_new(NUMBER, &num);
    arr->data[1] = object_new(NUMBER, &num);
    object_t *list = new_pair(object_new(ARRAY, arr),
			      new_pair(object_new(ARRAY, arr), NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение массивов с неравными значениями элементов
 */
void test_equal_arrays_with_different_values()
{
    printf("test_equal_arrays_with_different_values: ");
    int num1 = 1;
    int num2 = 2;
    array_t *arr1 = new_empty_array(2);
    arr1->data[0] = object_new(NUMBER, &num1);
    array_t *arr2 = new_empty_array(2);
    arr2->data[0] = object_new(NUMBER, &num2);
    object_t *list = new_pair(object_new(ARRAY, arr1),
			      new_pair(object_new(ARRAY, arr2), NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение массивов разной длины
 */
void test_equal_arrays_with_different_length()
{
    printf("test_equal_arrays_with_different_length: ");
    int num = 1;
    array_t *arr1 = new_empty_array(1);
    arr1->data[0] = object_new(NUMBER, &num);
    array_t *arr2 = new_empty_array(2);
    arr2->data[0] = object_new(NUMBER, &num);
    arr2->data[1] = object_new(NUMBER, &num);
    object_t *list = new_pair(object_new(ARRAY, arr1),
			      new_pair(object_new(ARRAY, arr2), NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение пустых объектов
 */
void test_equal_null_objects()
{
    printf("test_equal_array: ");
    object_t *list = new_pair(NULL,
			      new_pair(NULL, NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение объектов, один из объектов пустой
 */
void test_equal_one_object_is_null()
{
    printf("test_equal_one_object_is_null: ");
    int num = 1;
    object_t *list = new_pair(object_new(NUMBER, &num),
			      new_pair(NULL, NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/** 
 * Сравнение объектов разных типов
 */
void test_equal_different_types()
{
    printf("test_equal_different_types: ");
    int num = 1;
    object_t *list = new_pair(object_new(NUMBER, &num),
			      new_pair(object_new(SYMBOL, "A"), NULL));
    object_t *res = equal(list);
    ASSERT(res, t);
}

/**
 * Тест сравнения неравнества меньше для двух чисел
 */
void test_less()
{
    printf("test_less: ");
    int first1 = 1;
    int second1 = 2;
    object_t *list = new_pair(object_new(NUMBER, &first1),
			      new_pair(object_new(NUMBER, &second1), NULL));
    object_t *res = less(list);
    ASSERT(res, t);
}

/**
 * Тест сравнения неравнества меньше для двух чисел - проверка если первое число будет больше
 */
void test_less_great()
{
    printf("test_less_great: ");
    int first1 = 2;
    int second1 = 1;
    object_t *list = new_pair(object_new(NUMBER, &first1),
			      new_pair(object_new(NUMBER, &second1), NULL));
    object_t *res = less(list);
    ASSERT(res, NULL);
}

/**
 * Тест сравнения неравнества меньше для двух чисел - проверка на отсутствие агрументов
 */
void test_less_no_arguments()
{
    printf("test_less_no_arguments: ");
    object_t *list = NULL;
    object_t *res = less(list);
    ASSERT(res, ERROR);
}

/**
 * Тест сравнения неравнества меньше для двух чисел - проверка на только один аргумент
 */
void test_less_one_argument()
{
    printf("test_less_one_argument: ");
    int first1 = 1;
    object_t *list = new_pair(object_new(NUMBER, &first1), NULL);
    object_t *res = less(list);
    ASSERT(res, ERROR);
}

/**
 * Тест сравнения чисел на больше
 */
void test_gt(int num1, int num2, object_t *token) 
{
    printf("test_gt:");
    object_t *list = new_pair(object_new(NUMBER, &num1), 
                       new_pair(object_new(NUMBER, &num2), NULL));
    object_t *res = gt(list);
    ASSERT(res, token);
}

/**
 * Тест сравнения чисел на больше при пустом входном списке 
 */
void test_gt_list_is_null()
{
    printf("test_gt_list_is_null:");
    object_t *list = NULL;
    object_t *res = gt(list);
    ASSERT(res, ERROR);
}

/**
 * Тест сравнения чисел на больше при одном аргументе в списке 
 */
void test_gt_one_arg(int num1)
{
    printf("test_gt_one_arg:");
    object_t *list = new_pair(object_new(NUMBER, &num1), NULL);
    object_t *res = gt(list);
    ASSERT(res, ERROR);
}

int main()
{
    printf("------------test_arith---------\n");
    init_regions();
    test_add();
    test_add_null();
    test_add_no_number();
    test_sub();
    test_sub_null();
    test_sub_no_number();
    test_mul();
    test_mul_empty_list();
    test_mul_list_with_symbol();
    test_div();
    test_div_nulllist();
    test_div_zerodivisor();
    test_div_nulldivisor();
    test_num_eq(1, 2, nil);
    test_num_eq(10, 10, t);
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
    test_shift_left(1, 2, 4); //100
    test_shift_left(2, 3, 16); //10000
    test_shift_left_empty_list();
    test_shift_left_no_second_param();
    test_shift_right(1, 1, 0);
    test_shift_right(10, 2, 2);
    test_shift_right_empty_list();
    test_shift_right_no_second_param();
    test_equal();
    test_equal_empty_list();
    test_equal_no_second_param();
    test_equal_invalid_list_length();
    test_equal_symbols();
    test_equal_strings();
    test_equal_pairs();
    test_equal_pairs_with_different_values();
    test_equal_array();
    test_equal_arrays_with_different_length();
    test_equal_arrays_with_different_values();
    test_equal_null_objects();
    test_equal_one_object_is_null();
    test_equal_different_types();
    test_less();
    test_less_great();
    test_less_no_arguments();
    test_less_one_argument();
    test_gt(5, 3, t);
    test_gt(3, 5, nil);
    test_gt_list_is_null();
    test_gt_one_arg(3);
    return 0;
}
