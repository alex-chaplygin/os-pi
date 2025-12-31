#include <stdlib.h>
#include <string.h>
#include "objects.h"
#include "eval.h"
#include "symbols.h"
#include "parser.h"

extern object_t t;
extern object_t nil;

 /** 
 * Сложение двух чисел (+ 1 2)
 *
 * @param first_num - первое число
 * @param second_num - второе число
 *
 * @return результат сложения 
 */
 object_t add2(object_t first_num, object_t second_num)
 {
    if (IS_NUMBER(first_num) && IS_NUMBER(second_num))
        return new_number(get_value(first_num) + get_value(second_num));
    else if (TYPE(first_num) == FLOAT && TYPE(second_num) == FLOAT) 
	return new_float(GET_FLOAT(first_num)->value + GET_FLOAT(second_num)->value);
    else if (TYPE(first_num) == FLOAT && IS_NUMBER(second_num))
	return new_float(GET_FLOAT(first_num)->value + (float)get_value(second_num));
    else if (IS_NUMBER(first_num) && TYPE(second_num) == FLOAT)
        return new_float((float)get_value(first_num) + GET_FLOAT(second_num)->value);
    else
	error("add: Not number");
 }

/** 
 * Сложение аргументов (+ 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return сумму
 */
object_t add(object_t list)
{
    object_t res = new_number(0);
    while (list != NULLOBJ) {
        res = add2(res, FIRST(list));
        list = TAIL(list);
    }
    return res;
}

 /** 
 * Вычитание двух чисел (- 1 2)
 *
 * @param first_num - первое число
 * @param second_num - второе число
 *
 * @return результат вычитания 
 */
 object_t sub2(object_t first_num, object_t second_num)
 {
    if (IS_NUMBER(first_num) && IS_NUMBER(second_num))
        return new_number(get_value(first_num) - get_value(second_num));
    else if (TYPE(first_num) == FLOAT && TYPE(second_num) == FLOAT) 
	return new_float(GET_FLOAT(first_num)->value - GET_FLOAT(second_num)->value);
    else if (TYPE(first_num) == FLOAT && IS_NUMBER(second_num))
	return new_float(GET_FLOAT(first_num)->value - (float)get_value(second_num));
    else if (IS_NUMBER(first_num) && TYPE(second_num) == FLOAT)
        return new_float((float)get_value(first_num) - GET_FLOAT(second_num)->value);
    else
	error("sub: Not number");
 }

/**
 * Вычитание аргументов (- 1 2 3)
 * 
 * @param first - первое число (1)
 * @param list - список чисел (2 3)
 * 
 * @return разность
 */
object_t sub(object_t first, object_t list)
{
    object_t res;
    if (list == NULLOBJ && TYPE(first) == FLOAT)
	return new_float(-GET_FLOAT(first)->value);
    else if (list == NULLOBJ && IS_NUMBER(first))
	return new_number(-get_value(first));
    else if (!IS_NUMBER(first) && TYPE(first) != FLOAT)
	error("sub: Not number");
    else
	res = first;
    while (list != NULLOBJ) {
	res = sub2(res, FIRST(list));
        list = TAIL(list);
    }
    return res;
}

 /** 
 * Умножение двух чисел (* 1 2)
 *
 * @param first_num - первое число
 * @param second_num - второе число
 *
 * @return результат умножения 
 */
 object_t mul2(object_t first_num, object_t second_num)
 {
    if (IS_NUMBER(first_num) && IS_NUMBER(second_num))
        return new_number(get_value(first_num) * get_value(second_num));
    else if (TYPE(first_num) == FLOAT && TYPE(second_num) == FLOAT) 
	return new_float(GET_FLOAT(first_num)->value * GET_FLOAT(second_num)->value);
    else if (TYPE(first_num) == FLOAT && IS_NUMBER(second_num))
	return new_float(GET_FLOAT(first_num)->value * (float)get_value(second_num));
    else if (IS_NUMBER(first_num) && TYPE(second_num) == FLOAT)
        return new_float((float)get_value(first_num) * GET_FLOAT(second_num)->value);
    else
	error("mul: Not number");
 }

/**
 * Умножение аргументов (* 1 2 3)
 * 
 * @param list - список чисел (1 2 3)
 * 
 * @return произведение
 */
object_t mul(object_t list)
{
    object_t num = new_number(1);
    while(list != NULLOBJ){
	num = mul2(num, FIRST(list));
	list = TAIL(list);
    }
    return num;
}

 /** 
 * Деление двух чисел (/ 8 2)
 *
 * @param first_num - первое число
 * @param second_num - второе число
 *
 * @return результат деления 
 */
 object_t DIV2(object_t first_num, object_t second_num)
 {
    if ((IS_NUMBER(second_num) && get_value(second_num) == 0)
	|| (TYPE(second_num) == FLOAT && GET_FLOAT(second_num)->value == 0))
	error("div: divisor = 0");
    if (IS_NUMBER(first_num) && IS_NUMBER(second_num))
        return new_number(get_value(first_num) / get_value(second_num));
    else if (TYPE(first_num) == FLOAT && TYPE(second_num) == FLOAT) 
	return new_float(GET_FLOAT(first_num)->value / GET_FLOAT(second_num)->value);
    else if (TYPE(first_num) == FLOAT && IS_NUMBER(second_num))
	return new_float(GET_FLOAT(first_num)->value / (float)get_value(second_num));
    else if (IS_NUMBER(first_num) && TYPE(second_num) == FLOAT)
        return new_float((float)get_value(first_num) / GET_FLOAT(second_num)->value);
    else
	error("div: Not number");
 }

/**
 * Деление (/ 8 2)
 * 
 * @param list - список чисел (8 2)
 *
 * @return результат от деления
 */
object_t DIV(object_t first, object_t list)
{
    object_t num;
    if (list == NULLOBJ && TYPE(first) == FLOAT)
	return new_float(1.0f / GET_FLOAT(first)->value);
    if (IS_NUMBER(first) || TYPE(first) == FLOAT)
	num = first;
    else
	error("div: Not number");
    if (list == NULLOBJ)
	num = 1 / num;
    while (list != NULLOBJ) {
	first = FIRST(list);
	num = DIV2(num, FIRST(list));
	list = TAIL(list);
    }
    return num;
}

/**
 * Остаток от деления (% 8 2)
 * 
 * @param obj1, obj2 - числа (8, 2)
 *
 * @return остаток от деления
 */
object_t mod(object_t obj1, object_t obj2)
{
    if(!IS_NUMBER(obj1))
	error("mod: obj1 is not a whole number");
    else if(!IS_NUMBER(obj2))
	error("mod: obj2 is not a whole number");
    int first = get_value(obj1);
    int second = get_value(obj2);
    if(second == 0)
	error("mod: division by zero");
    int result = first % second;
    if(first * second < 0)
	result += second;
    return new_number(result);
}

/**
 * Сравнение на больше (> 8 2)
 *
 * @param obj1 - первое число
 * @param obj2 - второе число
 *
 * @return результат от сравнения
 */
object_t gt(object_t obj1, object_t obj2)
{
    float first, second;
    if(IS_NUMBER(obj1) && (IS_NUMBER(obj2)))
	return (get_value(obj1) > get_value(obj2)) ? t : nil;
    // Определение значения первого аргумента
    if (IS_NUMBER(obj1))
	first = get_value(obj1);
    else if (TYPE(obj1) == FLOAT)
	first = GET_FLOAT(obj1)->value;
    else
	error("gt: obj1 is not a number");
    // Определение значения второго аргумента
    if (IS_NUMBER(obj2))
	second = get_value(obj2);
    else if (TYPE(obj2) == FLOAT)
	second = GET_FLOAT(obj2)->value;
    else
	error("gt: obj2 is not a number");
    return (first > second) ? t : nil;
}

/**
 * Сравнение на меньше (< 8 2)
 *
 * @param obj1 - первое число
 * @param obj2 - второе число
 *
 * @return результат от сравнения (t или nil)
 */
object_t less(object_t obj1, object_t obj2)
{
    float first, second;
    if(IS_NUMBER(obj1) && (IS_NUMBER(obj2)))
	return (get_value(obj1) < get_value(obj2)) ? t : nil;
    // Определение значения первого аргумента
    if (IS_NUMBER(obj1))
	first = get_value(obj1);
    else if (TYPE(obj1) == FLOAT)
	first = GET_FLOAT(obj1)->value;
    else
	error("less: obj1 is not a number");
    // Определение значения второго аргумента
    if (IS_NUMBER(obj2))
	second = get_value(obj2);
    else if (TYPE(obj2) == FLOAT)
	second = GET_FLOAT(obj2)->value;
    else
	error("less: obj2 is not a number");
    return (first < second) ? t : nil;
}

/**
 * Функция сравнения объектов
 * 
 * @param obj1, obj2 - объекты для сравнения
 *
 * @return 1 если значения объектов равны, иначе 0
 */
object_t compare_obj(object_t obj1, object_t obj2) 
{
    if (obj1 == NULLOBJ && obj2 == NULLOBJ)
	return 1;
    else if (obj1 == NULLOBJ || obj2 == NULLOBJ)
	return 0;
    if (TYPE(obj1) != TYPE(obj2))
	return 0;
    else if (TYPE(obj1) == SYMBOL)
	return GET_SYMBOL(obj1) == GET_SYMBOL(obj2);
    else if (TYPE(obj1) == CHAR)
	return GET_CHAR(obj1) == GET_CHAR(obj2);
    else if (TYPE(obj1) == STRING)
	return strcmp(GET_STRING(obj1)->data, GET_STRING(obj2)->data) == 0;
    else if (IS_NUMBER(obj1))
	return get_value(obj1) == get_value(obj2);
    else if (TYPE(obj1) == FLOAT)
	return GET_FLOAT(obj1)->value == GET_FLOAT(obj2)->value;
    else if (TYPE(obj1) == PAIR) {
	if (!compare_obj(FIRST(obj1), FIRST(obj2)))
	    return 0;
	else
	    return compare_obj(TAIL(obj1), TAIL(obj2));
    } else if (TYPE(obj1) == ARRAY) {
    array_t *arr1 = GET_ARRAY(obj1);
    array_t *arr2 = GET_ARRAY(obj2);
	if (arr1->length != arr2->length)
	    return 0;
	for (int i = 0; i < arr1->length; i++)
	    if (!compare_obj(arr1->data[i], arr2->data[i]))
		return 0;	    
	return 1;
    } else
	error("equal: unknown object types");
}

object_t equal(object_t obj1, object_t obj2)
{
    return compare_obj(obj1, obj2) ? t : nil;
}

/**
 * Побитовое И для 2-ух аргументов
 *
 * @param obj1 - число 1
 * @param obj2 - число 2
 *
 * @return результат побитового И
 */
object_t bitwise_and2(object_t obj1, object_t obj2)
{
    if (!IS_NUMBER(obj1) || !IS_NUMBER(obj2))
	error("bitwise_and: Not a number");
    return new_number(get_value(obj1) & get_value(obj2));
}

/** 
 * Побитовое И (& 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return результат побитового И
 */
object_t bitwise_and(object_t list)
{
    object_t num = new_number(0xFFFFFFFF);
    while (list != NULLOBJ) {
	object_t obj2 = FIRST(list);
	num = bitwise_and2(num, obj2);
	list = TAIL(list);
    }
    return num;
}

/**
 * Побитовое ИЛИ для 2-ух аргументов
 *
 * @param obj1 - число 1
 * @param obj2 - число 2
 *
 * @return результат побитового ИЛИ
 */
object_t bitwise_or2(object_t obj1, object_t obj2)
{
    if (!IS_NUMBER(obj1) || !IS_NUMBER(obj2))
	error("bitwise_or: Not a number");
    return new_number(get_value(obj1) | get_value(obj2));
}

/** 
 * Побитовое ИЛИ (bitor 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return результат побитового ИЛИ
 */
object_t bitwise_or(object_t list)
{
    object_t num = new_number(0);
    while (list != NULLOBJ) {
	object_t obj2 = FIRST(list);
	num = bitwise_or2(num, obj2);
	list = TAIL(list);
    }
    return num;
}

/**
 * Побитовое исключающее ИЛИ для 2-ух аргументов
 *
 * @param obj1 - число 1
 * @param obj2 - число 2
 *
 * @return результат побитового исключающее ИЛИ
 */
object_t bitwise_xor2(object_t obj1, object_t obj2)
{
    if (!IS_NUMBER(obj1) || !IS_NUMBER(obj2))
	error("bitwise_xor: Not a number");
    return new_number(get_value(obj1) ^ get_value(obj2));
}

/** 
 * Побитовое исключающее ИЛИ (^ 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return побитовое исключающее ИЛИ
 */
object_t bitwise_xor(object_t list)
{
    object_t num = new_number(0);
    while (list != NULLOBJ) {
	object_t obj2 = FIRST(list);
	num = bitwise_xor2(num, obj2);
	list = TAIL(list);
    }
}

/**
 * Побитовый сдвиг влево (<< 8 2)
 * 
 * @param obj1 - любое число
 * @param obj2 - число бит
 *
 * @return результат сдвига
 */
object_t shift_left(object_t obj1, object_t obj2)
{
    if (!IS_NUMBER(obj1) || !IS_NUMBER(obj2))
	error("<<: Not a number");
    int num = get_value(obj1) << get_value(obj2);
    return new_number(num);
}

/**
 * Побитовый сдвиг вправо (>> 8 2)
 * 
 * @param obj1 - любое число
 * @param obj2 - число бит
 *
 * @return результат сдвига
 */
object_t shift_right(object_t obj1, object_t obj2)
{
    if (!IS_NUMBER(obj1) || !IS_NUMBER(obj2))
	error(">>: Not a number");
    int num = get_value(obj1) >> get_value(obj2);
    return new_number(num);
}

/**
 * Побитовое не
 *
 * @param obj1 - число
 *
 * @return результат вычисления
 */
object_t bitwise_not(object_t obj)
{
    if (!IS_NUMBER(obj))
	error("bitwise_not: Not number");
    return new_number(~(get_value(obj)));
}

float sinf(float v);
float cosf(float v);
float roundf(float v);
float sqrtf(float v);

/**
 * Вычисление синуса
 *
 * @param obj1 - число
 *
 * @return результат вычисления
 */
object_t SIN(object_t obj1)
{
    float num = 0;
    if(TYPE(obj1) != FLOAT)
	error("sin: not float");
    else
	return new_float(sinf(GET_FLOAT(obj1)->value));
}

/**
 * Вычисление косинуса
 *
 * @param obj1 - число
 *
 * @return результат вычисления
 */
object_t COS(object_t obj1)
{
    float num = 0;
    if(TYPE(obj1) != FLOAT)
	error("cos: not float");
    else
	return new_float(cosf(GET_FLOAT(obj1)->value));
}

/**
 * Округление числа с плавающей точкой до целого
 *
 * @param obj1 - число
 *
 * @return результат вычисления
 */
object_t ROUND(object_t obj1)
{
    if (TYPE(obj1) != FLOAT)
	error("round: not float");
    else {
	int v = (int)roundf(GET_FLOAT(obj1)->value);
	return new_number(v);
    }
}

/**
 * Вычисление квадратного корня
 *
 * @param obj1 - число
 *
 * @return квадратный корень числа
 */
object_t SQRT(object_t obj1)
{
    if (TYPE(obj1) != FLOAT)
	error("sqrt: not float");
    float num;
    if (TYPE(obj1) == FLOAT) {
        num = GET_FLOAT(obj1)->value;
    } else if (IS_NUMBER(obj1)) {
        num = (float)get_value(obj1);
    } else
        return 1;
    if (num < 0) 
       return 1;

    return new_float(sqrtf(num));
}

/** 
 * Инициализация арифметических функций
 */
void init_arith()
{
    register_func("+", add, 1, 0);
    register_func("-", sub, 1, 1);
    register_func("*", mul, 1, 0);
    register_func("EQUAL", equal, 0, 2);
    register_func("/", DIV, 1, 1);
    register_func("%", mod, 0, 2);
    register_func("&", bitwise_and, 1, 0);
    register_func("BITOR", bitwise_or, 1, 0);
    register_func("^", bitwise_xor, 1, 0);
    register_func("<<", shift_left, 0, 2);
    register_func(">>", shift_right, 0, 2);
    register_func(">", gt, 0, 2);
    register_func("<", less, 0, 2);
    register_func("~", bitwise_not, 0, 1);
    register_func("SIN", SIN, 0, 1);
    register_func("COS", COS, 0, 1);
    register_func("ROUND", ROUND, 0, 1);
    register_func("SQRT", SQRT, 0, 1);
}
