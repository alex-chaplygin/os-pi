#include <stdlib.h>
#include <string.h>
#include "objects.h"
#include "eval.h"
#include "symbols.h"
#include "parser.h"

extern object_t t;
extern object_t nil;

/** 
 * Сложение чисел с плавающей точкой
 *
 * @param list - список чисел (1.56 2 3.67)
 * @param sum - начальная сумма
 *
 * @return сумму
 */
object_t add_float(object_t list, float sum)
{
    while(list != NULLOBJ) {
	object_t first = FIRST(list);
	if (TYPE(first) == FLOAT)
	    sum += GET_FLOAT(first)->value;
	else if(IS_NUMBER(first))
	    sum += get_value(first);
	else
	    error("add: Not number");
	list = TAIL(list);
    }
    return new_float(sum);
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
    int num = 0;
    while (list != NULLOBJ) {
	object_t first = FIRST(list);
	if (IS_NUMBER(first)) {  
	    num += get_value(first);
	    list = TAIL(list);
	}
	else if(TYPE(first) == FLOAT)
	    return add_float(list, num);
	else
	    error("add: Not number");
    }
    return new_number(num);
}

/** 
 * Вычитание чисел с плавающей точкой
 *
 * @param list - список чисел (2.56 1 0.03)
 * @param sub - начальная разность
 *
 * @return разность
 */
object_t sub_float(object_t list, float sub)
{
    while(list != NULLOBJ) {
	object_t first = FIRST(list);
	if (TYPE(first) == FLOAT)
	    sub -= GET_FLOAT(first)->value;
	else if(IS_NUMBER(first))
	    sub -= get_value(first);
	else
	    error("sub: Not number");
	list = TAIL(list);
    }
    return new_float(sub);
}

/**
 * Вычитание аргументов (- 1 2 3)
 * 
 * @param list - список чисел (1 2 3)
 * 
 * @return разность
 */
object_t sub(object_t first, object_t list)
{ 
    int num;
    if (list == NULLOBJ && TYPE(first) == FLOAT)
	return new_float(-GET_FLOAT(first)->value);
    if (TYPE(first) == FLOAT) 
	return sub_float(list, GET_FLOAT(first)->value);
    else if (IS_NUMBER(first))
	num = get_value(first);
    else
	error("sub: Not number");
    if (list == NULLOBJ)
	num = -num;
    while (list != NULLOBJ) {
	first = FIRST(list);
	if (IS_NUMBER(first)) {  
	    num -= get_value(first);
	    list = TAIL(list);
	}
	else if(TYPE(first) == FLOAT)
	    return sub_float(list, num);
	else
	    error("sub: Not number");
    }
    return new_number(num);
}

/**
 * Умножение чисел с плавающей точкой
 * 
 * @param list - список чисел (1 2.2 3)
 * @param mul - начальное произведение
 * 
 * @return произведение
 */
object_t mul_float(object_t list, float mul)
{
    while(list != NULLOBJ) {
	object_t first = FIRST(list);
	if (TYPE(first) == FLOAT)
	    mul *= GET_FLOAT(first)->value;
	else if(IS_NUMBER(first))
	    mul *= get_value(first);
	else
	    error("mul: Not number");
	list = TAIL(list);
    }
    return new_float(mul);
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
    int num = 1;
    while(list != NULLOBJ){
	object_t first = FIRST(list);
	if(IS_NUMBER(first)){
	    num *= get_value(first);
	    list = TAIL(list);
	}
	else if(TYPE(first) == FLOAT)
	    return mul_float(list, num);
	else
	    error("mul: Not number");
    }
    return new_number(num);
}

/**
 * Деление с плавающей точкой
 * 
 * @param first - делимое
 * @param second - делитель
 *
 * @return результат от деления
 */
object_t div_float(object_t list, float div)
{
    while(list != NULLOBJ) {
	object_t first = FIRST(list);
	if (TYPE(first) == FLOAT)
	    div /= GET_FLOAT(first)->value;
	else if(IS_NUMBER(first))
	    div /= get_value(first);
	else
	    error("div: Not number");
	list = TAIL(list);
    }
    return new_float(div);
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
    int num;
    if (list == NULLOBJ && TYPE(first) == FLOAT)
	return new_float(1.0f / GET_FLOAT(first)->value);
    if (TYPE(first) == FLOAT) 
	return div_float(list, GET_FLOAT(first)->value);
    else if (IS_NUMBER(first))
	num = get_value(first);
    else
	error("div: Not number");
    if (list == NULLOBJ)
	num = 1 / num;
    while (list != NULLOBJ) {
	first = FIRST(list);
	if (IS_NUMBER(first)) {
	    int d = get_value(first);
	    if (d == 0)
		error("div: divisor = 0");
	    num /= d;
	    list = TAIL(list);
	}
	else if(TYPE(first) == FLOAT)
	    return div_float(list, num);
	else
	    error("div: Not number");
    }
    return new_number(num);
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
    if (obj1 == NULLOBJ || obj2 == NULLOBJ)
        return 1;
    if (!IS_NUMBER(obj1) || !IS_NUMBER(obj2))
	return 1;
    if (get_value(obj2) != 0) {
        int num = get_value(obj1) % get_value(obj2);
        return new_number(num);
    } else 
        return 1;
}

/**
 * Сравнение на больше (> 8 2)
 *
 * @param obj1 - первое число
 * @param obj2 - второе число
 *
 * @return результат сравнения (t или nil)
 */
object_t gt(object_t obj1, object_t obj2)
{
    float first, second;
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
	return (first > second) ? t : nil;
}

/**
 * Сравнение на меньше (< 8 2)
 *
 * @param obj1 - первое число
 * @param obj2 - второе число
 *
 * @return результат сравнения (t или nil)
 */
object_t less(object_t obj1, object_t obj2)
{
	float first, second;
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
 * Побитовое И (& 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return результат побитового И
 */
object_t bitwise_and(object_t list)
{
    if (!IS_NUMBER(FIRST(list))) 
	error("bitwise_and: Not a number");
    int num = get_value(FIRST(list));
    list = TAIL(list);
    while (list != NULLOBJ) {
	object_t first = FIRST(list);
	if (IS_NUMBER(first)) {  
	    num &= get_value(first);
	    list = TAIL(list);
	}
	else
	    error("bitwise_and: Not number");
    }
    return new_number(num);
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
    if (list == NULLOBJ)
	error("bitwise_or: no arguments");
    int num = get_value(FIRST(list));
    list = TAIL(list);
    while (list != NULLOBJ) {
	object_t first = FIRST(list);
	if (IS_NUMBER(first)) {  
	    num |= get_value(first);
	    list = TAIL(list);
	}
	else
	    error("bitwise_or: Not number");
    }
    return new_number(num);
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
    if (list == NULLOBJ)
	error("bitwise_xor: no arguments");
    int num = get_value(FIRST(list));
    list = TAIL(list);
    while (list != NULLOBJ) {
	object_t first = FIRST(list);
	if (IS_NUMBER(first)) {  
	    num ^= get_value(first);
	    list = TAIL(list);
	}
	else
	    error("bitwise_xor: Not number");
    }
    return new_number(num);
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
    if (obj1 == NULLOBJ || obj2 == NULLOBJ)
        return 1;
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
    if (obj1 == NULLOBJ || obj2 == NULLOBJ)
        return 1;
    int num = get_value(obj1) >> get_value(obj2);
    return new_number(num);
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
	return 1;
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
	return 1;
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
    if (obj1 == NULLOBJ)
        return 1;
    if (TYPE(obj1) != FLOAT)
	return 1;
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
    if (obj1 == NULLOBJ)
        return 1;
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
    register_func("SIN", SIN, 0, 1);
    register_func("COS", COS, 0, 1);
    register_func("ROUND", ROUND, 0, 1);
    register_func("SQRT", SQRT, 0, 1);
}
