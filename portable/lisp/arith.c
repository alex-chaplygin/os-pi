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
    if (list == NULLOBJ)
	error("add: no arguments");
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
object_t sub(object_t list)
{ 
    if (list == NULLOBJ)
        error("sub: no arguments");
    object_t first = FIRST(list);
    int num;
    if (TYPE(first) == FLOAT) 
	return sub_float(TAIL(list), GET_FLOAT(first)->value);
    else if (IS_NUMBER(first))
	num = get_value(FIRST(list));
    else
	error("sub: Not number");
    list = TAIL(list);
    while (list != NULLOBJ) {
	object_t first = FIRST(list);
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
    if (list == NULLOBJ)
	error("mul: no arguments");
    object_t first = FIRST(list);
    int num;
    if (TYPE(first) == FLOAT) 
	return mul_float(TAIL(list), GET_FLOAT(first)->value);
    else if (IS_NUMBER(first))
	num = get_value(FIRST(list));
    else
	error("mul: Not number");
    list = TAIL(list);
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
object_t div_float(object_t first, object_t second)
{
    float f;
    float s;
#define GET_F(o, v)\
    if (TYPE(o) == FLOAT)\
	v = GET_FLOAT(o)->value;\
    else if (IS_NUMBER(o))\
	v = (float)get_value(o);\
    else\
	error("div: Not number");
    GET_F(first, f);
    GET_F(second, s);
    return new_float(f / s);  
}

/**
 * Деление (/ 8 2)
 * 
 * @param list - список чисел (8 2)
 *
 * @return результат от деления
 */
object_t DIV(object_t list){

    if (list == NULLOBJ)
        error("div: no arguments");
    if (TAIL(list) == NULLOBJ)
        error("div: no divisor");
    object_t first = FIRST(list);
    object_t second = SECOND(list);
    if(TYPE(first) == FLOAT || TYPE(second) == FLOAT)
	return div_float(first, second);
    if (get_value(second) != 0) {		
	if (!IS_NUMBER(first))
	    error("div: Not number");
	if (!IS_NUMBER(second))
	    error("div: Not number");
	int num = get_value(first) / get_value(second);
	return new_number(num);
    } else 
        error("div: divisor = 0");
}

/**
 * Остаток от деления (% 8 2)
 * 
 * @param list - список чисел (8 2)
 *
 * @return остаток от деления
 */
object_t mod(object_t list){

    if (list == NULLOBJ)
        error("mod: no arguments");
    if (TAIL(list) == NULLOBJ)
        error("mod: no modisor");
    object_t first = FIRST(list);
    if (!IS_NUMBER(first))
	 error("mod: Not number");
    object_t second = SECOND(list);
    if (!IS_NUMBER(first))
	 error("mod: Not number");
    if (get_value(second) != 0) {
        int num = get_value(first) % get_value(second);
        return new_number(num);
    } else 
        error("mod: divisor = 0");
}

/* Сравнение на больше (> 8 2) */

/* @param list - список чисел (8 2) */

/* @return результат от сравнения */

object_t gt(object_t list)
{
    if (list == NULLOBJ)
        error(">: no arguments");
    if (TAIL(list) == NULLOBJ)
        error(">: one argument");
    object_t first = FIRST(list);
    object_t second = SECOND(list);
    if (IS_NUMBER(first))
	return (get_value(first) >  get_value(second)) ? t : NULLOBJ;
    else if (TYPE(first) == FLOAT && TYPE(second) == FLOAT)
	return (GET_FLOAT(first)->value > GET_FLOAT(second)->value) ? t : NULLOBJ;
    else
      return NULLOBJ;
}

/* Сравнение на меньше (< 8 2) */

/* @param list - список чисел (8 2) */

/* @return результат от сравнения */
object_t less(object_t list)
{
    if (list == NULLOBJ)
        error("<: no arguments");
    if (TAIL(list) == NULLOBJ)
        error("<: one argument");
    object_t first = FIRST(list);
    object_t second = SECOND(list);
    if (IS_NUMBER(first))
	return (get_value(first) <  get_value(second)) ? t : NULLOBJ;
    else if (TYPE(first) == FLOAT && TYPE(second) == FLOAT)
	return (GET_FLOAT(first)->value < GET_FLOAT(second)->value) ? t : NULLOBJ;
    else
      return NULLOBJ;
}

/**
 * Функция сравнения объектов
 * возвращает 1 если значения объектов равны, иначе 0
 */
int compare_obj(object_t obj1, object_t obj2)
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

/** 
 * Сравнение аргументов по значению (equal 1 2)
 *
 * @param list - список любых двух объектов (1 2)
 *
 * @return T - если равно, иначе NIL
 */
object_t equal(object_t list)
{
    if (list == NULLOBJ)
        error("equal: no arguments");
    object_t obj1 = FIRST(list);
    if (TAIL(list) == NULLOBJ)
        error("equal: no second argument");
    object_t obj2 = SECOND(list);
    if (TAIL(TAIL(list)) != NULLOBJ)
        error("equal: too many arguments");
    return compare_obj(obj1, obj2) ? t : nil;
}

/** 
 * Побитовое И (& 1 2 3)
 *
 * @param list - список чисел (1 2 3)
 *
 * @return побитовое И
 */
object_t bitwise_and(object_t list)
{
    if (list == NULLOBJ)
	error("bitwise_and: no arguments");
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
 * @return побитовое ИЛИ
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
	else {
	    error("bitwise_or: Not number");
	    return ERROR;
	}
    }
    return new_number(num);
}

/**
 * Побитовый сдвиг влево (<< 8 2)
 * 
 * @param list - список (<Любое число> <Число бит>)
 *
 * @return результат сдвига
 */
object_t shift_left(object_t list)
{
    if (list == NULLOBJ)
        error("shift_left: no arguments");
    if (TAIL(list) == NULLOBJ)
        error("shift_left: no second param");
    object_t first = FIRST(list);
    object_t second = SECOND(list);
    int num = get_value(first) << get_value(second);
    return new_number(num);
}

/**
 * Побитовый сдвиг вправо (>> 8 2)
 * 
 * @param list - список (<Любое число> <Число бит>)
 *
 * @return результат сдвига
 */
object_t shift_right(object_t list)
{
    if (list == NULLOBJ)
        error("shift_right: no arguments");
    if (TAIL(list) == NULLOBJ)
        error("shift_right: no second param");
    object_t first = FIRST(list);
    object_t second = SECOND(list);
    int num = get_value(first) >> get_value(second);
    return new_number(num);
}

float sinf(float v);
float cosf(float v);
float roundf(float v);
float sqrtf(float v);

/**
 * Вычисление синуса
 *
 * @param list - список аргументов
 *
 * @return результат вычисления
 */
object_t SIN(object_t list)
{
    object_t arg = FIRST(list);
    float num = 0;
    if(TYPE(arg) != FLOAT)
	error("sin: invalid argument");
    else
	return new_float(sinf(GET_FLOAT(arg)->value));
}

/**
 * Вычисление косинуса
 *
 * @param list - список аргументов
 *
 * @return результат вычисления
 */
object_t COS(object_t list)
{
    object_t arg = FIRST(list);
    float num = 0;
    if(TYPE(arg) != FLOAT)
	error("cos: invalid argument");
    else
	return new_float(cosf(GET_FLOAT(arg)->value));
}

/**
 * Округление числа с плавающей точкой до целого
 *
 * @param list - список аргументов
 *
 * @return результат вычисления
 */
object_t ROUND(object_t list)
{
    if (list == NULLOBJ)
        error("ROUND: no arguments");
    object_t arg = FIRST(list);
    if (TYPE(arg) != FLOAT)
	error("ROUND: invalid argument");
    else {
	int v = (int)roundf(GET_FLOAT(arg)->value);
	return new_number(v);
    }
}

/**
 * Вычисление квадратного корня
 *
 * @param list - список аргументов (одно число)
 *
 * @return квадратный корень числа
 */
object_t SQRT(object_t list)
{
    if (list == NULLOBJ)
        error("SQRT: no arguments");

    object_t arg = FIRST(list);
    float num;

    if (TYPE(arg) == FLOAT) {
        num = GET_FLOAT(arg)->value;
    } else if (IS_NUMBER(arg)) {
        num = (float)get_value(arg);
    } else
        error("SQRT: invalid argument");
    if (num < 0) 
        error("SQRT: negative number");

    return new_float(sqrtf(num));
}

/** 
 * Инициализация арифметических функций
 */
void init_arith()
{
    register_func("+", add);
    register_func("-", sub);
    register_func("*", mul);
    register_func("EQUAL", equal);
    register_func("/", DIV);
    register_func("%", mod);
    register_func("&", bitwise_and);
    register_func("BITOR", bitwise_or);
    register_func("<<", shift_left);
    register_func(">>", shift_right);
    register_func(">", gt);
    register_func("<", less);
    register_func("SIN", SIN);
    register_func("COS", COS);
    register_func("ROUND", ROUND);
    register_func("SQRT", SQRT);
}
