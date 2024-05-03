#include <stdio.h>
#include <setjmp.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "test.h"
#include "parser.h"
#include "pair.h"
#include "string.h"
#include "alloc.h"

extern object_t t;
extern object_t nil;
extern object_t current_env;

object_t  make_env(object_t args, object_t values);
int find_in_env(object_t env, object_t sym, object_t *res);
int is_lambda(object_t list);
void append_env(object_t l1, object_t l2);
object_t setq(object_t params);
object_t atom(object_t params);
object_t defvar(object_t params);
object_t cond(object_t list);
object_t progn(object_t list);
object_t defun(object_t list);
object_t quote(object_t list);
object_t eq(object_t list);
object_t and(object_t list);
object_t or(object_t list);
object_t backquote(object_t list);
object_t defmacro(object_t list);
object_t eval_symbol(object_t list);
object_t macro_call(object_t macro, object_t args, object_t env);
object_t eval_func(object_t lambda, object_t args, object_t env);
object_t eval_args(object_t args, object_t env);

jmp_buf jmp_env;

void error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
}
    
/**
 * Создать пары объеектов типа(t/nil,number)
 * (cond (nil 1)
        (T 2)  )
 * Обработать их функцией cond и проверить результат
*/
void test_cond()
{
    printf("test_cond: ");
    object_t p1 = new_pair(nil, new_pair(new_number(1), NULLOBJ)); 
    object_t p2 = new_pair(t, new_pair(new_number(2), NULLOBJ)); 
    object_t l = new_pair(p1, new_pair(p2, NULLOBJ)); 
    object_t res = cond(l); 
    ASSERT(TYPE(res), NUMBER); 
    ASSERT(get_value(res), 2); 
}

/**
 * cond без параметров
*/
void test_cond_null()
{
    printf("test_cond_null: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = cond(NULLOBJ); 
        FAIL;
    } else 
        OK;
}

/**
 * cond с неправильным параметром (1)
*/
void test_cond_tail_null()
{
    printf("test_cond_tail_null: ");
    object_t single_param_list = new_pair(new_number(1), NULLOBJ); 
    object_t cond_expr = new_pair(single_param_list, NULLOBJ); 
    if (setjmp(jmp_env) == 0) { 
        object_t result = cond(cond_expr); 
        FAIL;
    } else 
        OK;
}

/**
 * Создать выражение (cond ((eq 'a 'a) (eq 'a 'a) 'first))
 * Вычислить его и проверить результат на ошибку
*/
void test_cond_many_params()
{
    printf("test_cond_many_params: \n");
    object_t a = new_pair(NEW_SYMBOL("QUOTE"), new_pair(NEW_SYMBOL("A"), NULLOBJ)); 
    object_t eq_pair = new_pair(NEW_SYMBOL("EQ"), new_pair(a, new_pair(a, NULLOBJ))); 
    object_t first = new_pair(NEW_SYMBOL("QUOTE"), new_pair(NEW_SYMBOL("FIRST"), NULLOBJ)); 
    object_t args = new_pair(eq_pair, new_pair(eq_pair, new_pair(first, NULLOBJ))); 
    object_t cond_pair = new_pair(NEW_SYMBOL("COND"), new_pair(args, NULLOBJ));
    if (setjmp(jmp_env) == 0) { 
        object_t res = eval(cond_pair, NULLOBJ); 
        FAIL;
    } else 
        OK;
}

/**
 * Создать объект для выражения (lambda (a, b) (atom (car (a, b))))
 * Вызвать функцию is_lambda
 * Проверить результат = 1
 */
void test_is_lambda()
{
    printf("test_is_lambda: "); 
    object_t p1 = NEW_SYMBOL("a"); 
    object_t p2 = NEW_SYMBOL("b"); 
    object_t params = new_pair(p1, new_pair(p2, NULLOBJ)); 
    object_t q = new_pair(NEW_SYMBOL("ATOM"), new_pair(NEW_SYMBOL("CAR"), new_pair(params, NULLOBJ))); 
    object_t list = new_pair(NEW_SYMBOL("LAMBDA"), new_pair(params, new_pair(q, NULLOBJ))); 
    if (setjmp(jmp_env) == 0) {
        object_t i = is_lambda(list); 
        OK;
    } else
        FAIL;
}

object_t create_env()
{
    int num1 = 1;
    int num2 = 2;
    object_t arg_y = NEW_SYMBOL("Y"); 
    object_t arg_x = NEW_SYMBOL("X"); 
    object_t arg_1 = new_number(num1); 
    object_t arg_2 = new_number(num2); 
    object_t args = new_pair(arg_x, new_pair(arg_y, NULLOBJ)); 
    object_t values = new_pair(arg_1, new_pair(arg_2, NULLOBJ)); 
    return make_env(args, values); 
}


/**
 * Создать окружение
 * аргументы (x y)
 * значения (1 2)
 * Проверить результат = ((X.1) (Y.2))
 */
void test_make_env()
{
    printf("test_make_env: "); 
    object_t env = create_env(); 
    object_t p1 = FIRST(env); 
    object_t p2 = SECOND(env);   
    PRINT(env);
    ASSERT(TYPE(p1), PAIR); 
    ASSERT(TYPE(p2), PAIR); 
    ASSERT(GET_SYMBOL(FIRST(p1)), find_symbol("X")); 
    ASSERT(get_value(TAIL(p1)), 1); 
    ASSERT(GET_SYMBOL(FIRST(p2)), find_symbol("Y")); 
    ASSERT(get_value(TAIL(p2)), 2); 
}

/**
 * Создать окружение
 * аргументы (x y)
 * значения (1 2)
 * Проверить переменную Y
 */
void test_find_in_env()
{
    printf("test_find_in_env: ");
    object_t env = create_env();
    object_t res;
    PRINT(env);
    int result = find_in_env(env, NEW_SYMBOL("Y"), &res);
    ASSERT(result, 1);
    ASSERT(TYPE(res), NUMBER);
    ASSERT(get_value(res), 2);
}

/**
 * Создать функцию (defun null (x) (eq x '()))
 * Проверить значение символа null
 */
void test_defun()
{
    printf("test_defun: "); 
    object_t body = new_pair(NEW_SYMBOL("EQ"), new_pair(NEW_SYMBOL("X"), new_pair(NULLOBJ, NULLOBJ))); 
    object_t args = new_pair(NEW_SYMBOL("X"), NULLOBJ); 
    object_t res = defun(new_pair(NEW_SYMBOL("NULL"), new_pair(args, new_pair(body, NULLOBJ)))); 
    ASSERT(TYPE(res), SYMBOL); 
    symbol_t *null = find_symbol("NULL"); 
    ASSERT(GET_SYMBOL(res), null); 
    ASSERT(TYPE(null->lambda), PAIR); 
    ASSERT(GET_SYMBOL(FIRST(null->lambda)), find_symbol("LAMBDA"));
}

/**
 * Создать окружение с числовыми переменными X, Y
 * Вызвать функцию setq для изменения значения Y на 1010
 * Проверить что значение Y = 1010
 */
void test_setq_set_env()
{
    printf("test_setq_set_env: ");
    object_t env = create_env();
    object_t p2 = SECOND(env);
    int num = 1010;
    object_t num_obj = new_number(num);
    object_t params = new_pair(FIRST(p2), new_pair(num_obj, NULLOBJ));
    current_env = env;
    object_t setq_res = setq(params);
    object_t obj_in_env;
    find_in_env(current_env, FIRST(p2), &obj_in_env);
    ASSERT(get_value(setq_res), num);
    ASSERT(setq_res, num_obj);
    ASSERT(get_value(setq_res), get_value(obj_in_env));
    
    if (setjmp(jmp_env) == 0) { //проверка выхода из error при подаче неверных данных
        object_t obj_null = setq(NULLOBJ);
        FAIL;
    } else 
        OK;
}

/* /\** */
/*  * Создать глобальную числовую переменную test_var */
/*  * Вызвать функцию setq для изменения значения test_var на 992 */
/*  * Проверить что значение test_var = 992 */
/*  *\/ */
/* void test_setq_global_set() */
/* { */
/*     printf("test_setq_global_set: "); */
/*     object_t env = create_env(); */
/*     object_t p1 = FIRST(env); */
/*     object_t p2 = SECOND(env);   */
/*     int num = 1111; */
/*     object_t res; */

/*     object_t obj_val; */
/*     symbol_t s1; */
/*     s1.value = &obj_val; */
/*     strcpy(s1.str, "test_var"); */
/*     obj_val.type = SYMBOL; */
/*     obj_val.u.symbol = &s1; */

/*     object_t params = new_pair(&obj_val, new_pair(object_new(NUMBER, &num), NULL)); */
/*     object_t new_var = setq(params); */

/*     int num2 = 992; */
/*     object_t params2 = new_pair(object_new(SYMBOL, "test_var"), */
/*         new_pair(object_new(NUMBER, &num2), NULL)); */
/*     object_t setq_res = setq(params2); */

/*     symbol_t *sym = check_symbol("test_var"); */

/*     ASSERT(sym->value, setq_res); */
/*     ASSERT(sym->value->u.value, num2); */
/* } */

/**
 * Объединить два списка (1) (2)
 * Проверить список
 */
void test_append()
{
    printf("test_append: "); 
    object_t l1 = new_pair(new_number(1), NULLOBJ); 
    object_t l2 = new_pair(new_number(2), NULLOBJ); 
    append_env(l1, l2); 
    ASSERT(get_value(FIRST(l1)), 1); 
    ASSERT(get_value(SECOND(l1)), 2); 
}

/**
 * Создать список (progn 1 2 3)
 * Проверить результат 3
 */
void test_progn()
{
    printf("test_progn: ");
    object_t obj = new_pair(new_number(1), 
			    new_pair(new_number(2), 
				     new_pair(new_number(3), NULLOBJ)));
    object_t res = progn(obj); 
    ASSERT(TYPE(res), NUMBER);
    ASSERT(get_value(res), 3);
}

/**
 * Попытка вычисления пустого списка выражений
 */
void test_progn_null()
{
    printf("test_progn_null: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = progn(NULLOBJ);
        FAIL;
    } else
        OK;
}

/*
 *создать объект list = NULL и
 отправить его в метод backquote()
 */
void test_backquote_nulllist()
{
    printf("test_backquote_nulllist:\n");
    object_t li = NULLOBJ;
    if (setjmp(jmp_env) == 0) {
        object_t res = backquote(li);
        FAIL;
    } else
        OK;
}

/**
 * Входящий аргумент с вычислением -> выходящий аргумент
 * b = 7 `(a ,b c) -> (a 7 c)
 * (a (COMMA b) c)
 * b = (1 2) `(a ,b c) -> (a (1 2) c)
 * b = (1 2) `(a ,b c) -> (list 'a '(1 2) 'c)
 * b = (1 2) `(a (,b) c) -> (a ((1 2)) c)
 * b = (1 2) `(a ,@b c) -> (a 1 2 c)
 * (A (COMMA-AT B) C)
 *
 * abc = (5 8 6)
 * a = 9
 * Тестовый список - (1 (comma-at abc) (backquote abc) (comma-at nil) nil ((comma a)) "a" #(1 (comma a) 2))
 * (1 5 8 6 (backquote abc) nil (9) "a" #(1 9 2))
 */
void test_backquote_arguments()
{
    printf("test_backquote_arguments:\n");
    int length = 3; 
    int a = 5;
    int b = 8;
    int c = 6;
    int aa = 9;
    int s1 = 1;
    int s2 = 2;
    object_t abc = new_pair(new_number(a), new_pair(new_number(b), new_pair(new_number(c), NULLOBJ))); //Переменная-список abc
    find_symbol("ABC")->value = abc;
    object_t CAabc = new_pair(NEW_SYMBOL("COMMA-AT"), new_pair(NEW_SYMBOL("ABC"), NULLOBJ)); // (COMMA-AT abc)
    object_t CA = new_pair(NEW_SYMBOL("COMMA-AT"), new_pair(NULLOBJ, NULLOBJ)); // (COMMA-AT NIL) 
    object_t BQabc = new_pair(NEW_SYMBOL("BACKQUOTE"), new_pair(NEW_SYMBOL("ABC"), NULLOBJ)); // (BACKQUOTE abc) 
    find_symbol("A")->value = new_number(aa);		       
    object_t obj1 = new_number(s1); 
    object_t obj2 = new_pair(NEW_SYMBOL("COMMA"), new_pair(NEW_SYMBOL("A"), NULLOBJ)); 
    object_t obj3 = new_number(s2); 
    object_t p = new_pair(obj1, new_pair(obj2, new_pair(obj3, NULLOBJ))); 
    array_t *arr = NEW_ARRAY(p);
    object_t Ca = new_pair(obj2, NULLOBJ); // ((COMMA A)) 
    object_t inputlist = new_pair(obj1,  //1 
        new_pair(CAabc, //(COMMA-AT abc) *\\/ *\/ 
            new_pair(BQabc, //(BACKQUOTE abc) *\\/ *\/ 
                new_pair(CA, //(BACKQUOTE abc) *\\/ *\/ 
                    new_pair(NULLOBJ,
                        new_pair(Ca, //((COMMA-AT NIL)) *\\/ *\/ 
                            new_pair(NEW_STRING("a"),
                                new_pair(arr, NULLOBJ)))))))); // Наш массив *\\/ *\/
    PRINT(inputlist);
    object_t resultlist = backquote(new_pair(inputlist, NULLOBJ));
    PRINT(resultlist);
    ASSERT(TYPE(FIRST(TAIL(TAIL(TAIL(TAIL(TAIL(TAIL(TAIL(TAIL(resultlist)))))))))), ARRAY);
}

/**
 * Проверка объекта на то, является ли аргумент неделимым
 * Объект неделимый - результат истинна
 */
void test_atom()
{
    printf("test_atom: ");
    object_t num_obj = new_number(5);
    object_t pair = new_pair(num_obj, NULLOBJ);
    object_t res = atom(pair);
    ASSERT(res, t);
}

/**
 * Попытка проверки на неделимость объекта NULLOBJ
 */
void test_atom_null()
{
    printf("test_atom_null: ");
    if (setjmp(jmp_env) == 0) {
        object_t res = atom(NULLOBJ);
        FAIL;
    } else
        OK;
}

/**
 * Попытка проверки списка на неделимость
 * Объект список - результат nil
 */
void test_atom_list()
{
    printf("test_atom_list: ");
    object_t sym_obj = NEW_STRING("X");
    object_t num_obj = new_number(5);
    object_t list = new_pair(num_obj, new_pair(sym_obj, NULLOBJ));
    object_t temp = new_pair(list, NULLOBJ);
    object_t res = atom(temp);
    ASSERT(res, nil);
}

/**
 * Попытка проверки на неделимость нескольких элементов
 */
void test_atom_many_args()
{
    printf("test_atom_many_args: ");
    object_t num_obj = new_number(5);
    object_t list = new_pair(num_obj, new_pair(num_obj, NULLOBJ));
    if (setjmp(jmp_env) == 0) {
        object_t res = atom(list);
        FAIL;
    } else
        OK;
}

/**
 * Попытка вернуть элемент из списка нескольких параметров
 */
void test_quote_error()
{
    printf("test_quote_error: ");
    object_t num_obj = new_number(5);
     object_t list = new_pair(num_obj, new_pair(num_obj, NULLOBJ));
     if (setjmp(jmp_env) == 0) {
        object_t res = quote(list);
        FAIL;
     } else
        OK;
}

/**
 * Тест цитирования символа
 */
void test_quote()
{
     printf("test_quote: ");
     object_t obj = NEW_SYMBOL("a");
     object_t list = new_pair(obj, NULLOBJ);
     object_t res = quote(list);
     ASSERT(res, obj);
}

/* /\** */
/*  * Создать объекты для выражения  */
/*  * Вызвать функцию eq */
/*  * Проверить результат */
/*  * (eq )            -> error */
/*  * (eq 'a)          -> error */
/*  * (eq 'a 'a 'a)    -> error */
/*  * (eq 'a 'a)       -> t */
/*  * (eq 'a 'b)       -> nil */
/*  *\/ */
void test_eq() 
{ 
    printf("test_eq: "); 
    object_t n1 = new_number(1); 
    object_t n2 = new_number(1);
    object_t s1 = NEW_STRING("String");
    object_t s2 = NEW_STRING("String");
    object_t s3 = NEW_SYMBOL("a");
    object_t s4 = NEW_SYMBOL("a");
    
    object_t list1 = new_pair(n1, new_pair(n2, NULLOBJ)); 
    object_t list2 = new_pair(s1, new_pair(s2, NULLOBJ)); 
    object_t list3 = new_pair(s3, new_pair(s4, NULLOBJ)); 
    
    object_t res1 = eq(list1); 
    object_t res2 = eq(list2); 
    object_t res3 = eq(list3); 
    
    ASSERT(res1, t); 
    ASSERT(res2, nil);
    ASSERT(res3, t); 
}

/**
 * отсутсвие аргумента
 */
void test_and_null()
{
    printf("test_and_null: \n");
    if (setjmp(jmp_env) == 0) {
        object_t res = and(NULLOBJ); 
        FAIL;
    } else
        OK;
}

/**
 * некорректный аргумент
 */
void test_and_invalid()
{
    printf("test_and_invalid: \n");
    object_t p1 = new_number(1); 
    object_t l1 = new_pair(p1, NULLOBJ); 
    if (setjmp(jmp_env) == 0) {
        object_t res = and(l1); 
        FAIL;
    } else
        OK;
}

/**
 * корректный аргумент  true
 */
void test_and()
{
    printf("test_and: \n");
    object_t l1 = new_pair(t, new_pair(t, NULLOBJ));
    object_t res = and(l1);
    ASSERT(res, t);
}

/**
 * корректный аргумент  nil
 */
void test_and_nil()
{
    printf("test_and_nil: \n");
    object_t l1 = new_pair(t, new_pair(nil, NULLOBJ));
    object_t res = and(l1);
    ASSERT(res, nil);
}

/**
 * Тестирование функции or
 * передаём NULLOBJ вместо списка
 */
void test_or_null()
{
    printf("test_or_null: \n");
    if (setjmp(jmp_env) == 0) {
        object_t res = or(NULLOBJ);
        FAIL;
    } else
        OK;
}

 /**
 * Тестирование функции or
 * передаём список с некорректными данными
 */
void test_or_invalid()
{
    printf("test_or_invalid: \n");
    object_t p1 = new_number(1);
    object_t l1 = new_pair(p1, NULLOBJ);
    if (setjmp(jmp_env) == 0) {
        object_t res = or(l1);
        FAIL;
    } else
        OK;
}

 /**
 * Тестирование функции or
 * Первый элемент - t
 */
void test_or_first()
{
    printf("test_or_first: \n");
    object_t l1 = new_pair(t, new_pair(t, NULLOBJ));
    object_t res = or(l1);
    ASSERT(res, t);
}

/**
 * Тестирование функции or
 * Первый элемент - nil, второй - t
 */
void test_or_tail()
{
    printf("test_or_tail: \n");
    object_t l1 = new_pair(nil, new_pair(t, NULLOBJ));
    object_t res = or(l1);
    ASSERT(res, t);
}


/**
 * Тестирование функции or
 * Первый элемент - nil, второй - nil
 */
void test_or_nil()
{
    printf("test_or_nil: \n");
    object_t l1 = new_pair(nil, new_pair(nil, NULLOBJ));
    object_t res = or(l1);
    ASSERT(res, nil);
}

/**
 * Тест на неправильный символ LAMBDA
 */
void test_is_lambda_invalid_symbol()
{
    printf("test_is_lambda_invalid_symbol (lambda->type != SYMBOL) \n"); 
    int num = 5; 
    object_t p1 = NEW_SYMBOL("a"); 
    object_t p2 = new_number(num); 
    object_t params = new_pair(p1, new_pair(p2, NULLOBJ)); 
    object_t q = new_pair(NEW_SYMBOL("ATOM"), new_pair(NEW_SYMBOL("CAR"), new_pair(params, NULLOBJ))); 
    object_t list = new_pair(new_number(num), new_pair(params, new_pair(q, NULLOBJ))); 

    if (setjmp(jmp_env) == 0) {
        int i = is_lambda(list); 
        FAIL;
    } else
        OK;
}

/**
 * LAMBDA без параметров
 */
void test_is_lambda_no_params()
{
    printf("test_is_lambda_no_params \n"); 
    
    object_t list = new_pair(NEW_SYMBOL("LAMBDA"), NULLOBJ); 
    if (setjmp(jmp_env) == 0) {
        int i = is_lambda(list); 
        FAIL;
    } else
        OK;
}

/**
 * Неправильный список аргументов в функции
 */
void test_is_lambda_invalid_params()
{
    printf("test_is_lambda_invalid_params \n"); 
    object_t p1 = NEW_SYMBOL("a"); 
    object_t q = new_pair(NEW_SYMBOL("ATOM"), new_pair(NEW_SYMBOL("CAR"), new_pair(p1, NULLOBJ))); 
    object_t list = new_pair(NEW_SYMBOL("LAMBDA"), new_pair(p1, new_pair(q, NULLOBJ))); 
    if (setjmp(jmp_env) == 0) {
        int i = is_lambda(list); 
        FAIL;
    } else
        OK;
}

/**
 * Не символ в параметрах
 */
void test_is_lambda_not_symbol()
{
    printf("test_is_lambda_not_symbol: \n"); 
    int num = 5; 
    object_t p1 = NEW_SYMBOL("a"); 
    object_t p2 = new_number(num); 
    object_t params = new_pair(p1, new_pair(p2, NULLOBJ)); 
    object_t q = new_pair(NEW_SYMBOL("ATOM"), new_pair(NEW_SYMBOL("CAR"), new_pair(params, NULLOBJ))); 
    object_t list = new_pair(NEW_SYMBOL("LAMBDA"), new_pair(params, new_pair(q, NULLOBJ))); 
    if (setjmp(jmp_env) == 0) {
        int i = is_lambda(list); 
        FAIL;
    } else
        OK;
}

/**
 * Нет тела в функции
 */
void test_is_lambda_no_body()
{
    printf("test_is_lambda_no_body \n"); 
    object_t p1 = NEW_SYMBOL("a"); 
    object_t p2 = NEW_SYMBOL("b"); 
    object_t params = new_pair(p1, new_pair(p2, NULLOBJ)); 
    object_t list = new_pair(NEW_SYMBOL("LAMBDA"), new_pair(params, NULLOBJ)); 
    if (setjmp(jmp_env) == 0) {
        int i = is_lambda(list); 
        FAIL;
    } else
        OK;
}

/**
 * Вызов (macrocall (lambda (x) (list x)))
 */
void test_macro_call()
{
    printf("test_macro_call: \n");
    object_t p1 = NEW_SYMBOL("x"); // x 
    object_t q = new_pair(NEW_SYMBOL("LIST"), //(list x) 
			  new_pair(p1, NULLOBJ)); 
    object_t lx = new_pair(NEW_SYMBOL("LAMBDA"), new_pair(new_pair(p1, NULLOBJ), new_pair(new_pair(NEW_SYMBOL("LIST"), NULLOBJ), new_pair(p1, NULLOBJ)))); 
    // (lambda (x) (list x)); 
    object_t args = new_pair(new_number(10), NULLOBJ); 
    object_t res = macro_call(lx, args, NULLOBJ); 
    ASSERT(get_value(res), 10);
}

/**
 * Тест вызова функции ((lambda (x) x) 10)
 */
void test_eval_func()
{
    printf("test_eval_func: ");
    object_t x1 = NEW_SYMBOL("x"); // x 
    object_t param1 = new_pair(x1, NULLOBJ); // (x) 
    object_t list = new_pair(NEW_SYMBOL("LAMBDA"), new_pair(param1, param1)); // (lambda (x) x) 
    object_t arg_x = new_number(10); // 10
    object_t args = new_pair(arg_x, NULLOBJ); //(
    object_t res = eval_func(list, args, NULLOBJ); 
    ASSERT(TYPE(res), NUMBER); 
    ASSERT(get_value(res), 10); 
}

/**
 * Тест вызова функции ((lambda (x) 10 x) 10)
 */
void test_eval_func2()
{
    printf("test_eval_func2: ");
    object_t arg_x = new_number(10);// 10 
    object_t args = new_pair(arg_x, NULLOBJ); //(10) 
    object_t x1 = NEW_SYMBOL("x"); // x 
    object_t param1 = new_pair(x1, NULLOBJ); // (x) 
    object_t param2 = new_pair(arg_x, param1); 
    object_t list = new_pair(NEW_SYMBOL("LAMBDA"), new_pair(param1, param2));  
    object_t res = eval_func(list, args, NULLOBJ); 
    ASSERT(TYPE(res), NUMBER); 
    ASSERT(get_value(res), 10);
}

/** 
 * Тест создания окружения с числом аргументов большим чем параметров
 */
/* void test_er_num_arg_make_env() */
/* { */
/*     printf("test_er_num_arg_make_env: \n"); */
/*     int num = 1; */
/*     object_t *args = NULLOBJ; */
/*     object_t *val = object_new(NUMBER, &num); */
/*     object_t *res = make_env(args, val); */
/*     ASSERT(res, ERROR); */
/* } */

/** 
 * Определение нового макроса
 */
void test_defmacro()
{
    printf("test_defmacro: ");
    object_t arg1 = NEW_SYMBOL("test"); 
    object_t arg2 = new_pair(nil, new_pair(nil, NULLOBJ)); 
    object_t arg3 = new_number(1);
    object_t args = new_pair(arg1, new_pair(arg2, new_pair(arg3, NULLOBJ))); 
    object_t result = defmacro(args);
    PRINT(result);
    ASSERT(TYPE(result), SYMBOL); 
    ASSERT(strcmp(GET_SYMBOL(result)->str, "test"), 0); 
}

/**
 * Проверка значения созданной переменной
 */
void test_eval_symbol_with_defined_variable()
{
    printf("test_eval_symbol_with_defined_variable:");
    find_symbol("A")->value = new_number(10);
    object_t myVar = NEW_SYMBOL("A");

    object_t result = eval(myVar, NULLOBJ);
    
    ASSERT(TYPE(result), NUMBER); 
    ASSERT(get_value(result), 10); 
}

/**
 * Проверка значения созданной переменной в созданном окружении
 */
void test_eval_symbol_environment_variable()
{
    printf("test_eval_symbol_environment_variable: ");
    current_env = create_env();
    if (setjmp(jmp_env) == 0) {
        object_t result = eval(NEW_SYMBOL("X"), current_env);
        ASSERT(get_value(result), 1);
    } else
        FAIL;
}

/**
 * Проверка вычисления несуществующей переменной
 */
void test_eval_symbol_undefined_variable()
{
    printf("test_eval_symbol_undefined_variable: ");
    object_t var = NEW_SYMBOL("undefinedVar");
    if (setjmp(jmp_env) == 0) {
        eval(var, NULLOBJ);
        FAIL;
    } else
        OK;
}

/**
 * Проверка функции eval_args (1 2)
 */
void test_eval_args()
{
    printf("test_eval_args: ");
    object_t num1 = new_number(1);
    object_t num2 = new_number(2);
    object_t list = new_pair(num1, new_pair(num2, NULLOBJ));
    object_t res = eval_args(list, NULLOBJ);
    ASSERT(TYPE(res), PAIR); 
    ASSERT(get_value(FIRST(res)), 1); 
    ASSERT(get_value(SECOND(res)), 2); 
}

/**
 * Проверка функции eval_args с пустым списком
 */
void test_eval_args_null()
{
    printf("test_eval_args_null: ");
    object_t res = eval_args(NULLOBJ, NULLOBJ);
    ASSERT(res, NULLOBJ); 
}
/*
eval_int
+---------------------------+------------------------------------------------+------------------------------------------------------+
| Условие                   | Правильный                                     | Неправильный                                         |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| atom                      |1)число                                         |52)Нет аргументов                                     |
|                           |2)символ - заданная переменная                  |131)Больше одного аргумента                           |
|                           |3)строка                                        |                                                      |
|                           |4)массив                                        |                                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция quote             |5)1 аргумент                                    |53)!= 1 аргументов                                    |
|                           |6)любой аргумент                                |                                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция atom              |7)аргумент список                               |54)отсутсвие аргументов                               |
|                           |8)аргумент не список                            |                                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция eq                |9)2 аргумента                                   |55)отсутсвие аргументов                               |
|                           |                                                |56)> 2 аргументов                                     |
|                           |                                                |57)1 аргумент                                         |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция car               |10)аргумент список                              |58)не список                                          |
|                           |                                                |59)отсутсвие аргументов                               |
|                           |                                                |60)> 1 аргумента                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция cdr               |11)аргумент список                              |61)не список                                          |
|                           |                                                |62)отсутсвие аргументов                               |
|                           |                                                |63)> 1 аргумента                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция cons              |12)2 аргумента                                  |64)отсутсвие аргументов                               |
|                           |                                                |65)> 2 аргументов                                     |
|                           |                                                |66)1 аргумент                                         |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция cond              |13)хотя бы  1 аргумент приводит к истине        |67)отсутсвие аргументов                               |
|                           |                                                |68)отсутсвие аргумента, приводящего к истине          |
|                           |                                                |69)отсутствие выражения после предикат                |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция lambda            |14)одно выражение в теле                        |70)отсутствие параметров                              |
|                           |15)больше одного выражения в теле               |71)отсутствие аргументов                              |
|                           |125)одинаковое кол-во параметров и аргументов   |72)несоответствие количества параметров и аргументов  |
|                           |                                                |73)отсутствие выражения                               |
|                           |                                                |124)параметр не символ                                |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| определение функции defun |18)хотя бы одно выражение                       |16)отсутствие параметров                              |
|                           |                                                |17)0 аргументов                                       |
|                           |                                                |74)нет выражения в теле                               |
|                           |                                                |126)только имя функции                                |
|                           |                                                |127)имя - не символ                                   |
|                           |                                                |128)список параметров - не список                     |
|                           |                                                |129)2-й параметр неоднородный список                  |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция rplaca            |19)первый аргумент пара                         |75)отсутствие аргументов                              |
|                           |20)два аргумента                                |76)> 2 аргументов                                     |
|                           |                                                |77)1 аргумент                                         |
|                           |                                                |78)первый аргумент не пара                            |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция rplacd            |21)первый аргумент пара                         |79)отсутствие аргументов                              |
|                           |22)два аргумента                                |80)> 2 аргументов                                     |
|                           |                                                |81)1 аргумент                                         |
|                           |                                                |82)первый аргумент не пара                            |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция defvar            |23)имя переменной                               |83)отсутствие аргументов                              |
|                           |24)первый аргумент символ                       |84)> 2 аргументов                                     |
|                           |                                                |85)первый аргумент не символ                          |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция setq              |25)не менее двух аргументов                     |86)отсутствие аргументов                              |
|                           |26)чётное количество аргументов                 |87)нечётное количество аргументов                     |
|                           |27)нечётные аргументы символы                   |88)нечётные аргументы не символы                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция concat            |28)аргумент строка                              |89)отсутствие аргументов                              |
|                           |29)больше одного аргумента                      |90)аргумент не строка                                 |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция intern            |30)аргумент строка                              |91)отсутствие аргументов                              |
|                           |                                                |92)аргумент не строка                                 |
|                           |                                                |93)больше одного аргумента                            |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция symbol-name       |31)аргумент символ                              |94)отсутствие аргументов                              |
|                           |                                                |95)аргумент не символ                                 |
|                           |                                                |96)больше одного аргумента                            |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция make-array        |32)аргумент имя и размер                        |97)отсутствие аргументов                              |
|                           |33)аргумент размер                              |98)> 2 аргументов                                     |
|                           |34)аргумент имя                                 |                                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция seta              |35)три аргумента                                |99)отсутствие аргументов                              |
|                           |36)второй аргумент число                        |100)>3 аргументов                                     |
|                           |37)первый аргумент объект массив                |101)<3 аргументов                                     |
|                           |                                                |102)второй аргумент не число                          |
|                           |                                                |103)первый аргумент не объект массив                  |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция aref              |38)два аргумента                                |104)отсутствие аргументов                             |
|                           |39)первый аргумент объект массив                |105)>2 аргументов                                     |
|                           |40)второй аргумент число                        |106)<2 аргументов                                     |
|                           |                                                |107)второй аргумент не число                          |
|                           |                                                |108)первый аргумент не объект массив                  |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| функция defmacro          |41)один параметр, аргумент и тело               |109)отсутствие аргументов                             |
|                           |                                                |110)только имя макроса                                |
|                           |                                                |111)<2 аргументов                                     |
|                           |                                                |112)параметр имя не символ                            |
|                           |                                                |113)2-й параметр неоднородный список                  |
|                           |                                                |114)второй параметр не список                         |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| ` символ                  |42)параметр символ                              |115)отсутствие аргументов                             |
|                           |43)параметр список                              |                                                      |
|                           |117)параметр массив                             |                                                      |
|                           |118)параметр строка                             |                                                      |
|                           |116)параметр число                              |                                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| , символ                  |44)параметр список                              |119)выражение вне символа `                           |
|                           |45)параметр символ                              |120)отсутствие аргументов                             |
|                           |46)параметр число                               |                                                      |
|                           |47)параметр массив                              |                                                      |
|                           |132)параметр строка                             |                                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
| ,@ символ                 |48)параметр список                              |121)выражение вне символа `                           |
|                           |49)параметр символ                              |122)отсутствие аргументов                             |
|                           |50)параметр число                               |123)результат вычисления выражения не список          |
|                           |51)параметр массив                              |                                                      |
|                           |133)параметр строка                             |                                                      |
+---------------------------+------------------------------------------------+------------------------------------------------------+
*/
int main()
{
    printf("------------test_eval_int---------\n");
    init_pair();
    init_eval();
    init_regions();
    init_objects();
    test_is_lambda();//14
    test_cond();//13
    test_cond_null();//67
    test_cond_tail_null();//69
    test_cond_many_params();
    test_make_env();
    test_find_in_env();
    test_defun();//18
    test_setq_set_env();
    /* test_setq_global_set(); */
    test_append();
    test_progn();
    test_progn_null();
    test_backquote_nulllist();
    test_backquote_arguments();
    test_atom();//1
    test_atom_null();//52
    test_atom_list();
    test_atom_many_args();//131
    test_quote();
    test_quote_error();//53
    test_eq(); //9, 55, 56, 57
    test_and_null();
    test_and_invalid();
    test_and();
    test_and_nil();
    test_or_null();
    test_or_invalid();
    test_or_first();
    test_or_tail();
    test_or_nil();
    test_is_lambda_invalid_symbol();
    test_is_lambda_no_params();
    test_is_lambda_invalid_params();
    test_is_lambda_not_symbol();
    test_is_lambda_no_body();
    test_macro_call();
    test_eval_func();
    test_eval_func2();
    /* test_er_num_arg_make_env(); */
    test_defmacro();
    test_eval_symbol_with_defined_variable();
    test_eval_symbol_environment_variable();
    test_eval_symbol_undefined_variable();
    test_eval_args();
    test_eval_args_null();
    return 0;
}
