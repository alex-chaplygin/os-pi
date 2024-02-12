#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "test.h"
#include "parser.h"
#include "pair.h"
#include "string.h"

extern object_t *t;
extern object_t *nil;
extern object_t *current_env;

object_t * make_env(object_t *args, object_t *values);
int find_in_env(object_t *env, object_t *sym, object_t **res);
int is_lambda(object_t *list);
void append_env(object_t *l1, object_t *l2);
object_t *setq(object_t *params);
object_t *atom(object_t *params);
object_t *defvar(object_t *params);
object_t *cond(object_t *list);
object_t *progn(object_t *list);
object_t *quote(object_t *list);
object_t *eq(object_t *list);
object_t *and(object_t *list);
object_t *or(object_t *list);
object_t *backquote(object_t *list);
object_t *defmacro(object_t *list);
object_t *macro_call(object_t *macro, object_t *args, object_t *env);
object_t *eval_func(object_t *lambda, object_t *args, object_t *env);

void error(char *str, ...)
{
  printf("%s", str);
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
    int n1 = 1;
    int n2 = 2;
    object_t *p1 = new_pair(nil, new_pair(object_new(NUMBER, &n1), NULL));
    object_t *p2 = new_pair(t, new_pair(object_new(NUMBER, &n2), NULL));
    object_t *l = new_pair(object_new(SYMBOL, "COND"), new_pair(p1, new_pair(p2, NULL)));
    object_t *res = eval(l, NULL);
    ASSERT(res->type, NUMBER);
    ASSERT(res->u.value, 2);
}

/**
 * Попытка обработать null функцией cond
*/
void test_cond_null()
{
    printf("test_cond_null: ");
    object_t *res = cond(NULL);
    ASSERT(res, ERROR);
}

/**
 * Попытка обработать объект с одним элементом функцией cond
*/
void test_cond_tail_null()
{
    printf("test_cond_tail_null: ");
    int num1 = 1;
    object_t *single_param_list = new_pair(object_new(NUMBER, &num1), NULL);
    object_t *cond_expr = new_pair(single_param_list, NULL);

    object_t *result = cond(cond_expr);
    ASSERT(result, ERROR);
}

/**
 * Создать выражение (cond ((eq 'a 'a) (eq 'a 'a) 'first))
 * Вычислить его и проверить результат на ошибку
*/
void test_cond_many_params()
{
    printf("test_cond_many_params: \n");
    object_t *a = new_pair(object_new(SYMBOL, "QUOTE"), new_pair(
        object_new(SYMBOL, "A"), NULL));
    object_t *eq_pair = new_pair(
        object_new(SYMBOL, "EQ"), new_pair(
            a, new_pair(
                a, NULL)));
    object_t *first = new_pair(object_new(SYMBOL, "QUOTE"), new_pair(
        object_new(SYMBOL, "FIRST"), NULL));
    object_t *args = new_pair(eq_pair, new_pair(
        eq_pair, new_pair(
            first, NULL)));
    object_t *cond_pair = new_pair(object_new(SYMBOL, "COND"), new_pair(args, NULL));
    object_t *res = eval(cond_pair, NULL);
    ASSERT(res, ERROR);
}

/**
 * Создать объект для выражения (lambda (a, b) (atom (car (a, b))))
 * Вызвать функцию is_lambda
 * Проверить результат = 1
 */
void test_is_lambda()
{
    printf("test_is_lambda: ");
    object_t *p1 = object_new(SYMBOL, "a");
    object_t *p2 = object_new(SYMBOL, "b");
    object_t *params = new_pair(p1, new_pair(p2, NULL));

    object_t *q = new_pair(object_new(SYMBOL, "ATOM"),
        new_pair(object_new(SYMBOL, "CAR"), new_pair(params, NULL)));
    
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(params, new_pair(q, NULL)));

    int i = is_lambda(list);
    ASSERT(i, 1);
}

/**
 * Создать объект для некорректного выражения (lambda (a, 5) (atom (car (a, 5))))
 * Вызвать функцию is_lambda
 * Проверить результат = 0
 */
object_t *create_env()
{
  int num1 = 1;
  int num2 = 2;
  object_t *arg_y = object_new(SYMBOL, "Y");
  object_t *arg_x = object_new(SYMBOL, "X");
  object_t *arg_1 = object_new(NUMBER, &num1);
  object_t *arg_2 = object_new(NUMBER, &num2);
  object_t *args = new_pair(arg_x, new_pair(arg_y, NULL));
  object_t *values = new_pair(arg_1, new_pair(arg_2, NULL));
  return make_env(args,values);
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
  object_t *env = create_env();
  object_t *p1 = FIRST(env);
  object_t *p2 = SECOND(env);  
  ASSERT(p1->type, PAIR);
  ASSERT(p2->type, PAIR);
  ASSERT(FIRST(p1)->u.symbol, find_symbol("X"));
  ASSERT(TAIL(p1)->u.value, 1);
  ASSERT(FIRST(p2)->u.symbol, find_symbol("Y"));
  ASSERT(TAIL(p2)->u.value, 2);      
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
  object_t *env = create_env();
  object_t *res;
  PRINT(env);
  int result = find_in_env(env, object_new(SYMBOL, "Y"), &res);
  ASSERT(result, 1);
  ASSERT(res->type, NUMBER);
  ASSERT(res->u.value, 2);
}

/**
 * Создать функцию (defun null (x) (eq x '()))
 * Проверить значение символа null
 */
void test_defun()
{
    printf("test_defun: ");
    object_t *body = new_pair(object_new(SYMBOL, "EQ"),
			      new_pair(object_new(SYMBOL, "X"),
				       new_pair(NULL, NULL)));
    object_t *args = new_pair(object_new(SYMBOL, "X"), NULL);
    object_t *func = new_pair(object_new(SYMBOL, "DEFUN"),
			      new_pair(object_new(SYMBOL, "NULL"),
				       new_pair(args,
						new_pair(body, NULL))));
    object_t *res = eval(func, NULL);
    ASSERT(res->type, SYMBOL);
    symbol_t *null = find_symbol("NULL");
    ASSERT(res->u.symbol, null);
    ASSERT(null->lambda->type, PAIR);
    ASSERT(null->lambda->u.pair->left->u.symbol, find_symbol("LAMBDA"));
}

/**
 * Создать окружение с числовыми переменными X, Y
 * Вызвать функцию setq для изменения значения Y на 1010
 * Проверить что значение Y = 1010
 */
void test_setq_set_env()
{
    printf("test_setq_set_env: ");
    object_t *env = create_env();
    object_t *p2 = SECOND(env);
    int num = 1010;
    object_t *num_obj = object_new(NUMBER, &num);
    object_t *params = new_pair(FIRST(p2), new_pair(num_obj, NULL));
    current_env = env;
    object_t *setq_res = setq(params);
    object_t *obj_in_env;
    find_in_env(current_env, FIRST(p2), &obj_in_env);
    ASSERT(setq_res->u.value, num);
    ASSERT(setq_res, num_obj);
    ASSERT(setq_res->u.value, obj_in_env->u.value);
}

/**
 * Создать глобальную числовую переменную test_var
 * Вызвать функцию setq для изменения значения test_var на 992
 * Проверить что значение test_var = 992
 */
void test_setq_global_set()
{
    printf("test_setq_global_set: ");
    object_t *env = create_env();
    object_t *p1 = FIRST(env);
    object_t *p2 = SECOND(env);  
    int num = 1111;
    object_t *res;

    object_t obj_val;
    symbol_t s1;
    s1.value = &obj_val;
    strcpy(s1.str, "test_var");
    obj_val.type = SYMBOL;
    obj_val.u.symbol = &s1;

    object_t *params = new_pair(&obj_val, new_pair(object_new(NUMBER, &num), NULL));
    object_t *new_var = setq(params);

    int num2 = 992;
    object_t *params2 = new_pair(object_new(SYMBOL, "test_var"),
        new_pair(object_new(NUMBER, &num2), NULL));
    object_t *setq_res = setq(params2);

    symbol_t *sym = check_symbol("test_var");

    ASSERT(sym->value, setq_res);
    ASSERT(sym->value->u.value, num2);
}

/**
 * Объединить два списка (1) (2)
 * Проверить список
 */
void test_append()
{
    int num1 = 1;
    int num2 = 2;
    printf("test_append: ");
    object_t *l1 = new_pair(object_new(NUMBER, &num1), NULL);
    object_t *l2 = new_pair(object_new(NUMBER, &num2), NULL);
    append_env(l1, l2);
    ASSERT(l1->u.pair->left->u.value, 1);
    ASSERT(l1->u.pair->right->u.pair->left->u.value, 2);
}

/**
 * Создать список (progn 1 2 3)
 * Проверить результат 3
 */
void test_progn()
{
    printf("test_progn: ");
    int num1 = 1;
    int num2 = 2;
    int num3 = 3;
    object_t *obj = new_pair(object_new(SYMBOL, "PROGN"),
			     new_pair(object_new(NUMBER, &num1),
				      new_pair(object_new(NUMBER, &num2),
					       new_pair(object_new(NUMBER, &num3), NULL))));
    object_t *res = eval(obj, NULL);
    ASSERT(res->type, NUMBER);
    ASSERT(res->u.value, 3);
}

/**
 * Попытка вычисления пустого списка выражений
 */
void test_progn_null()
{
    printf("test_progn_null: ");
    object_t *res = progn(NULL);
    ASSERT(res, ERROR);
}

/**
 * Попытка вычисления выражения ошибки 
 */
void test_progn_error()
{
    printf("test_progn_error: ");
    int number = 5;
    object_t *num_obj = object_new(NUMBER, &number);
    object_t *list = new_pair(num_obj, ERROR);
    object_t *res = progn(list);
    ASSERT(res, ERROR);
}

/*
 *создать объект list = NULL и 
 отправить его в метод backquote()
 */
void test_backquote_nulllist()
{
    printf("test_backquote_nulllist:\n");
    object_t *li = NULL;
    object_t *res = backquote(li);
    ASSERT(res, ERROR);
}

/*
 *Тест backquote с неверным типом аргумента
 */
void test_backquote_invalid_arg_type()
{
    int num = 5;
    printf("test_backquote_invalid_arg_type:\n");
    object_t *res = backquote(new_pair(new_pair(object_new(NUMBER, &num), new_pair((object_t *)&num, NULL)), NULL));
    ASSERT(res, ERROR);
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
    object_t *abc = new_pair(object_new(NUMBER, &a),
	                    new_pair(object_new(NUMBER, &b),
		                     new_pair(object_new(NUMBER, &c),NULL))); //Переменная-список abc
    find_symbol("ABC")->value = abc;
    object_t *CAabc= new_pair(object_new(SYMBOL, "COMMA-AT"),
			      new_pair(object_new(SYMBOL,"ABC"),NULL)); // (COMMA-AT abc)
    object_t *CA= new_pair(object_new(SYMBOL, "COMMA-AT"),
			      new_pair(NULL, NULL)); // (COMMA-AT NIL)
    object_t *BQabc= new_pair(object_new(SYMBOL, "BACKQUOTE"),
			      new_pair(object_new(SYMBOL,"ABC"),NULL)); // (BACKQUOTE abc)
    find_symbol("A")->value = object_new(NUMBER,&aa);		      
    object_t *obj1 = object_new(NUMBER, &s1);
    object_t *obj2 = new_pair(object_new(SYMBOL, "COMMA"),
		            new_pair(object_new(SYMBOL, "A"),NULL));
    object_t *obj3 = object_new(NUMBER, &s2);
    array_t *arr = new_empty_array(length);
    arr->data[0] = obj1;
    arr->data[1] = obj2;
    arr->data[2] = obj3; //здесь создаём массив с элементами 1 (COMMA A) 2
    object_t *Ca = new_pair(obj2, NULL); // ((COMMA A))
    object_t *inputlist = new_pair(object_new(NUMBER, &s1),  //1
        new_pair(CAabc, //(COMMA-AT abc) *\/ */
            new_pair(BQabc, //(BACKQUOTE abc) *\/ */
                new_pair(CA, //(BACKQUOTE abc) *\/ */
                    new_pair(NULL,
                        new_pair(Ca, //((COMMA-AT NIL)) *\/ */
                            new_pair(object_new(STRING, "a"),
                                new_pair(object_new(ARRAY, arr), NULL)))))))); // Наш массив *\/ */
    object_t *resultlist = backquote(new_pair(inputlist, NULL));
    PRINT(inputlist);
    PRINT(resultlist);
    ASSERT(FIRST(TAIL(TAIL(TAIL(TAIL(TAIL(TAIL(TAIL(TAIL(resultlist)))))))))->type, ARRAY);
}

/**
 * Проверка объекта на то, является ли аргумент неделимым
 * Объект неделимый - результат истинна
 */
void test_atom()
{
    printf("test_atom: ");
    int number = 5;
    object_t *num_obj = object_new(NUMBER, &number);
    object_t *pair = new_pair(num_obj, NULL);
    object_t *res = atom(pair);
    ASSERT(res, t);
}

/**
 * Попытка проверки на неделимость объекта NULL 
 */
void test_atom_null()
{
    printf("test_atom_null: ");
    object_t *res = atom(NULL);
    ASSERT(res, ERROR);
}

/**
 * Попытка проверки списка на неделимость 
 * Объект список - результат nil
 */
void test_atom_list()
{
    printf("test_atom_list: ");
    int number = 5;
    char *symbol = "X";
    object_t *sym_obj = object_new(SYMBOL, symbol);
    object_t *num_obj = object_new(NUMBER, &number);
    object_t *list = new_pair(num_obj, new_pair(sym_obj, NULL));
    object_t *temp = new_pair(list, NULL);
    object_t *res = atom(temp);
    ASSERT(res, nil);
}

/**
 * Попытка проверки на неделимость нескольких элементов 
 */
void test_atom_many_args()
{
    printf("test_atom_many_args: ");
    int number = 5;
    object_t *num_obj = object_new(NUMBER, &number);
    object_t *list = new_pair(num_obj, new_pair(num_obj, NULL));
    object_t *res = atom(list);
    ASSERT(res, ERROR);
}

/**
 * Попытка вернуть элемент из списка нескольких параметров  
 */
void test_quote_error()
{
    printf("test_quote_error: ");
    int number = 5;
    object_t *num_obj = object_new(NUMBER, &number);
    object_t *list = new_pair(num_obj, new_pair(num_obj, NULL));
    object_t *res = quote(list);
    ASSERT(res, ERROR);
}

/**
 * Создать объекты для выражения 
 * Вызвать функцию eq
 * Проверить результат
 * (eq )            -> error
 * (eq 'a)          -> error
 * (eq 'a 'a 'a)    -> error
 * (eq 'a 'a)       -> t
 * (eq 'a 'b)       -> nil
 */
void test_eq()
{
    printf("test_eq: ");
    object_t *p1 = object_new(SYMBOL, "a");
    object_t *p2 = object_new(SYMBOL, "b");

    object_t *listnull = NULL;
    object_t *list1 = new_pair(p1, NULL);
    object_t *list2 = new_pair(p1, new_pair(p1, new_pair(p1, NULL)));
    object_t *list3 = new_pair(p1, new_pair(p1, NULL));
    object_t *list4 = new_pair(p1, new_pair(p2, NULL));

    object_t *resnull = eq(listnull);
    object_t *res1 = eq(list1);
    object_t *res2 = eq(list2);
    object_t *res3 = eq(list3);
    object_t *res4 = eq(list4);
    
    ASSERT(resnull, ERROR);
    ASSERT(res1, ERROR);
    ASSERT(res2, ERROR);
    ASSERT(res3, t);
    ASSERT(res4, nil);
}

/**
 * отсутсвие аргумента  
 */
void test_and_null()
{
    printf("test_and_null: \n");
    object_t *res = and(NULL);
    ASSERT(res, ERROR);
}

/**
 * некорректный аргумент  
 */
void test_and_invalid()
{
    printf("test_and_invalid: \n");
    int number = 1;
    object_t *p1 = object_new(NUMBER, &number);
    object_t *l1 = new_pair(p1, NULL);
    object_t *res = and(l1);
    ASSERT(res, ERROR);
}

/**
 * корректный аргумент  true
 */
void test_and()
{
    printf("test_and: \n");
    object_t *l1 = new_pair(t, new_pair(t, NULL));
    object_t *res = and(l1);
    ASSERT(res, t);
}

/**
 * корректный аргумент  nil
 */
void test_and_nil()
{
    printf("test_and_nil: \n");
    object_t *l1 = new_pair(t, new_pair(nil, NULL));
    object_t *res = and(l1);
    ASSERT(res, nil);
}

/**
 * Тестирование функции or
 * передаём NULL вместо списка
 */
void test_or_null()
{
    printf("test_or_null: \n");
    object_t *res = or(NULL);
    ASSERT(res, ERROR);
}

 /**
 * Тестирование функции or
 * передаём список с некорректными данными
 */
void test_or_invalid()
{
    printf("test_or_invalid: \n");
    int number = 1;
    object_t *p1 = object_new(NUMBER, &number);
    object_t *l1 = new_pair(p1, NULL);
    object_t *res = or(l1);
    ASSERT(res, ERROR);
}

 /**
 * Тестирование функции or
 * Первый элемент - t
 */
void test_or_first()
{
    printf("test_or_first: \n");
    object_t *l1 = new_pair(t, new_pair(t, NULL));
    object_t *res = or(l1);
    ASSERT(res, t);
}

/**
 * Тестирование функции or
 * Первый элемент - nil, второй - t
 */
void test_or_tail()
{
    printf("test_or_tail: \n");
    object_t *l1 = new_pair(nil, new_pair(t, NULL));
    object_t *res = or(l1);
    ASSERT(res, t);
}


/**
 * Тестирование функции or
 * Первый элемент - nil, второй - nil
 */ 
void test_or_nil()
{
    printf("test_or_nil: \n");
    object_t *l1 = new_pair(nil, new_pair(nil, NULL));
    object_t *res = or(l1);
    ASSERT(res, nil);
}

/** 
 * Тест на неправильный символ LAMBDA
 */
void test_is_lambda_invalid_symbol()
{
    printf("test_is_lambda_invalid_symbol (lambda->type != SYMBOL) \n");
    int num = 5;
    object_t *p1 = object_new(SYMBOL, "a");
    object_t *p2 = object_new(NUMBER, &num);
    object_t *params = new_pair(p1, new_pair(p2, NULL));

    object_t *q = new_pair(object_new(SYMBOL, "ATOM"),
        new_pair(object_new(SYMBOL, "CAR"), new_pair(params, NULL)));
    
    object_t *list = new_pair(object_new(NUMBER, &num), new_pair(params, new_pair(q, NULL)));

    int i = is_lambda(list);
    ASSERT(i, 0);
}

/** 
 * LAMBDA без параметров
 */
void test_is_lambda_no_params()
{
    printf("test_is_lambda_no_params \n");
    
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), NULL);

    int i = is_lambda(list);
    ASSERT(i, 0);
}

/** 
 * Неправильный список аргументов в функции
 */
void test_is_lambda_invalid_params()
{
    printf("test_is_lambda_invalid_params \n");

    object_t *p1 = object_new(SYMBOL, "a");

    object_t *q = new_pair(object_new(SYMBOL, "ATOM"),
        new_pair(object_new(SYMBOL, "CAR"), new_pair(p1, NULL)));
    
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(p1, new_pair(q, NULL)));

    int i = is_lambda(list);
    ASSERT(i, 0);
}

/** 
 * Не символ в параметрах
 */
void test_is_lambda_not_symbol()
{
    printf("test_is_lambda_not_symbol: \n");
    
    int num = 5;
    object_t *p1 = object_new(SYMBOL, "a");
    object_t *p2 = object_new(NUMBER, &num);
    object_t *params = new_pair(p1, new_pair(p2, NULL));

    object_t *q = new_pair(object_new(SYMBOL, "ATOM"),
        new_pair(object_new(SYMBOL, "CAR"), new_pair(params, NULL)));
    
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(params, new_pair(q, NULL)));

    int i = is_lambda(list);
    ASSERT(i, 0);
}

/** 
 * Нет тела в функции
 */
void test_is_lambda_no_body()
{
    printf("test_is_lambda_no_body \n");

    object_t *p1 = object_new(SYMBOL, "a");
    object_t *p2 = object_new(SYMBOL, "b");
    object_t *params = new_pair(p1, new_pair(p2, NULL));
    
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(params, NULL));

    int i = is_lambda(list);
    ASSERT(i, 0);
}

/** 
 * Вызов (macrocall (lambda (x) (list x)))
 */
void test_macro_call()
{
    printf("test_macro_call: \n");
    int num = 10;
    object_t *env = NULL;
    object_t *p1 = object_new(SYMBOL, "x"); // x
    object_t *q = new_pair(object_new(SYMBOL, "LIST"), //(list x)
    new_pair(p1, NULL));
        object_t *lx = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(new_pair(p1, NULL), new_pair(new_pair(object_new(SYMBOL, "LIST"), NULL),
        new_pair(p1, NULL))));
    // (lambda (x) (list x));
    object_t *args = new_pair(object_new(NUMBER, &num), NULL);
    object_t *res = macro_call(lx, args, env);
    ASSERT(res->u.value, 10);
}

/** 
 * Тест вызова функции ((lambda (x) x) 10)
 */
void test_eval_func()
{
    printf("test_eval_func: ");

    object_t *x1 = object_new(SYMBOL, "x"); // x
    object_t *param1 = new_pair(x1, NULL); // (x)
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(param1, param1)); // (lambda (x) x)
    
    int numX = 10;
    object_t *arg_x = object_new(NUMBER, &numX); // 10
    object_t *args = new_pair(arg_x, NULL); //(10)
    
    object_t *res = eval_func(list, args, NULL);
    ASSERT(res->type, NUMBER);
    ASSERT(res->u.value, 10);
}

/** 
 * Тест вызова функции ((lambda (x) 10 x) 10)
 */
void test_eval_func2()
{
    printf("test_eval_func2: ");
    
    int numX = 10;
    object_t *arg_x = object_new(NUMBER, &numX);// 10
    object_t *args = new_pair(arg_x, NULL); //(10)
    object_t *x1 = object_new(SYMBOL, "x"); // x
    object_t *param1 = new_pair(x1, NULL); // (x)
    object_t *param2 = new_pair(arg_x, param1);
    object_t *list = new_pair(object_new(SYMBOL, "LAMBDA"), new_pair(param1, param2)); 
       
    object_t *res = eval_func(list, args, NULL);
    ASSERT(res->type, NUMBER);
    ASSERT(res->u.value, 10);
}

/** 
 * Тест создания окружения с числом аргументов большим чем параметров
 */
void test_er_num_arg_make_env()
{
    printf("test_er_num_arg_make_env: \n");
    int num = 1;
    object_t *args = NULL;
    object_t *val = object_new(NUMBER, &num);
    object_t *res = make_env(args, val);
    ASSERT(res, ERROR);
}

/** 
 * Определение нового макроса
 */
void test_defmacro()
{
    printf("test_defmacro: ");
    
    object_t *arg1 = object_new(SYMBOL, "test");
    object_t *arg2 = new_pair(nil, new_pair(nil, NULL));
    int num = 1;
    object_t *arg3 = object_new(NUMBER, &num);
    
    object_t *args = new_pair(arg1, new_pair(arg2, new_pair(arg3, NULL)));
    
    object_t *result = defmacro(args);
    
    ASSERT(result->type, SYMBOL);
    ASSERT(strcmp(result->u.symbol->str, "test"), 0);
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
    test_is_lambda();//14
    test_cond();//13
    test_cond_null();//67
    test_cond_tail_null();//69    
    test_cond_many_params();
    test_make_env();
    test_find_in_env();
    test_defun();//18
    test_setq_set_env();
    test_setq_global_set();
    test_append();
    test_progn();
    test_progn_null();
    test_progn_error();
    test_backquote_nulllist();
    test_backquote_invalid_arg_type();
    test_backquote_arguments();
    test_atom();//1
    test_atom_null();//52
    test_atom_list();
    test_atom_many_args();//131
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
    test_er_num_arg_make_env();
    test_defmacro();
    return 0;
}
