#include <stdio.h>
#include <objects.h>
#include <test.h>
#include "alloc.h"
#include "vm.h"
#include "test.h"

#define N(x) new_number(x)

enum {
    CONST,
    JMP,
    JNT,
    ALLOC,
    GLOBALREF,
    GLOBALSET,
    LOCALREF,
    LOCALSET,
    DEEPREF,
    DEEPSET,
    PUSH,
    PACK,
    REGCALL,
    RETURN,
    FIXCLOSURE,
    SAVEFRAME,
    SETFRAME,
    RESTOREFRAME,
    PRIM,
    NPRIM,
    HALT
};

extern object_t *pc_reg;
extern object_t acc_reg;
extern object_t *global_var_memory;
extern object_t *stack_top;

void *alloc_region(int size)
{
    return malloc(size);
}

pair_t* alloc_for_pair()
{
    object_t pair = (object_t)malloc(sizeof(pair_t) * 2);
    pair = (pair >> MARK_BIT) + 1 << MARK_BIT;
    return (pair_t*)pair;
}

object_t new_number(int num)
{
    return (object_t)(num << MARK_BIT);
}

int get_value(object_t o)
{
    return ((int)o) >> MARK_BIT;
}

object_t new_pair(object_t left, object_t right) 
{ 
    pair_t *pair = alloc_for_pair();
    
    pair->next = NULL; 
    pair->free = 0; 
    pair->left = left; 
    pair->right = right;
    return NEW_OBJECT(PAIR, pair);
} 

/**

/**
 * Тест инструкции const
 * Программа с командной CONST. Проверяется значение ACC
 */
void test_const()
{
    printf("test_const_init:\n");
    object_t pc_progmem[] = {
	N(CONST), N(0),
	N(HALT)
    };
    object_t pc_constmem[] = {N(1)};
    vm_init(pc_progmem, 3, pc_constmem, 1, 1);
    vm_run();
    ASSERT(get_value(acc_reg), 1);
}

/**
 * Тест инструцкии jmp
 * Тестирует безусловный переход. После выполнения команды JMP 5 должен совершиться относительный переход к команде JMP -7, и, соответсвенно, должна выполниться команда CONST 2. Таким образом, значение ACC должно быть равно 2
 */
void test_jmp()
{
    printf("test_jmp:\n");
    object_t pc_progmem[] = {
	N(CONST), N(0),
        N(JMP), N(5),
        N(CONST), N(1),
	N(HALT),
	N(CONST), N(2),
	N(JMP), N(-7),
	N(HALT)
    };
    object_t pc_constmem[] = {N(1), N(2), N(3)};
    vm_init(pc_progmem, 12, pc_constmem, 3 ,0);
    vm_run();
    ASSERT(get_value(acc_reg),2);
}

/**
 * Тест инструкции jnt
 * Тестирует переход, если ACC == NIL. В ACC записывается 1, а потом NULLOBJ и проверяется, правильно ли работает условие перехода.
 */
void test_jnt()
{
    printf("test_jnt:\n");
    object_t pc_progmem[] = {
	N(CONST), N(1),
	N(JNT),	N(0),
	N(CONST), N(0),
	N(JNT), N(2),
	N(CONST), N(2),
	N(HALT)
    };
    object_t pc_constmem[] = { NULLOBJ, N(1), N(2) };
    vm_init(pc_progmem, 11, pc_constmem, 3, 0);
    vm_run(); 
    ASSERT(get_value(acc_reg), 0);
}

/**
 * Тест инструкций GLOBAL-SET и GLOBAL-REF
 * Устанавливает число 10 в ACC, устанавливает глобальной переменной с индексом 0 значение 10, устанавливает в ACC другое значние и считывает глобальную переменную в регистр ACC. Таким образом, проверяется корректность работы команд GLOBAL-REF и GLOBAL-SET 
 */
void test_global_ref_set()
{
    printf("test_global_ref_set:\n");
    object_t pc_progmem[] = {
	N(CONST), N(0),
	N(GLOBALSET), N(0),
	N(CONST), N(1),
	N(GLOBALREF), N(0),
	N(HALT)
    };

    object_t pc_constmem[] = { N(10), N(20) };
    vm_init(pc_progmem, 5, pc_constmem, 2, 1);
    vm_run();

    int n = get_value(global_var_memory[0]);
    ASSERT(n, 10);
    ASSERT(get_value(acc_reg), 10);
}

/*
 * Тест инструкций PUSH и POP.
 * Помещает в стек числа 0, 1, 2, а затем, используя команду PACK 2, записывает перед 0 список, состоящий из двух чисел: 1 и 2. POP помещает созданный список в ACC
 */
void test_push_pack()
{
    printf("test_push_pack:\n");
    object_t pc_progmem[] = {
	N(CONST), N(0),
	N(PUSH),
	N(CONST), N(1),
	N(PUSH),
	N(CONST), N(2),
	N(PUSH),
	N(PACK), N(2),
	N(HALT)
    };

    object_t pc_constmem[] = { N(0), N(1), N(2) };
    vm_init(pc_progmem, 12, pc_constmem, 3, 1);
    vm_run();
    ASSERT(get_value(FIRST(stack_top[-1])), 1);
    ASSERT(get_value(SECOND(stack_top[-1])), 2);
}

int main()
{  
    test_const();
    test_jmp();
    test_jnt();
    test_global_ref_set();
    test_push_pack();
    return 0;
}
