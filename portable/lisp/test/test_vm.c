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

void *alloc_region(int size)
{
    return malloc(size);
}

object_t new_number(int num)
{
    return (object_t)(num << MARK_BIT);
}

int get_value(object_t o)
{
    return ((int)o) >> MARK_BIT;
}

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
    printf("test_global_set:\n");
    object_t pc_progmem[] = {
	N(CONST), N(0),
	N(GLOBALSET), N(0),
	N(CONST), N(1),
	N(GLOBALREF), N(0),
	N(HALT)
    };

    object_t pc_constmem[] = { N(10), N(20) };
    vm_init(pc_progmem, 5, pc_constmem, 1, 1);
    vm_run();

    int n = get_value(global_var_memory[0]);
    ASSERT(n, 10);
    ASSERT(get_value(acc_reg), 10);
}

int main()
{
    test_const();
    test_jmp();
    test_jnt();
    test_global_ref_set();
    return 0;
}
