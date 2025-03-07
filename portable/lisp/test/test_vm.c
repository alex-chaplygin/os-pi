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
object_t t = 1;

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
 */
void test_const()
{
    printf("test_const_init: ");
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
 */
void test_jmp()
{
    printf("test_jmp: ");
    object_t pc_progmem[] = {
	N(CONST), N(0),
        N(JMP), N(5),
        N(CONST), N(1),
	N(HALT),
	N(CONST), N(2),
	N(JMP), N(-3),
	N(HALT)
    };
    object_t pc_constmem[] = {N(1), N(2), N(3)};
    vm_init(pc_progmem, 17, pc_constmem, 3 ,0);
    vm_run();
    ASSERT(get_value(acc_reg), 2);
}

/**
 * Тест инструкции jnt
 */
void test_jnt()
{
    printf("test_jnt: ");
    object_t pc_progmem[] = {
	N(CONST), N(0),
	N(JNT),	N(5),
	N(CONST), N(1),
	N(HALT),
	N(CONST), N(2),
	N(JNT),	N(5),
	N(CONST), N(3),
	N(HALT),
	N(CONST), N(4),
	N(HALT)
    };
    object_t pc_constmem[] = {N(1), N(1), N(0), N(2), N(3)};
    vm_init(pc_progmem, 17, pc_constmem, 5, 0);
    vm_run();
    ASSERT(get_value(acc_reg),2);
}

/**
 * Тест инструкции HALT
 */
void test_halt()
{
    printf("test_halt: ");
    object_t pc_progmem[] = {};
    object_t pc_constmem[] = {N(2),N(3)};
    vm_init(pc_progmem,5,pc_constmem,2,0);
    vm_run();
    ASSERT(get_value(acc_reg),3);
}

int main()
{
    test_const();
    test_jmp();
    return 0;
}
