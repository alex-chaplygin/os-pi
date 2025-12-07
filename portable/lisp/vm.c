#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "objects.h"
#include "alloc.h"
#include "vm.h"
#include "symbols.h"
#include "arith.h"
#include "str.h"
#include "eval.h"
#include "pair.h"
#include "str.h"
#include "array.h"
#include "predicates.h"
#include "bind.h"
#include "arith.h"
#ifdef OS
#include "../../include/x86/sys.h"
#endif

#define FUNCALL 8 // номер примитива funcall
#define APPLY 37 // номер примитива apply
#define REG_CALL 12 // операция return
#define RETURN_OP 13 // операция return
#define VM_THRESHOLD (400 * 1024) // порог сборки мусора

//Размер памяти программы
int program_size;
//Количество констант, переданных виртуальной машине
int const_count;
//Количество глобальных переменных, переданных виртуальной машине
int global_var_count;

//Память программы, в которой хранятся инструкции в виде типа NUMBER
int *program_memory;
//Указатель, ссылающийся на память, в которой хранятся константы
object_t *const_memory;
//Указатель, ссылающийся на память, в которой хранятся глобальные переменные
object_t *global_var_memory;
//Стек, хранящий объекты любых типов
object_t stack[STACK_SIZE];
//Указатель на вершину стека
object_t *stack_top;

//Хранит указатель на текущую выполняемую инструкцию
int *pc_reg;
//Хранит результат последней операции
object_t acc_reg;
//Хранит текущий кадр активации
object_t frame_reg;
//хранит флаг работы машины
int working = 1;

//Таблица примитивов
struct prim {
    void* func; 
    int args_count;
} nprims[] = {
    { add, 0 },
    { sub, 1 },
    { mul, 0 },
    { DIV, 1 },
    { bitwise_and, 0 },
    { bitwise_or, 0 },
    { bitwise_xor, 0 },
    { concat, 0 },
    { funcall, 1 },
    { print_object, 0 },
    { error_func, 0 }
}, prims[] = {
    car , 1, cdr , 1, atom , 1, new_pair , 2, rplaca , 2, rplacd , 2,
    mod , 2, shift_left , 2, shift_right , 2, eq , 2, equal , 2, gt , 2, less , 2,
    SIN , 1, COS , 1, SQRT , 1,
    intern , 1, symbol_name , 1, symbol_function , 1, string_size , 1, int_to_str , 1, code_char , 1, char_code , 1, PUTCHAR , 1, str_char , 2, subseq , 3,
    make_array , 1, make_string , 2, array_size , 1, aref , 2, seta , 3, sets , 3,
    symbolp , 1, integerp , 1, pairp , 1, functionp , 1, gensym , 0, apply , 2,
    ROUND, 1,
#ifdef OS
    INB, 1,
    INW, 1,
    INDW, 1,
    INSW, 2,
    OUTB, 2,
    OUTW, 2,
    OUTDW, 2,
    OUTSW, 2,
    SET_CURSOR, 2,
    SET_COLOR, 1,
    SET_BACK_COLOR, 1,
    HIDE_CURSOR, 0,
    SHOW_CURSOR, 0,
    SET_INT_HANDLER, 2,
    send_text_buffer, 5,
    send_graphics_buffer, 5,
    MEMCPY, 2
#endif
};

void (*instructions[])() =
    {
	const_inst,
	jmp_inst,
	jnt_inst,
	alloc_inst,
	global_ref_inst,
	global_set_inst,
	local_ref_inst,
	local_set_inst,
	deep_ref_inst,
	deep_set_inst,
	push_inst,
	pack_inst,
	reg_call_inst,
	return_inst,
	fix_closure_inst,
	save_frame_inst,
	set_frame_inst,
	restore_frame_inst,
	prim_inst,
	nprim_inst,
	halt,
	prim_closure,
	nprim_closure,
	catch_inst,
	throw_inst,
    };

/** 
 * @brief Инициализирует виртуальную машину 
 *
 * @param *prog_mem - память программы
 * @param prog_size - размер памяти программы
 * @param *const_mem - память программы, в которой хранятся константы 
 * @param const_c - количество констант, переданных виртуальной машине
 * @param glob_var_c - количество глобальных переменных, переданных виртуальной машине
 */
void vm_init(int *prog_mem, int prog_size, object_t *const_mem, int const_c, int glob_var_c)
{
    program_size = prog_size;
    program_memory = prog_mem;
    const_count = const_c;
    const_memory = const_mem;
    global_var_count = glob_var_c;
    global_var_memory = alloc_region(global_var_count * sizeof(object_t));
    global_var_memory[0] = t;
    global_var_memory[1] = const_memory[1] = NULLOBJ;
    pc_reg = program_memory;
    acc_reg = NULLOBJ;
    stack_top = stack + STACK_SIZE - 1;
    frame_reg = NULLOBJ;
    working = 1;
}

/**
 * @brief Извлекает очередной код из программы и передвигает указатель
 */
int fetch()
{
    return *pc_reg++;
}

/**
 * @brief Функция, помещающая константу с номером num в регистр ACC
 */
void const_inst()
{
    int n = fetch();
    acc_reg = const_memory[n];
#ifdef DEBUG    
    printf("CONST %d ", n);
    PRINT(acc_reg);
#endif    
}

/**
 * @brief Функция безусловного перехода на ofs относительно PC
 */
void jmp_inst()
{
    int ofs = fetch(); 
    pc_reg += ofs - 2;
#ifdef DEBUG    
    printf("JMP %d\n", ofs);
#endif    
}

/**
 * @brief Функция относительного перехода на смещение ofs, если ACC == NIL
 */
void jnt_inst()
{
    int ofs = fetch();
    if (acc_reg == NULLOBJ)
        pc_reg += ofs - 2;
#ifdef DEBUG
    printf("JNT %d ", ofs);
    PRINT(acc_reg);
#endif    
}

/**
 * @brief Функция, устанавливающая регистру ACC значение глобальной переменной с индексом i
 */
void global_ref_inst()
{
    int i = fetch();
    acc_reg = global_var_memory[i]; 
#ifdef DEBUG
    printf("GLOBAL-REF %d ", i);
    PRINT(acc_reg);
#endif    
}

/**
 * @brief Функция, устанавливающая глобальной переменной с индексом i значение регистра ACC
 */
void global_set_inst()
{
    int i = fetch();
    global_var_memory[i] = acc_reg;
#ifdef DEBUG
    printf("GLOBAL-SET %d ", i);
    PRINT(acc_reg);
#endif    
}

/**
 * @brief Функция, загружающая в ACC значение локальной переменной с индексом (текущего кадра активации)
 */
void local_ref_inst()
{
    int i = fetch();
    acc_reg = GET_ARRAY(frame_reg)->data[i + 2];
#ifdef DEBUG
    printf("LOCAL-REF %d ", i);
    PRINT(acc_reg);
#endif    
}

/**
 * @brief Функция, присваивающая локальной переменной i (текущего кадра активации) значение регистра ACC
 */
void local_set_inst()
{
    int i =  fetch();
    GET_ARRAY(frame_reg)->data[i + 2] = acc_reg;
#ifdef DEBUG
    printf("LOCAL-SET %d ", i);
    PRINT(acc_reg);
#endif    
}

/**
 * @brief Функция, загружающая в ACC значение локальной переменной с индексом j в кадре i (начиная от текущего)
 */
void deep_ref_inst()
{
    int frame_num = fetch();
    int var_num = fetch();
    object_t frame = frame_reg;
    for (int i = 0; i < frame_num; i++)
	frame = GET_ARRAY(frame)->data[0];
    acc_reg = GET_ARRAY(frame)->data[var_num + 2];
#ifdef DEBUG
    printf("DEEP-REF %d %d ", frame_num, var_num);
    PRINT(acc_reg);
#endif    
}

/**
 * @brief Функция, присваивающая локальной переменной j в кадре i значение регистра ACC
 */
void deep_set_inst()
{
    int frame_num = fetch();
    int var_num = fetch();
    object_t frame = frame_reg;
    for (int i = 0; i < frame_num; i++)
	frame = GET_ARRAY(frame)->data[0];
    GET_ARRAY(frame)->data[var_num + 2] = acc_reg;
#ifdef DEBUG
    printf("DEEP-SET %d %d ", frame_num, var_num);
    PRINT(acc_reg);
#endif    
}

/** 
 * @brief Помещает объект в стек
 */
void push(object_t obj)
{
    if (stack_top < stack)
	error("stack overflow");
    else
    	*stack_top-- = obj;
}

/**
 * @brief Функция, добавляющая значение регистра ACC в стэк
 */
void push_inst()
{
    push(acc_reg);
#ifdef DEBUG
    printf("PUSH ");
    PRINT(acc_reg);
#endif    
}

/** 
 * Выталкивание значения из стека
 *
 * @return объект из стека
 */
object_t pop()
{
    return *(++stack_top);
}

/**
 * @brief Функция, создающая новый кадр активации с числом аргументов n.
 * Извлекает из стека аргументы начиная с позиции 1 (0-й элемент остается в стеке)
 */
void alloc(int n)
{
    array_t *ar = new_empty_array(n + 2);
    object_t frame = NEW_OBJECT(ARRAY, ar);
    ar->data[0] = frame_reg;
    if (frame_reg == NULLOBJ)
	ar->data[1] = 0;
    else
    ar->data[1] = (object_t)(GET_ARRAY(frame_reg)->data[1] + (1 << MARK_BIT));
    object_t fr = pop();
    for (int i = 0; i < n; i++)
	ar->data[n - i + 1] = pop();
    push(fr);
    frame_reg = frame;
}

void alloc_inst()
{
    int n =  fetch();
    alloc(n);
#ifdef DEBUG
    printf("ALLOC %d ", n);
    PRINT(frame_reg);
#endif    
}

/**
 * @brief Функция, собирающая последние n элементов из стека в список и добавляет его в стек
 */
void pack_inst()
{
    int n = fetch();
    object_t list = NULLOBJ;
    for (int i = 0; i < n; i++)
    	list = new_pair(pop(), list);
    push(list);
#ifdef DEBUG
    printf("PACK %d ", n);
    PRINT(list);
#endif    
}

/**
 * @brief Вызов подпрограммы
 * Функция, добавляющая адрес следующей инструкции в стэк и производит переход по смещению ofs
 */
void call(int *addr)
{
    push((object_t)(pc_reg - program_memory << MARK_BIT));
    pc_reg = addr;
}

void reg_call_inst()
{
    int ofs = fetch();
    call(pc_reg + ofs - 2);
#ifdef DEBUG
    printf("REG-CALL %d\n", ofs);
#endif    
}

/**
 * @brief Возврат из подпрограммы
 * Функция, производящая переход на адрес из верхушки стэка, при этом удаляет этот адрес из стека
 */
void return_inst()
{
    pc_reg = program_memory + (pop() >> MARK_BIT);
#ifdef DEBUG
    printf("RETURN\n");
#endif    
}

/**
 * @brief Функция, добавляющая в регистр ACC объект замыкание с текущим кадром активации и смещением на код функции относительно текущего адреса ofs
 */
void fix_closure_inst()
{
    int ofs = fetch();
    acc_reg = new_function(NULLOBJ, (pc_reg + ofs - program_memory - 2 << MARK_BIT), frame_reg, NULLOBJ);
#ifdef DEBUG
    printf("FIX-CLOSURE %d\n", ofs);
#endif    
}


/**
 * @brief Функция, сохраняющая кадр активации в стеке
 */
void save_frame_inst()
{
    push(frame_reg);
#ifdef DEBUG
    printf("SAVE-FRAME ");
    PRINT(frame_reg);
#endif    
}

/**
 * @brief Функция, устанавливающая кадр активации с номером num относительно начала глубины вызовов
 */
void set_frame_inst()
{
    int num = fetch() - 1;
    object_t frame = frame_reg;
    int count;
    if (frame != NULLOBJ) {
	count = (GET_ARRAY(frame)->data[1] >> MARK_BIT) - num;
	for (int i = 0; i < count; i++)
	    frame = GET_ARRAY(frame)->data[0];
	frame_reg = frame;
    }
#ifdef DEBUG
    printf("SET-FRAME %d ", num);
    PRINT(frame_reg);
#endif    
}

/**
 * @brief Функция, восстанавливающая кадр активации из стека
 */
void restore_frame_inst()
{
    frame_reg = pop();
#ifdef DEBUG
    printf("RESTORE-FRAME ");
    PRINT(frame_reg);
#endif    
}

/**
 * @brief Применение объекта функции (примитива) fun к аргументам args
 */
void vm_apply(object_t fun, object_t args)
{
    extern int total_arrays;
    int calls = 1;
    if (TYPE(fun) != FUNCTION)
	error("vm_apply: not function\n");
    function_t *f = GET_FUNCTION(fun);
    if (f->func != NULL) {
	acc_reg = call_form(f->func, args, f->nary, f->count, f->count);
	return;
    }
    int c = 0;
    while (args != NULLOBJ) {
	push(FIRST(args));
	args = TAIL(args);
	c++;
    }
    push(frame_reg);
    frame_reg = f->env;
    alloc(c);
    call(program_memory + (f->body >> MARK_BIT));
    do {
	if (total_arrays >= VM_THRESHOLD)
	    vm_garbage_collect();
	c = fetch();
	if (c == REG_CALL)
	    calls++;
	else if (c == RETURN_OP)
	    calls--;
#ifdef DEBUG    
	printf("%d: ", pc_reg - program_memory);
#endif
	instructions[c]();
#ifdef BIGDEBUG    
	vm_dump();
#endif
    } while (calls != 0);
    frame_reg = pop();
}

/**
 * @brief Функция, вызывающая примитив с номером n из таблицы примитивов с фиксированным числом аргументов
 */
void prim_inst()
{
    int n = fetch();
    object_t arg1, arg2, arg3, arg4, arg5;
#ifdef DEBUG
    printf("PRIM %d ", n);
#endif
    if (n == APPLY) {
	arg1 = pop();
	vm_apply(pop(), arg1);
    } else {
	struct prim *pr = &prims[n];
	switch (pr->args_count) {
	case 0:
	    acc_reg = ((func0_t)pr->func)();
	    break;
	case 1:
	    acc_reg = ((func1_t)pr->func)(pop());
	    break;
	case 2:
	    arg2 = pop();
	    arg1 = pop();
	    acc_reg = ((func2_t)pr->func)(arg1, arg2);
	    break;
	case 3:
	    arg3 = pop();
	    arg2 = pop();
	    arg1 = pop();
	    acc_reg = ((func3_t)pr->func)(arg1, arg2, arg3);
	    break;
	case 5:
	    arg5 = pop();
	    arg4 = pop();
	    arg3 = pop();
	    arg2 = pop();
	    arg1 = pop();
	    acc_reg = ((func5_t)pr->func)(arg1, arg2, arg3, arg4, arg5);
	    break;
	default:
	    printf("primitive with %d arguments", pr->args_count);
	    break;
	}
    }
#ifdef DEBUG
    PRINT(acc_reg);
#endif    
}

/**
 * @brief Функция, берет из стека нужное число аргументов, это число фиксированных аргументов + 1 (список переменных аргументов), вызывает функцию примитива с аргументами, результат записывается в ACC.
 */
void nprim_inst()
{
    int n = fetch();
#ifdef DEBUG
    printf("NPRIM %d ", n);
#endif    
    object_t args = pop();
    if (n == FUNCALL)
	vm_apply(pop(), args);
    else {
	struct prim *pr = &nprims[n];
	switch (pr->args_count) {
	case 0:
	    acc_reg = ((func1_t)pr->func)(args);
	    break;
	case 1:
	    acc_reg = ((func2_t)pr->func)(pop(), args);
	    break;
	default:
	    printf("nary primitive with %d arguments", pr->args_count);
	    break;
	}
    }
#ifdef DEBUG
    PRINT(acc_reg);
#endif    
}

/**
 * @brief остановка вм
 */
void halt()
{
#ifdef DEBUG
    printf("HALT\n");
#endif    
    working = 0;
}

void prim_closure()
{
    int n = fetch();
    acc_reg = new_prim_function((func0_t)prims[n].func, 0, prims[n].args_count);
#ifdef DEBUG
    printf("PRIM_CLOSURE %d\n", n);
#endif    
}

void nprim_closure()
{
    int n = fetch();
    acc_reg = new_prim_function((func0_t)nprims[n].func, 1, nprims[n].args_count);
#ifdef DEBUG
    printf("NPRIM_CLOSURE %d\n", n);
#endif    
}

void catch_inst()
{
    fetch();
    //    error("CATCH");
}

void throw_inst()
{
    pop();
    PRINT(acc_reg);
    error("THROW");
}   

/** 
 * Печать состояния виртуальной машины
 */
void vm_dump()
{
    object_t o;
    printf("--------vm_dump--------------\n");
    printf("ACC: ");
    PRINT(acc_reg);
    printf("FRAME: ");
    PRINT(frame_reg);
    printf("STACK: \n");
    for (int i = stack_top - stack + 1; i < STACK_SIZE; i++) {
	o = stack[i];
	PRINT(o);
    }
    printf("-------------------------\n");
}

/** 
 * Сборка мусора при работе виртуальной машины
 */
void vm_garbage_collect()
{
    int i;
    object_t *c;
    extern int total_arrays;
    extern object_t consts;
    extern object_t static_bind[];
#ifdef DEBUG
    printf("VM garbage collect: arrays = %d\n", total_arrays);
    //    printf("mark const\n");
#endif
    mark_object(consts);
    for (i = 0, c = static_bind; i < last_static; i++)
	mark_object(*c++);    
    for (i = 0, c = global_var_memory; i < global_var_count; i++)
	mark_object(*c++);
    //    printf("mark stack\n");
    for (i = stack_top - stack + 1, c = stack_top + 1; i < STACK_SIZE; i++)
	mark_object(*c++);
    //    printf("mark registers\n");
    mark_object(acc_reg);
    mark_object(frame_reg);
    //    printf("sweep\n");
    sweep();
#ifdef DEBUG
    printf("VM garbage collect done: arrays = %d\n", total_arrays);
#endif
    total_arrays = 0;
}

/**
 * @brief Запускает виртуальную машину
 */
void vm_run()
{
    extern int total_arrays;
    while (working == 1)
    {
	if (total_arrays >= VM_THRESHOLD)
	    vm_garbage_collect();
#ifdef DEBUG    
	printf("%d: ", pc_reg - program_memory);
#endif
	instructions[fetch()]();
#ifdef BIGDEBUG    
	vm_dump();
#endif
    }
}

