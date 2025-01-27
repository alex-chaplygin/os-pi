#include "alloc.h"
#include "vm.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

//Размер памяти программы
int program_size;
//Количество констант, переданных виртуальной машине
int const_count;
//Количество глобальных переменных, переданных виртуальной машине
int global_var_count;

//Память программы, в которой хранятся инструкции в виде типа NUMBER
object_t *program_memory;
//Указатель, ссылающийся на память, в которой хранятся константы
object_t *const_memory;
//Указатель, ссылающийся на память, в которой хранятся глобальные переменные
object_t *global_var_memory;
//Стек, хранящий объекты любых типов
object_t stack[STACK_SIZE];
//Указатель на вершину стека
object_t *stack_top;
//Хранит указатель на последний кадр активации машины 
frame_t frame_activation_list[FRAME_SIZE];

//Хранит указатель на текущую выполняемую инструкцию
object_t *pc_reg;
//Хранит результат последней операции
object_t acc_reg;
//Хранит текущий кадр активации
frame_t *frame_reg;
//хранит флаг работы машины
int working = 1;

/** 
 * @brief Инициализирует виртуальную машину 
 *
 * @param *prog_mem - память программы
 * @param prog_size - размер памяти программы
 * @param *const_mem - память программы, в которой хранятся константы 
 * @param const_c - количество констант, переданных виртуальной машине
 * @param glob_var_c - количество глобальных переменных, переданных виртуальной машине
 */
void vm_init(object_t *prog_mem, int prog_size, object_t *const_mem, int const_c, int glob_var_c)
{
    program_size = prog_size;
    program_memory = prog_mem;
    const_count = const_c;
    const_memory = const_mem;
    global_var_count = glob_var_c;
    global_var_memory = alloc_region(global_var_count * sizeof(object_t));
    pc_reg = program_memory;
    acc_reg = NULLOBJ;
    frame_reg = NULL;
}

/**
 * @brief Извлекает очередной код из программы и передвигает указатель
 */
int fetch()
{
    return *pc_register++ >> MARK_BIT;
}

/**
 * @brief Функция, помещающая константу с номером num в регистр ACC
 */
void const_inst()
{
    acc_reg = const_memory[fetch()];
    printf("Константа с номером помещена в регистр АСС");
}

/**
 * @brief Функция безусловного перехода на ofs относительно PC
 */
void jmp_inst()
{
    pc_reg += fetch();
}

/**
 * @brief Функция относительного перехода на смещение ofs, если ACC != T
 */
void jnt_inst()
{
    if (acc_reg != t)
        pc_reg += fetch();
}

/**
 * @brief Функция, создающая новый кадр активации с числом аргументов n.
 * Извлекает из стека аргументы начиная с позиции 1 (0-й элемент остается в стеке)
 */
void alloc_inst()
{
    int n =  fetch(); 
    frame_reg = (frame_t *)stack_top; 
    stack_top += n;
}

/**
 * @brief Функция, устанавливающая регистру ACC значение глобальной переменной с индексом i
 */
void global_ref_inst()
{
    int i = fetch();
    acc_reg = global_var_memory[i]; 
    printf("Глобальная переменная с индексом %d в ACC", i);
}

/**
 * @brief Функция, устанавливающая глобальной переменной с индексом i значение регистра ACC
 */
void global_set_inst()
{
    int i = fetch();
    frame_reg->local_args[i] = acc_reg;
    printf("Значение ACC сохранено в глобальной переменной с индексом %d", i);
}

/**
 * @brief Функция, загружающая в ACC значение локальной переменной с индексом (текущего кадра активации)
 */
void local_ref_inst()
{
    int i = fetch();
    acc_reg = frame_reg->local_args[i];
    printf("Локальная переменная с индексом %d загружена в ACC\n", i);
}

/**
 * @brief Функция, присваивающая локальной переменной i (текущего кадра активации) значение регистра ACC
 */
void local_set_inst()
{
    int i =  fetch();
    frame_reg->local_args[i] = acc_reg;
    printf("Значение ACC сохранено в локальной переменной с индексом %d", i);
}

/**
 * @brief Функция, загружающая в ACC значение локальной переменной с индексом j в кадре i (начиная от текущего)
 */
void deep_ref_inst()
{
    int i = fetch();
    int depth = fetch();
    frame_t *target_frame = frame_reg;

    for (int d = 0; d < depth; d++) {
        target_frame = target_frame->prev;
    }

    acc_reg = target_frame->local_args[i];
}

/**
 * @brief Функция, присваивающая локальной переменной j в кадре i значение регистра ACC
 */
void deep_set_inst()
{
    int i = fetch();
    int depth = fetch();
    frame_t *target_frame = frame_reg;

    for (int d = 0; d < depth; d++)
        target_frame = target_frame->prev;

    target_frame->local_args[i] = acc_reg;
}

/**
 * @brief Функция, добавляющая значение регистра ACC в стэк
 */
void push_inst()
{
    *stack_top++ = acc_reg; 
    printf("Значение ACC помещено в стек");
}

/**
 * @brief Функция, собирающая последние n элементов из стека в список и добавляет его в стек
 */
void pack_inst()
{
    int n =  fetch();
    int i=0;
    object_t list[100];
    while(n!=0){
	list[i]=*stack_top;
	stack_top--;
	n--;
    }
    *stack_top++ = list; 
}

/**
 * @brief  Функция, добавляющая адрес следующей инструкции в стэк и производит переход по смещению ofs
 */
void reg_call_inst() {
    object_t *return_address = pc_register++; 
    *stack_top++ = pc_register; 
    int ofs =  fetch(); 
    pc_reg += ofs; 
}

/**
 * @brief Функция, производящая переход на адрес из верхушки стэка, при этом удаляет этот адрес из стека
 */
void return_inst() {
    stack_top--;
    pc_reg = *stack_top; 
    printf("Возврат к адресу %p", pc_reg);
}

/**
 * @brief Функция, добавляющая в регистр ACC объект замыкание с текущим кадром активации и смещением на код функции относительно текущего адреса ofs
 */
void fix_closure_inst() {
    int ofs = fetch();
    acc_reg = *frame_reg;
    pc_reg += ofs;
    printf("Создано замыкание с текущим кадром и смещением %d\n", ofs);
}


/**
 * @brief Функция, сохраняющая кадр активации в стеке
 */
void save_frame_inst() {
    *stack_top++ = (object_t)frame_reg;
}

/**
 * @brief Функция, устанавливающая кадр активации с номером num относительно начала глубины вызовов
 */
void set_frame_inst() {
    int num = fetch();
    frame_reg = frame_activation_list[num];  
}

/**
 * @brief Функция, восстанавливающая кадр активации из стека
 */
void restore_frame_inst() {
    stack_top--;
    frame_reg = (frame_t *)*stack_top;
    printf("Кадр активации восстановлен");
}

/**
 * @brief Функция, вызывающая примитив с номером n из таблицы примитивов с фиксированным числом аргументов
 */
void prim_inst() {
    object_t n = fetch();
    instructions[n]();
}


/**
 * @brief Функция, вызывающая примитив с номером n из таблицы примитивов с переменным числом аргументов
 */
void nprim_inst() {
    object_t arg = fetch();
    object_t args[100];
    args[0] = arg;
    int i = 1;

    while ((arg = fetch()) != NIL) {
        args[i++] = arg;
    }

    instructions[args[0]]();
}

/**
 * @brief остановка вм
 */
void halt()
{
    printf("!!!machine stopped!!!");
    working = 0;
}

void (*instructions)()[] =
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
	halt
    };

/**
 * @brief Запускает виртуальную машину
 */
void vm_run()
{
    while (working == 1)
	instructions[fetch()]();
}
