#include "alloc.h"
#include "vm.h"

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
obcject_t *global_var_memory;
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

//Функция, помещающая константу с номером num в регистр ACC
void const_inst() {}
//Функция безусловного перехода на ofs относительно PC
void jmp_inst() {}
//Функция относительного перехода на смещение ofs, если ACC != T
void jnt_inst() {}
/*Функция, создающая новый кадр активации с числом аргументов n.
 *Извлекает из стека аргументы начиная с позиции 1 (0-й элемент остается в стеке) */
void alloc_inst() {}
//Функция, устанавливающая регистру ACC значение глобальной переменной с индексом i
void global_ref_inst() {}
//Функция, устанавливающая глобальной переменной с индексом i значение регистра ACC
void global_set_inst() {}
//Функция, загружающая в ACC значение i локальной переменной (текущего кадра активации)
void local_ref_inst() {}
//Функция, присваивающая локальной переменной i (текущего кадра активации) значение регистра ACC
void local_set_inst() {}
//Функция, загружающая в ACC значение локальной переменной с индексом j в кадре i (начиная от текущего)
void deep_ref_inst() {}
//Функция, присваивающая локальной переменной j в кадре i значение регистра ACC
void deep_set_inst() {}
//Функция, добавляющая значение регистра ACC в стэк
void push_inst() {}
//Функция, собирающая последние n элементов из стека в список и добавляет его в стек
void pack_inst() {}
//Функция, добавляющая адрес следующей инструкции в стэк и производит переход по смещению ofs
void reg_call_inst() {}
//Функция, производящая переход на адрес из верхушки стэка, при этом удаляет этот адрес из стэка
void return_inst() {}
/*Функция, добавляющая в регистр ACC объект замыкание с текущим кадром активации и смещением
  на код функции относительно текущего адреса ofs */
void fix_closure_inst() {}
//Функция, сохраняющая кадр активации в стеке
void save_frame_inst() {}
//Функция, устанавливающая кадр активации с номером num относительно начала глубины вызовов
void set_frame_inst() {}
//Функция, восстанавливающая кадр активации из стека
void restore_frame_inst() {}
//Функция, вызывающая примитив с номером n из таблицы примитивов с фиксированным числом аргументов
void prim_inst() {}
//Функция, вызывающая примитив с номером n из таблицы примитивов с переменным числом аргументов
void nprim_inst() {}

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
    };

/**
 * @brief Запускает виртуальную машину
 */
void vm_run()
{
    while (1)
	instructions[fetch()]();
}
