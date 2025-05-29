#include <stdio.h>
#include <stdlib.h>
#include "objects.h"

#define STACK_SIZE 1024
#define FRAME_SIZE 1024
#define MAX_LOCAL_ARGS 16

//Структура кадра активации
typedef struct frame {
    //Число аргументов в кадре
    int count_args;
    //Массив, в котором хранятся локальные аргументы кадра
    object_t local_args[MAX_LOCAL_ARGS];
    //Указатель на предыдущий кадр
    struct frame *prev;
    //Номер глубины вызова
    int call_number;
} frame_t;

//Функция инициализации виртуальной машины
void vm_init(object_t *prog_mem, int prog_size,
			  object_t *const_mem, int const_c, int glob_var_c);

//Функция запуска виртуальной машины
void vm_run();
void vm_dump();
