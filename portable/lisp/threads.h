#include <setjmp.h>
#include "objects.h"
#include "cont.h"

typedef struct thread_s
{
    union
    {
        object_t function; // функция потока (лямбда)
        func0_t func;      // указатель на функцию-примитив (если встроенная)
    };
    object_t result; // результат выполнения

    object_t current_env; // локальные переменные
    object_t func_env;    // локальные функции
    object_t **protected; // выделенный массив для protected
    int last_protected;   // индекс стека защищенных объектов GC

    continuation_t *tagbody_buffers; // выделенный массив для tagbody
    catch_t *catch_buffers;          // выделенный массив для catch
    int tb_index_buf;                // индекс кадра tagbody
    int ct_index_buf;                // индекс кадра catch

    void *stack;        // выделенный стек C (stack base / buffer)
    void *sp;           // сохраненный указатель вершины стека (ESP/RSP)
    jmp_buf jmp_buffer; // регистры процессора

    int free; // Если 1 - слот потока свободен

    struct thread_s *next; // указатель на следующий поток
    struct thread_s *prev; // указатель на предыдущий поток
#ifdef X32
    int pad[6]; // выравнивание 40 + 24 = 64 байта
#endif
} thread_t;

// Максимальное количество потоков
#define MAX_THREADS 64
// Максимальный размер стека потока
#define MAX_THREAD_STACK_SIZE (16 * 1024)

thread_t *get_thread(object_t thread_id);

int get_thread_id(object_t thread_id);

object_t thread_spawn(object_t fun);

void thread_yield();

object_t thread_wait(object_t thread_id);

void thread_kill(object_t thread_id);
void init_threads();