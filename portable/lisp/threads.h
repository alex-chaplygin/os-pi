#ifndef THREADS_H
#define THREADS_H

#include "objects.h"
#include "cont.h"

/// Индексы сохраняемых регистров процессора
enum {
    _AX,
    _BX,
    _CX,
    _DX,
    _SI,
    _DI,
    _BP,
    _SP,
#ifndef X32
    _R8,
    _R9,
    _R10,
    _R11,
    _R12,
    _R13,
    _R14,
    _R15,
#endif
    _IP,
    _FLAGS,
    NUM_REGS
};

/// Поток исполнения
/// При изменении структуры необходимы изменения в x86/switch.asm
typedef struct thread_s
{
    union
    {
        object_t function;		/**< Функция потока (лямбда) */
        func0_t func;			/**< Указатель на функцию-примитив (если встроенная) */
    };
    object_t result;			/**< Результат выполнения */

    object_t current_env;		/**< Локальные переменные */
    object_t func_env;			/**< Локальные функции */
    object_t **protected;		/**< Выделенный массив для protected */
    int last_protected;			/**< Индекс стека защищенных объектов GC */

    continuation_t *tagbody_buffers;	/**< Выделенный массив для tagbody */
    catch_t *catch_buffers;		/**< Выделенный массив для catch */
    int tb_index_buf;			/**< Индекс кадра tagbody */
    int ct_index_buf;			/**< Индекс кадра catch */

    void *stack;			/**< Выделенный стек C (stack base / buffer) */
    object_t regs[NUM_REGS];		/**< Регистры процессора */

    int free; // Если 1 - слот потока свободен

    struct thread_s *next; // указатель на следующий поток
    struct thread_s *prev; // указатель на предыдущий поток
} thread_t;

/// Максимальное количество потоков
#define MAX_THREADS 64
/// Максимальный размер стека потока
#define MAX_THREAD_STACK_SIZE (16 * 1024)

thread_t *get_thread(object_t thread_id);
int get_thread_id(object_t thread_id);
object_t thread_spawn(object_t fun);
void thread_yield();
object_t thread_wait(object_t thread_id);
void thread_kill(object_t thread_id);
void init_threads();

extern thread_t *dead_threads;

int save_regs(thread_t *from);
void restore_regs(thread_t *to);
void free_dead_threads();
void kill_current_thread();
void thread_finish(object_t result);

#endif
