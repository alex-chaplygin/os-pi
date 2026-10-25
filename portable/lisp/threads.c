#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "objects.h"
#include "bind.h"
#include "threads.h"
#include "symbols.h"

extern object_t current_env;
extern object_t func_env;
extern int last_protected;
extern int tb_index_buf;
extern int ct_index_buf;
extern continuation_t *tagbody_buffers;
extern catch_t *catch_buffers;

thread_t threads[MAX_THREADS];
thread_t *free_threads = NULL;
int total_threads = 1;
thread_t *current_thread = threads;

void alloc_thread(thread_t *thread)
{
    thread->protected = malloc(sizeof(object_t *) * MAX_PROTECTED);
    thread->tagbody_buffers = malloc(sizeof(continuation_t) * MAX_TAGBODY_SIZE);
    thread->catch_buffers = malloc(sizeof(catch_t) * MAX_CATCH_SIZE);
    thread->stack = malloc(MAX_THREAD_STACK_SIZE);
}

void free_thread(thread_t *thread)
{
    free(thread->protected);
    free(thread->tagbody_buffers);
    free(thread->catch_buffers);
    free(thread->stack);
}

void save_env(thread_t *from)
{
    // сохранение стека потока
    from->current_env = current_env;
    from->func_env = func_env;
    from->last_protected = last_protected;
    from->tb_index_buf = tb_index_buf;
    from->ct_index_buf = ct_index_buf;
}

void restore_env(thread_t *to)
{
    // восстановление стека потока
    current_env = to->current_env;
    func_env = to->func_env;
    protected = to->protected;
    last_protected = to->last_protected;
    tb_index_buf = to->tb_index_buf;
    tagbody_buffers = to->tagbody_buffers;
    catch_buffers = to->catch_buffers;
    ct_index_buf = to->ct_index_buf;
}

void switch_to(thread_t *from, thread_t *to)
{
    printf("switch_to: not implemented");
}

/**
 * Получает указатель на поток по его идентификатору
 *
 * @param thread_id - идентификатор потока
 *
 * @return указатель на поток
 */
thread_t *get_thread(object_t thread_id)
{
    int id = get_thread_id(thread_id);
    return threads + id;
}

/**
 * Получает идентификатор потока из объекта
 *
 * @param thread_id - объект идентификатора потока
 *
 * @return идентификатор потока
 */
int get_thread_id(object_t thread_id)
{
    if (!IS_NUMBER(thread_id))
        error("get_thread: thread_id is not a number");
    int id = get_value(thread_id);
    if (id < 0 || id >= MAX_THREADS)
        error("get_thread: invalid thread id");
    return id;
}

/**
 * Создает новый поток и ставит в конец очереди потоков
 *
 * @param fun - функция потока (лямбда)
 *
 * @return ID созданного потока
 */
object_t thread_spawn(object_t fun)
{
    if (free_threads == NULL)
        error("Error: out of memory: threads");

    thread_t *thread = free_threads;
    free_threads = free_threads->next;

    thread->next = current_thread;
    thread->prev = current_thread->prev;
    current_thread->prev->next = thread;
    current_thread->prev = thread;
    alloc_thread(thread);

    thread->last_protected = 0;
    thread->tb_index_buf = 0;
    thread->ct_index_buf = 0;

    thread->free = 0;
    thread->function = fun;
    thread->result = 0;
    total_threads++;
    return new_number(thread - threads);
}

/**
 * Передает управление другому потоку
 */
void thread_yield()
{
    thread_t *next_thread = current_thread->next;
    if (current_thread == next_thread)
        return; // если в очереди только один поток, то ничего не делаем

    save_env(current_thread);
    restore_env(next_thread);
    switch_to(current_thread, next_thread);
    current_thread = next_thread;
}

/**
 * Ожидает завершения одного или нескольких потоков
 *
 * @param thread_id - идентификатор потока
 * @return результат выполнения (объект или массив объектов результатов)
 */
object_t thread_wait(object_t thread_id)
{
    thread_t *thread = get_thread(thread_id);
    error("thread_wait: not implemented");
    return 0;
}

/**
 * Завершает поток
 *
 * @param thread_id - идентификатор потока
 */
void thread_kill(object_t thread_id)
{
    thread_t *thread = get_thread(thread_id);
    if (thread == NULL)
        error("thread_kill: null pointer: obj");
    if (thread->free)
        return;
    if (thread == threads) // если это главный поток
        error("thread_kill: cannot kill main thread");

    thread->result = 0;

    thread_t *next_thread = thread->next;

    free_thread(thread);

    // удаляем поток из списка активных потоков
    thread->prev->next = thread->next;
    thread->next->prev = thread->prev;

    // добавляем поток в список свободных потоков
    thread->next = free_threads;
    thread->prev = NULL;
    free_threads = thread;

    thread->free = 1;
    total_threads--;

    // переключаемся на следующий поток, если текущий поток был убит
    if (current_thread == thread)
    {
        restore_env(next_thread);
        switch_to(current_thread, next_thread);
        current_thread = next_thread;
    }
}

/**
 * Инициализация потоков
 */
void init_threads()
{
    register_func("THREAD-SPAWN", (func0_t)thread_spawn, 0, 1);
    register_func("THREAD-YIELD", (func0_t)thread_yield, 0, 1);
    register_func("THREAD-WAIT", (func0_t)thread_wait, 0, 1);
    register_func("THREAD-KILL", (func0_t)thread_kill, 0, 1);

    // инициализация главного потока
    current_thread->free = 0;
    current_thread->next = current_thread;
    current_thread->prev = current_thread;

    alloc_thread(current_thread);

    protected = current_thread->protected;
    tagbody_buffers = current_thread->tagbody_buffers;
    catch_buffers = current_thread->catch_buffers;

    total_threads = 1;

    // заполнение free_threads
    free_threads = threads + 1; // первый свободный поток после нулевого (главного) потока
    thread_t *free_thread;
    for (free_thread = free_threads; free_thread < threads + MAX_THREADS - 1; free_thread++)
    {
        free_thread->next = free_thread + 1;
        free_thread->free = 1;
    }

    // явное указание последнего свободного потока
    free_thread->next = NULL;
    free_thread->free = 1;
}
