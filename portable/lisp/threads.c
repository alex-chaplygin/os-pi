#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include "objects.h"
#include "bind.h"
#include "threads.h"
#include "symbols.h"
#include "eval.h"

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

/**
 * Выделяет динамическую память для буферов потока
 *
 * @param thread - указатель на структуру потока
 */
void alloc_thread(thread_t *thread)
{
    thread->protected = malloc(sizeof(object_t *) * MAX_PROTECTED);
    thread->tagbody_buffers = malloc(sizeof(continuation_t) * MAX_TAGBODY_SIZE);
    thread->catch_buffers = malloc(sizeof(catch_t) * MAX_CATCH_SIZE);
    thread->stack = malloc(MAX_THREAD_STACK_SIZE);
}

thread_t *dead_threads = NULL;

/**
 * Освобождает динамическую память буферов и стека потока
 *
 * @param thread - указатель на структуру потока
 */
void free_thread(thread_t *thread)
{
    free(thread->protected);
    free(thread->tagbody_buffers);
    free(thread->catch_buffers);
    free(thread->stack);
    thread->protected = NULL;
    thread->tagbody_buffers = NULL;
    thread->catch_buffers = NULL;
    thread->stack = NULL;
}

/**
 * Освобождает динамическую память всех завершенных потоков в dead_threads
 * и возвращает их в список свободных потоков free_threads.
 */
void free_dead_threads()
{
    while (dead_threads != NULL)
    {
        thread_t *t = dead_threads;
        dead_threads = dead_threads->next;
        free_thread(t);
        t->next = free_threads;
        t->prev = NULL;
        t->free = 1;
        free_threads = t;
    }
}

/**
 * Сохраняет контекст интерпретатора текущего потока
 *
 * @param from - поток, контекст которого сохраняется
 */
void save_env(thread_t *from)
{
    from->current_env = current_env;
    from->func_env = func_env;
    from->last_protected = last_protected;
    from->tb_index_buf = tb_index_buf;
    from->ct_index_buf = ct_index_buf;
}

/**
 * Восстанавливает контекст интерпретатора целевого потока
 *
 * @param to - поток, контекст которого восстанавливается
 */
void restore_env(thread_t *to)
{
    current_env = to->current_env;
    func_env = to->func_env;
    protected = to->protected;
    last_protected = to->last_protected;
    tb_index_buf = to->tb_index_buf;
    tagbody_buffers = to->tagbody_buffers;
    catch_buffers = to->catch_buffers;
    ct_index_buf = to->ct_index_buf;
}

/**
 * Переключает контекст между двумя потоками.
 * Сохраняет окружение и регистры потока prev, переключает current_thread на next,
 * восстанавливает окружение next и передает управление через restore_regs.
 * При возобновлении потока prev очищает память завершенного потока (если был).
 *
 * @param prev - текущий поток
 * @param next - целевой поток
 */
static void switch_context(thread_t *prev, thread_t *next)
{
    save_env(prev);
    restore_env(next);
    current_thread = next;
    if (save_regs(prev) == 0)
        restore_regs(next);
}

/**
 * Завершает выполнение потока после возврата из eval.
 * Вызывается из kill_current_thread.
 *
 * @param result - результат вычисления выражения функцией eval
 */
void thread_finish(object_t result)
{
    current_thread->result = result;
    thread_kill(new_number(current_thread - threads));
}

/**
 * Получает указатель на поток по его идентификатору
 *
 * @param thread_id - идентификатор потока
 *
 * @return Указатель на поток
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
 * @return Идентификатор потока
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
    thread_t *thread = NULL;

    if (dead_threads != NULL)
    {
        // Переиспользуем готовый мертвый поток (буферы уже выделены)
        thread = dead_threads;
        dead_threads = dead_threads->next;

        // Очищаем все остальные накопившиеся мертвые потоки и пополняем free_threads
        free_dead_threads();
    }
    else if (free_threads != NULL)
    {
        // Первый запуск слота - выделяем динамические буферы
        thread = free_threads;
        free_threads = free_threads->next;
        alloc_thread(thread);
    }
    else
    {
        error("Error: out of memory: threads");
    }

    thread->next = current_thread;
    thread->prev = current_thread->prev;
    current_thread->prev->next = thread;
    current_thread->prev = thread;

    thread->last_protected = 0;
    thread->tb_index_buf = 0;
    thread->ct_index_buf = MAX_CATCH_SIZE;
    thread->current_env = current_env;
    thread->func_env = func_env;

    thread->free = 0;
    thread->function = fun;
    thread->result = 0;

    // Инициализация первого кадра стека и регистров для запуска eval
#ifndef X32
    void *stack_top = (char *)thread->stack + MAX_THREAD_STACK_SIZE;
    void **sp = (void **)((uintptr_t)stack_top & ~15UL);
    *(--sp) = (void *)kill_current_thread; // адрес возврата при ret из eval
    thread->regs[_SP] = (object_t)sp;
    thread->regs[_IP] = (object_t)eval;
    thread->regs[_DI] = fun;         // 1-й аргумент eval (объект-выражение)
    thread->regs[_SI] = current_env; // 2-й аргумент eval (окружение переменных)
    thread->regs[_DX] = 0;
    thread->regs[_AX] = 0;
    thread->regs[_BX] = 0;
    thread->regs[_CX] = 0;
    thread->regs[_BP] = 0;
    thread->regs[_R8] = 0;
    thread->regs[_R9] = 0;
    thread->regs[_R10] = 0;
    thread->regs[_R11] = 0;
    thread->regs[_R12] = 0;
    thread->regs[_R13] = 0;
    thread->regs[_R14] = 0;
    thread->regs[_R15] = 0;
    thread->regs[_FLAGS] = 0x202; // IF = 1
#else
    void **sp = (void **)((char *)thread->stack + MAX_THREAD_STACK_SIZE);
    *(--sp) = (void *)current_env;         // 2-й аргумент eval
    *(--sp) = (void *)fun;                 // 1-й аргумент eval
    *(--sp) = (void *)kill_current_thread; // адрес возврата при ret из eval
    thread->regs[_SP] = (object_t)sp;
    thread->regs[_IP] = (object_t)eval;
    thread->regs[_AX] = 0;
    thread->regs[_BX] = 0;
    thread->regs[_CX] = 0;
    thread->regs[_DX] = 0;
    thread->regs[_SI] = 0;
    thread->regs[_DI] = 0;
    thread->regs[_BP] = 0;
    thread->regs[_FLAGS] = 0x202; // IF = 1
#endif

    total_threads++;

    return new_number(thread - threads);
}

/**
 * Передает управление другому потоку
 */
void thread_yield()
{
    free_dead_threads();

    thread_t *next_thread = current_thread->next;
    if (current_thread == next_thread)
        return; // если в очереди только один поток, то ничего не делаем

    switch_context(current_thread, next_thread);

    free_dead_threads();
}

/**
 * Ожидает завершения одного или нескольких потоков
 *
 * @param thread_id - идентификатор потока
 * @return Результат выполнения (объект или массив объектов результатов)
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

    thread_t *next_thread = thread->next;

    // удаляем поток из списка активных потоков
    thread->prev->next = thread->next;
    thread->next->prev = thread->prev;
    total_threads--;

    // помещаем поток в стек мертвых потоков для переиспользования
    thread->free = 1;
    thread->next = dead_threads;
    thread->prev = NULL;
    dead_threads = thread;

    // переключаемся на следующий поток, если текущий поток был убит
    if (current_thread == thread)
    {
        current_thread = next_thread;
        restore_env(next_thread);
        restore_regs(next_thread);
    }
}

/**
 * Инициализация потоков
 */
void init_threads()
{
    free_dead_threads();
    dead_threads = NULL;
    register_func("THREAD-SPAWN", (func0_t)thread_spawn, 0, 1);
    register_func("THREAD-YIELD", (func0_t)thread_yield, 0, 0);
    register_func("THREAD-WAIT", (func0_t)thread_wait, 0, 1);
    register_func("THREAD-KILL", (func0_t)thread_kill, 0, 1);

    // инициализация главного потока
    current_thread = threads;
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
