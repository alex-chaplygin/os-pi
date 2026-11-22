#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <setjmp.h>
#include "test.h"
#include "alloc.h"
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "../threads.h"

jmp_buf jmp_env;

// Количество используемых потоков
extern int total_threads;
// Хранилище потоков
extern thread_t threads[MAX_THREADS];
// Список свободных потоков
extern thread_t* free_threads;
// Стек завершенных (мертвых) потоков для переиспользования
extern thread_t* dead_threads;
// Активный поток (по умолчанию главный поток)
extern thread_t* current_thread;
// Освобождение памяти для потока
extern void free_thread(thread_t* thread);
// Очистка пула мертвых потоков
extern void free_dead_threads();

void error(char* str, ...)
{
    longjmp(jmp_env, 1);
}

// Создает Lisp-вызов (THREAD-YIELD)
static object_t make_yield_expr()
{
    return new_pair(NEW_SYMBOL("THREAD-YIELD"), NULLOBJ);
}

static object_t make_progn_yield_expr(int val)
{
    object_t yield_call = make_yield_expr();
    return new_pair(NEW_SYMBOL("PROGN"),
                    new_pair(yield_call,
                             new_pair(new_number(val), NULLOBJ)));
}

// Создает Lisp-вызов (THREAD-KILL <thread_id>)
static object_t make_kill_expr(int id)
{
    return new_pair(NEW_SYMBOL("THREAD-KILL"),
                    new_pair(new_number(id), NULLOBJ));
}

void reset_threads()
{
    for (int i = 0; i < MAX_THREADS; i++)
    {
        if (threads[i].protected != NULL)
        {
            free_thread(&threads[i]);
            threads[i].protected = NULL;
        }
    }
    dead_threads = NULL;
    current_thread = NULL;
    free_threads = NULL;
    total_threads = 0;
}

/**
 * Тест инициализации потоков
 * Проверка, что главный поток создан и активен
 */
void test_init_threads()
{
    init_threads();
    printf("test_init_threads: ");
    ASSERT(current_thread, threads);
    ASSERT(current_thread->free, 0);
    ASSERT(free_threads, &threads[1]);
    ASSERT(threads[MAX_THREADS - 1].next, NULL);
    ASSERT(total_threads, 1);

    ASSERT(current_thread->next, current_thread);
    ASSERT(current_thread->prev, current_thread);

    reset_threads();
}

/**
 * Тест создания FIFO очереди потоков с настоящим eval
 *
 * При thread_spawn новые потоки добавляются в конец активной очереди (FIFO).
 * Проверяется порядок потоков и связей в кольцевом списке.
 */
void test_spawn_order()
{
    init_threads();
    printf("test_spawn_threads: ");
    object_t id1 = thread_spawn(make_yield_expr());
    object_t id2 = thread_spawn(make_yield_expr());
    object_t id3 = thread_spawn(make_yield_expr());

    ASSERT(get_value(id1), 1);
    ASSERT(get_value(id2), 2);
    ASSERT(get_value(id3), 3);

    thread_t* thread1 = get_thread(id1);
    thread_t* thread2 = get_thread(id2);
    thread_t* thread3 = get_thread(id3);

    ASSERT(current_thread->next, thread1);
    ASSERT(thread1->next, thread2);
    ASSERT(thread2->next, thread3);
    ASSERT(thread3->next, current_thread);

    ASSERT(current_thread->prev, thread3);
    ASSERT(thread1->prev, current_thread);
    ASSERT(thread2->prev, thread1);
    ASSERT(thread3->prev, thread2);

    ASSERT(total_threads, 4);
    ASSERT(free_threads, &threads[4]);

    reset_threads();
}

/**
 * Тест создания потоков при нехватке памяти
 */
void test_spawn_out_of_memory()
{
    init_threads();
    printf("test_spawn_out_of_memory: ");
    object_t expr = make_yield_expr();
    for (int i = 0; i < MAX_THREADS - 1; i++)
    {
        thread_spawn(expr);
    }
    ASSERT(total_threads, MAX_THREADS);
    // Попытка создать еще один поток должна вызвать ошибку
    if (setjmp(jmp_env) == 0)
    {
        thread_spawn(expr);
        FAIL;
    }

    reset_threads();
}

/**
 * Тест переключения и вычисления (PROGN (THREAD-YIELD) <val>) через настоящий eval
 */
void test_yield_order()
{
    init_threads();
    printf("test_yield_order: ");

    // Создаем два потока с Lisp-формами (PROGN (THREAD-YIELD) 10) и (PROGN (THREAD-YIELD) 20)
    object_t id1 = thread_spawn(make_progn_yield_expr(10));
    object_t id2 = thread_spawn(make_progn_yield_expr(20));

    thread_t* t1 = get_thread(id1);
    thread_t* t2 = get_thread(id2);

    ASSERT(t1->free, 0);
    ASSERT(t2->free, 0);
    ASSERT(total_threads, 3);

    // 1-й yield главного потока: поток 1 начинает вычисление, делает THREAD-YIELD и передает управление потоку 2.
    // Поток 2 делает THREAD-YIELD и возвращает управление главному потоку.
    thread_yield();

    ASSERT(t1->free, 0);
    ASSERT(t2->free, 0);

    // 2-й yield главного потока: поток 1 продолжает вычисление PROGN, вычисляет 10 и завершается (ret).
    // Поток 2 продолжает вычисление PROGN, вычисляет 20 и завершается (ret).
    // Управление возвращается главному потоку.
    thread_yield();

    ASSERT(t1->free, 1);
    ASSERT(t2->free, 1);
    ASSERT(get_value(t1->result), 10);
    ASSERT(get_value(t2->result), 20);
    ASSERT(total_threads, 1);
    ASSERT(current_thread, threads);

    reset_threads();
}

/**
 * Тест принудительного уничтожения фонового потока с помощью thread_kill
 */
void test_thread_kill()
{
    init_threads();
    printf("test_thread_kill: ");
    object_t id1 = thread_spawn(make_yield_expr());
    object_t id2 = thread_spawn(make_yield_expr());

    thread_t* thread1 = get_thread(id1);
    thread_t* thread2 = get_thread(id2);

    thread_kill(id1);
    ASSERT(total_threads, 2);
    ASSERT(thread1->free, 1);
    ASSERT(dead_threads, thread1);

    ASSERT(current_thread->next, thread2);
    ASSERT(thread2->prev, current_thread);
    ASSERT(thread2->next, current_thread);
    ASSERT(current_thread->prev, thread2);

    // Вновь создаем поток и проверяем, что он переиспользует слот из dead_threads
    object_t id1_again = thread_spawn(make_yield_expr());
    ASSERT(get_value(id1_again), 1);
    ASSERT(get_thread(id1_again)->free, 0);
    ASSERT(dead_threads, NULL);

    // Попытка убить главный поток должна вызвать ошибку
    if (setjmp(jmp_env) == 0)
    {
        thread_kill(new_number(0));
        FAIL;
    }

    reset_threads();
}

/**
 * Тест самоуничтожения текущего потока через вычисление Lisp-формы (THREAD-KILL 2)
 */
void test_thread_kill_current()
{
    init_threads();
    printf("test_thread_kill_current: ");
    object_t id1 = thread_spawn(make_yield_expr());
    // Поток 2 исполняет (THREAD-KILL 2) через настоящий eval
    object_t id2 = thread_spawn(make_kill_expr(2));

    // Передаем управление в очередь: поток 1 делает (THREAD-YIELD) и переключается на поток 2.
    // Поток 2 выполняет самоуничтожение (THREAD-KILL 2) и переключается обратно на главный поток.
    thread_yield();

    ASSERT(get_thread(id2)->free, 1);
    ASSERT(total_threads, 2);

    thread_t* thread1 = get_thread(id1);
    ASSERT(current_thread->next, thread1);
    ASSERT(thread1->next, current_thread);

    reset_threads();
}

/**
 * Тест вычисления выражения через настоящий eval, возврата по ret и перехвата результата в kill_current_thread
 */
void test_eval_and_finish()
{
    init_threads();
    printf("test_eval_and_finish: ");
    // Передаем число 42 как выражение для настоящего eval
    object_t id = thread_spawn(new_number(42));
    thread_t* thread = get_thread(id);

    // До передачи управления созданный поток находится в очереди
    ASSERT(thread->free, 0);
    ASSERT(total_threads, 2);

    // Главный поток уступает управление: созданный поток вычисляет 42,
    // завершается через ret в kill_current_thread и сохраняет результат.
    thread_yield();

    ASSERT(thread->free, 1);
    ASSERT(get_value(thread->result), 42);
    ASSERT(total_threads, 1);

    reset_threads();
}

/**
 * Тест очистки мертвых потоков при thread_yield и переиспользования одного потока в thread_spawn
 */
void test_dead_threads_cleanup_and_reuse()
{
    init_threads();
    printf("test_dead_threads_cleanup_and_reuse: ");

    object_t id = thread_spawn(new_number(42));
    thread_t *thread = get_thread(id);

    // Запускаем поток и ждем его завершения
    thread_yield();

    // При выходе из thread_yield завершившийся поток физически очищен и возвращен в free_threads
    ASSERT(thread->free, 1);
    ASSERT(dead_threads, NULL);
    ASSERT(free_threads, thread);
    ASSERT(thread->stack, NULL);

    // Проверяем частичное переиспользование: убиваем два потока без yield
    object_t id1 = thread_spawn(make_yield_expr());
    object_t id2 = thread_spawn(make_yield_expr());
    thread_t *t1 = get_thread(id1);
    thread_t *t2 = get_thread(id2);
    void *saved_stack2 = t2->stack;

    thread_kill(id1);
    thread_kill(id2);
    // В dead_threads находятся t2 -> t1
    ASSERT(dead_threads, t2);
    ASSERT(t2->next, t1);

    // thread_spawn должен забрать t2 (с сохранением стека) и очистить t1 в free_threads
    object_t new_id = thread_spawn(make_yield_expr());
    thread_t *reused = get_thread(new_id);
    ASSERT(reused, t2);
    ASSERT(reused->stack, saved_stack2);
    ASSERT(t1->stack, NULL);
    ASSERT(free_threads, t1);
    ASSERT(dead_threads, NULL);

    reset_threads();
}

int main()
{
    printf("--------------test threads (with real eval)---------------------\n");
    init_regions();
    init_objects();
    init_eval();

    test_init_threads();
    test_spawn_order();
    test_spawn_out_of_memory();
    test_yield_order();
    test_thread_kill();
    test_thread_kill_current();
    test_eval_and_finish();
    test_dead_threads_cleanup_and_reuse();
    return 0;
}
