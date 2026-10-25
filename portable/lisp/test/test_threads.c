#include <stdio.h>
#include <stddef.h>
#include <setjmp.h>
#include "test.h"
#include "alloc.h"
#include "objects.h"
#include "threads.h"

extern thread_t *current_thread;
extern thread_t threads[MAX_THREADS];
extern thread_t *free_threads;
extern int total_threads;

object_t current_env = NULLOBJ;
object_t func_env = NULLOBJ;
jmp_buf jmp_env;

void error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
}

object_t create_simple_function()
{
    object_t xsym = NEW_OBJECT(SYMBOL, new_symbol("X"));
    object_t ysym = NEW_OBJECT(SYMBOL, new_symbol("Y"));
    object_t plussym = NEW_OBJECT(SYMBOL, new_symbol("+"));
    object_t args = new_pair(xsym, new_pair(ysym, NULLOBJ));
    object_t body = new_pair(plussym, new_pair(xsym, new_pair(ysym, NULLOBJ)));
    object_t o = new_function(args, body, NULLOBJ, NULLOBJ);

    return o;
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
    ASSERT(threads[63].next, NULL);
    ASSERT(total_threads, 1);

    ASSERT(current_thread->next, current_thread);
    ASSERT(current_thread->prev, current_thread);
}

/**
 * Тест создания FIFO очереди потоков
 *
 * Проверка порядка создания потоков и их связей в списке
 * Главный поток является активным и связан с первым созданным потоком
 * Количество потоков увеличилось на 3 после создания трех новых потоков
 * Список свободных потоков указывает на следующий свободный поток после последнего созданного потока
 * Предыдущие указатели потоков корректно установлены
 */
void test_spawn_order()
{
    init_threads();
    printf("test_spawn_threads: ");
    object_t id1 = thread_spawn(create_simple_function());
    object_t id2 = thread_spawn(create_simple_function());
    object_t id3 = thread_spawn(create_simple_function());

    ASSERT(get_value(id1), 1);
    ASSERT(get_value(id2), 2);
    ASSERT(get_value(id3), 3);

    thread_t *thread1 = get_thread(id1);
    thread_t *thread2 = get_thread(id2);
    thread_t *thread3 = get_thread(id3);

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
}

/**
 * Тест создания потоков при нехватке памяти
 */
void test_spawn_out_of_memory()
{
    init_threads();
    printf("test_spawn_out_of_memory: ");
    for (int i = 0; i < MAX_THREADS - 1; i++)
    {
        thread_spawn(create_simple_function());
    }
    ASSERT(total_threads, MAX_THREADS);
    // Попытка создать еще один поток должна вызвать ошибку
    if (setjmp(jmp_env) == 0)
    {
        thread_spawn(create_simple_function());
        FAIL;
    }
}

/**
 * Тест переключения между потоками с помощью thread_yield
 */
void test_yield_order()
{
    init_threads();
    printf("test_yield_order: ");
    object_t id1 = thread_spawn(create_simple_function());
    object_t id2 = thread_spawn(create_simple_function());

    thread_t *thread1 = get_thread(id1);
    thread_t *thread2 = get_thread(id2);

    thread_yield();
    ASSERT(current_thread, thread1);

    thread_yield();
    ASSERT(current_thread, thread2);

    thread_yield();
    ASSERT(current_thread, threads);
}

/**
 * Тест уничтожения потока с помощью thread_kill
 * Удаления потока из списка активных потоков и добавляется в список свободных потоков
 * Уменьшение количества потоков на 1 после уничтожения потока
 * Попытка уничтожить главный поток
 */
void test_thread_kill()
{
    init_threads();
    printf("test_thread_kill: ");
    object_t id1 = thread_spawn(create_simple_function());
    object_t id2 = thread_spawn(create_simple_function());

    thread_t *thread1 = get_thread(id1);
    thread_t *thread2 = get_thread(id2);

    thread_kill(id1);
    ASSERT(total_threads, 2);
    ASSERT(thread1->free, 1);
    ASSERT(free_threads, thread1);

    ASSERT(current_thread->next, thread2);
    ASSERT(thread2->prev, current_thread);
    ASSERT(thread2->next, current_thread);
    ASSERT(current_thread->prev, thread2);

    // Вновь создаем поток и проверяем, что он использует слот освобожденного потока
    object_t id1_again = thread_spawn(create_simple_function());
    ASSERT(get_value(id1_again), 1);
    ASSERT(get_thread(id1_again)->free, 0);

    // Попытка убить главный поток должна вызвать ошибку
    if (setjmp(jmp_env) == 0)
    {
        thread_kill(new_number(0));
        FAIL;
    }
}

/**
 * Тест уничтожения текущего потока
 */
void test_thread_kill_current()
{
    init_threads();
    printf("test_thread_kill_current: ");
    object_t id1 = thread_spawn(create_simple_function());
    object_t id2 = thread_spawn(create_simple_function());

    thread_t *thread1 = get_thread(id1);
    thread_t *thread2 = get_thread(id2);

    thread_yield();
    thread_kill(id1);

    ASSERT(current_thread, thread2);
    ASSERT(current_thread->prev, threads);
    ASSERT(current_thread->next, threads);
}

int main()
{
    printf("--------------test threads---------------------\n");
    init_regions();
    init_objects();

    test_init_threads();
    test_spawn_order();
    test_spawn_out_of_memory();
    test_thread_kill();
    test_yield_order();
    test_thread_kill_current();
    return 0;
}
