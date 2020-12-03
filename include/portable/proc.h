#include <portable/types.h>

#define STATUS_READY 0		/**< состояние "процесс готов" */
#define STATUS_RUNNING 1	/**< состояние "процесс запущен" */
#define STATUS_SLEEPING 2	/**< состояние "процесс заморожен" */
#define STATUS_STOPPING 3	/**< состояние "процесс остановлен" */
#define BUFFER_SIZE 64		/**< размер буфера регистров процесса */

/// структура процесса
struct proc {
    int pid;			/**< айди процесса */
    int state;			/**< состояние */
    unsigned char*  codePtr;	/**< адрес кода */
    unsigned char*  dataPtr;	/**< адрес данных */
    unsigned char*  stackPtr;	/**< адрес стека */
    byte regs[BUFFER_SIZE];	/**< буфер регистров */
};

// инициализирует хранилище процессов
void initProcesses();

//проверяет, свободен ли указанный pid
int isFree(unsigned int pid);

//удаляет процесс, освобождает слот
int deleteProc(unsigned int pid);

// создаёт процесс
// возвращает pid созданного процесса
int createProc(unsigned char* codePtr, unsigned char* dataPtr, unsigned char* stackPtr);

void sheduler();

