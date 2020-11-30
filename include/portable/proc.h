#include <portable/types.h>

#define STATUS_READY 0		/**< состояние "процесс готов" */
#define STATUS_RUNNING 1	/**< состояние "процесс запущен" */
#define STATUS_SLEEPING 2	/**< состояние "процесс заморожен" */
#define STATUS_STOPPING 3	/**< состояние "процесс остановлен" */
#define BUFFER_SIZE 64		/**< размер буфера регистров процесса */

/** 
 * структура процесса
 * pid - айди процесса, state - состояние, codePtr - адрес кода, dataPtr - адрес данных, stackPtr - адрес стека
 */
struct proc {
    int pid;
    int state;
    unsigned char*  codePtr;
    unsigned char*  dataPtr;
    unsigned char*  stackPtr;
    byte regs[BUFFER_SIZE];
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

