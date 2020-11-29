#include <portable/types.h>

#define STATUS_READY 0
#define STATUS_RUNNING 1
#define STATUS_SLEEPING 2
#define STATUS_STOPPING 3
#define BUFFER_SIZE 64

//pid, состояние, адрес кода, адрес данных, адрес стека.
struct proc {
    int pid;
    int state;
    unsigned char*  codePtr;
    unsigned char*  dataPtr;
    unsigned char*  stackPtr;
    byte buffer[BUFFER_SIZE];
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

