#include <portable/types.h>

#define STATUS_READY 0		/**< состояние "процесс готов" */
#define STATUS_RUNNING 1	/**< состояние "процесс запущен" */
#define STATUS_SLEEPING 2	/**< состояние "процесс заморожен" */
#define STATUS_STOPPING 3	/**< состояние "процесс остановлен" */
#define BUFFER_SIZE 64		/**< размер буфера регистров процесса */
#define STACK_SIZE 1024		/**< размер стека в байтах */

/// структура процесса (если меняется, то нужно менять в x86.asm)
struct proc {
  int pid;			/**< номер процесса */
  int parent_id;		/**<  номер родительского процесса*/
  int state;			/**< состояние */
  void* codePtr;	        /**< адрес кода */
  int code_size;		/**< размер сегмента кода */
  void* dataPtr;	        /**< адрес данных */
  int data_size;		/**< размер сегмента данных */
  void* stackPtr;	        /**< адрес стека */
  void *program_counter;		/**< счетчик команд */
  void *stack_pointer;		/**< указатель стека */
  int regs[BUFFER_SIZE];	/**< буфер регистров */
};

// инициализирует хранилище процессов
void initProcesses();

//проверяет, свободен ли указанный pid
int isFree(unsigned int pid);

//удаляет процесс, освобождает слот
int deleteProc(unsigned int pid);

// создаёт процесс
// возвращает pid созданного процесса
int createProc(void* codePtr, int code_size, void* dataPtr, int data_size);

void sheduler();

