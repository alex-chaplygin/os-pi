#include <portable/proc.h>
#include <portable/limits.h>
#include <portable/console.h>
#include <x86/x86.h>

struct proc processes[MAX_PROC_AMOUNT];	/**< Массив процессов. */
struct proc *current_proc = 0;	/**< Указатель на текущий процесс. */
int current_proc_numb = 0;	/**< Номер текущего процесса */

void printProc1()
{
  while(1)
    kprint("Process 1\n");
}

void printProc2()
{
  while(1)
    kprint("Process 2\n");
}

/** 
 * @brief Инициализация процессов. Устанавливает процессам состояние STATUS_READY.
 * 
 */
void initProcesses(){
    for (int i = 0; i < MAX_PROC_AMOUNT; i++)
    {
        processes[i].pid = -1;
        processes[i].state = STATUS_READY;
        processes[i].codePtr = 0;
        processes[i].dataPtr = 0;

	for(int j = 0; j < BUFFER_SIZE; j++)
	  processes[i].regs[j] = 0;
    }

    void (*message1) () = &printProc1;
    void (*message2) () = &printProc2;

    int pid1 = createProc(message1, 0);
    int pid2 = createProc(message2, 0);
}

/** 
 * @brief Функция проверки свободен ли айди процесса.
 * 
 * @param pid Проверяемый айди процесса.
 * 
 * @return -1 если проверяемый айди неверный, 1 если проверяемый айди свободен, 0 если занят.
 */
int isFree(unsigned int pid){
    if (pid < 0 || pid > MAX_PROC_AMOUNT){
        return -1;
    }

    if (processes[pid].pid == -1){
        return 1;
    } else {
        return 0;
    }
}

/** 
 * @brief Функция создния процесса.
 * 
 * @param codePtr адрес кода.
 * @param dataPtr адрес данных.
 * @param stackPtr адрес стека.
 * 
 * @return возвращает айди созданного процесса.
 */
int createProc(void (*codePtr)(), unsigned char* dataPtr){
    int freeSlot = -1;
    
    for (int i = 0; i < MAX_PROC_AMOUNT; i++)
    {
        if (isFree(i)){
            freeSlot = i;
            break;
        }
    }

    if(freeSlot == -1)
        return -1;


    processes[freeSlot].pid = freeSlot;
    processes[freeSlot].state = STATUS_READY;
    processes[freeSlot].codePtr = codePtr;
    processes[freeSlot].dataPtr = dataPtr;
    processes[freeSlot].stackPtr = (unsigned char*)malloc(1024 * sizeof(unsigned char));
}

/** 
 * @brief Функция удаления существующего процесса.
 * 
 * @param pid айди процесса.
 * 
 * @return -1 если pid неверный, 0 если pid освобожден.
 */
int deleteProc(unsigned int pid){
    if(pid < 0 || pid > MAX_PROC_AMOUNT){
        return -1;
    }

    processes[pid].pid = -1;
    return 0;
}

/** 
 * @brief Планировщик задач. Переключает контекст на новый процесс при необходимости.
 * 
 */
void sheduler()
{
  for(int i = current_proc_numb; i < MAX_PROC_AMOUNT; i++)
    {
      if(processes[i].state == STATUS_READY)
	if(current_proc != 0)
	  {
	    if(processes[i].pid != -1)
	      {
		current_proc->state = STATUS_READY;
		current_proc = &processes[i];
		current_proc->state = STATUS_RUNNING;
		restore_regs(current_proc->regs);
		current_proc_numb = i;
		break;
	      }
	  }
	else
	  {
	    if(processes[i].pid != -1)
	      {
		current_proc = &processes[i];
		current_proc->state = STATUS_RUNNING;
		restore_regs(current_proc->regs);
		break;
	      }
	  }
      if(i == MAX_PROC_AMOUNT - 1)
	{
	  current_proc_numb = 0;
	  i = current_proc_numb - 1;
	}
    }
}

