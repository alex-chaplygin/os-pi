#include <portable/proc.h>
#include <portable/limits.h>
#include <portable/console.h>
#include <x86/x86.h>

struct proc processes[MAX_PROC_AMOUNT];	/**< Массив процессов. */
struct proc *current_proc = 0;	/**< Указатель на текущий процесс. */
int current_proc_numb = 0;	/**< Номер текущего процесса */

void printProc1()
{
  ushort *video = (ushort *)0xb8000;
  int i = 0;
  while(1) {
    i++;
    if (i > 5) {
      *video = 0xffaa;
      i = 0;
    }
    else *video = 0;
  }
}

void printProc2()
{
  ushort *video = (ushort *)0xb8020;
  int i = 0;
  while(1) {
    i++;
    if (i > 5) {
      *video = 0xaaaa;
      i = 0;
    }
    else *video = 0;
  }
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
	processes[i].stackPtr = 0;

	for(int j = 0; j < BUFFER_SIZE; j++) {
	  processes[i].regs[j] = 0;
	}
    }
    processes[0].pid = 0; // процесс ядра
    processes[0].state = STATUS_RUNNING;
    current_proc = processes;
    current_proc_numb = 1;
    int pid1 = createProc(printProc1, 0);
    int pid2 = createProc(printProc2, 0);
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
 * 
 * @return возвращает айди созданного процесса.
 */
int createProc(void* codePtr, void* dataPtr){
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

    byte *new_stack = (void*)malloc(STACK_SIZE);
    processes[freeSlot].pid = freeSlot;
    processes[freeSlot].codePtr = codePtr;
    processes[freeSlot].dataPtr = dataPtr;
    processes[freeSlot].stackPtr = new_stack;
    processes[freeSlot].program_counter = codePtr;
    processes[freeSlot].stack_pointer = new_stack + STACK_SIZE;
    processes[freeSlot].state = STATUS_READY;
    processes[freeSlot].regs[54] = 0x8; // CS
    processes[freeSlot].regs[55] = 0x200; // EFLAGS
    return freeSlot;
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
 * Создание дочернего процесса, копии родительского
 * 
 * 
 * @return для родительского процесса возвращается идентификатор дочернего процесса, -1 - для дочернего
 */
int fork()
{
  return 0;
}

/** 
 * Заменяет текущий процесс на новый
 * 
 * @param name имя исполняемого файла
 * 
 * @return 0 - успешно, меньше нуля если ошибка
 */
int exec(char *name)
{
  return 0;
}

/** 
 * Завершает текущий процесс
 * 
 * @param code код возврата
 */
void exit(int code)
{
}

/** 
 * Ждет завершения процесса
 * 
 * @param id идентификатор процесса
 */
void wait(int id)
{
}

/** 
 * @brief Планировщик задач. Переключает контекст на новый процесс при необходимости.
 * 
 */
void sheduler()
{
  current_proc++;
  while (current_proc->pid == -1) {
    if (current_proc >= processes + MAX_PROC_AMOUNT - 1) {
      current_proc = processes;
      break;
    }
    current_proc++;
  }
  restore_regs();
  /*  for(int i = current_proc_numb; i < MAX_PROC_AMOUNT; i++)
    {
      if(processes[i].state == STATUS_READY)
	if(current_proc != 0)
	  {
	    if(processes[i].pid != -1)
	      {
		current_proc->state = STATUS_READY;
		current_proc = &processes[i];
		current_proc->state = STATUS_RUNNING;
		current_proc_numb = i;
		restore_regs(); // нет возврата отсюда
		break;
	      }
	  }
	else
	  {
	    if(processes[i].pid != -1)
	      {
		current_proc = &processes[i];
		current_proc->state = STATUS_RUNNING;
		restore_regs(); // нет возврата отсюда
		break;
	      }
	  }
      if(i == MAX_PROC_AMOUNT - 1)
	{
	  current_proc_numb = 0;
	  i = current_proc_numb - 1;
	  }
	  }*/
}

