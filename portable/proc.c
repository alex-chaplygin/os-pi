#include <portable/proc.h>
#include <portable/limits.h>

struct proc processes[MAX_PROC_AMOUNT];
struct proc *current_proc;

void initProcesses(){
    for (int i = 0; i < MAX_PROC_AMOUNT; i++)
    {
        processes[i].pid = -1;
        processes[i].state = STATUS_READY;
        processes[i].codePtr = 0;
        processes[i].dataPtr = 0;
        processes[i].stackPtr = 0;
	processes[i].regs[BUFFER_SIZE] = 0;
    } 
}

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

// возвращает pid
int createProc(unsigned char* codePtr, unsigned char* dataPtr, unsigned char* stackPtr){
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
    processes[freeSlot].stackPtr = stackPtr;
    
}

int deleteProc(unsigned int pid){
    if(pid < 0 || pid > MAX_PROC_AMOUNT){
        return -1;
    }

    processes[pid].pid = -1;
    return 0;
}

/** 
 * Планировщик задач.
 * Переключает контекст на новый процесс при необходимости.
 */
void sheduler()
{
  for(int i = 0; i < MAX_PROC_AMOUNT; i++)
    {
      if(processes[i].state == STATUS_READY)
	if(i != 0)
	  {
	    current_proc = &processes[i];
	    current_proc->state = STATUS_RUNNING;
	    processes[i - 1].state = STATUS_READY;
	    break;
	  }
	else
	  {
	    current_proc = &processes[i];
	    current_proc->state = STATUS_RUNNING;
	    break;
	  }
    }
}

