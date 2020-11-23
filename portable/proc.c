#include <portable/proc.h>
#include <portable/limits.h>


struct proc processes[MAX_PROC_AMOUNT];

void initProcesses(){
    for (int i = 0; i < MAX_PROC_AMOUNT; i++)
    {
        processes[i].pid = -1;
        processes[i].state = STATUS_INIT;
        processes[i].codePtr = 0;
        processes[i].dataPtr = 0;
        processes[i].stackPtr = 0;
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
    processes[freeSlot].state = STATUS_RUNNING;
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
}

