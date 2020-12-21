#include <portable/proc.h>
#include <portable/limits.h>
#include <x86/console.h>
#include <x86/x86.h>

struct proc processes[MAX_PROC_AMOUNT];	/**< Массив процессов. */
struct proc *current_proc = 0;	/**< Указатель на текущий процесс. */
int current_proc_numb = 0;	/**< Номер текущего процесса */

void printProc1()
{
  ushort *video = (ushort *)0xb8000;
  int i = 0;
  int j = 0;
  while(1) {
    i++;
    j++;
    if (i > 5) {
      *video = 0xffaa;
      i = 0;
    }
    else *video = 0;
    //if (j % 100000 == 0)
      //    if (test_syscall(0, "1 ", 6) < 0) *video = 0x1111;
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
        processes[i].parent_id = -1;
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
    int pid1 = createProc(printProc1, 1024, 0, 0);
    int pid2 = createProc(printProc2, 1024, 0, 0);
    //processes[1].state = STATUS_SLEEPING;   
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
 * @return возвращает номер созданного процесса или -1, если нет места для процесса
 */
int createProc(void* codePtr, int code_size, void* dataPtr, int data_size){
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
    processes[freeSlot].code_size = code_size;
    processes[freeSlot].data_size = data_size;
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
  // создание нового элемента в таблице процессов
  // установка номера родительского процесса
  // копирование памяти для кода и данных
  // сохранить значение -1 в регистр eax дочернего процесса regs[REGS_SIZE - 1]
  return 0; // возврат номера дочернего процесса или ERROR_MAXPROC
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
  // чтение исполняемого файла
  // разбор файла ELF
  // создание сегмента кода и сегмента данных
  // заполнение сегмента BSS нулями.
  // установка указателя стека
  // установка счетчика команд (CS:IP)
  // установка флагов
  // установить eax 0 - успешный запуск процесса
  // восстановление регистров
  return 0; // 0 - успешный запуск, ERROR_NOFILE - файл не найден, ERROR_INVALID_PARAMETERS если name == 0
}

/** 
 * Завершает текущий процесс
 * 
 * @param code код возврата
 */
int exit(int code)
{
  return 0;
}

/** 
 * Ждет завершения процесса
 * 
 * @param id идентификатор процесса
 */
int wait(int id)
{
  return 0;
}

/** 
 * @brief Планировщик задач. Переключает контекст на новый процесс при необходимости.
 * 
 */
void sheduler()
{
  current_proc->state=STATUS_READY;
  
  for (current_proc++; current_proc->state != STATUS_READY;  current_proc++) ;

  current_proc->state=STATUS_RUNNING;
  
  while (current_proc->pid == -1) {
    if (current_proc >= processes + MAX_PROC_AMOUNT - 1) {
      current_proc = processes;
      break;
    }
    current_proc++;
  }
  restore_regs();
}

