#include "portable/mem.h"
#include "portable/console.h"
#include "portable/libc.h"
/**
 * @file   mem.c
 * @author yri066 <yri066@ubuntu>
 * @date   Mon Nov  9 08:25:46 2020
 * 
 * @brief  Выделение памяти malloc
 * 
 * 
 */

void _print_mem();

/** 
 * Структура сегмента памяти
 * freedom - признак своден ли сегмент: 0 - свободен, 1 - занят
 * address - адрес сегмента памяти
 * mem_size - размер занятой памяти
 * @return 
 */
struct memory
{
    unsigned int freedom;
    unsigned int address;
    unsigned int mem_size;
};

///массив сегментов
struct memory mem[MAX_SEGMENTS];

///текущее количество используемых сегментов
int segment_count = 0;

/** 
 * mem_init() инициализирует массив сегментов
 * 
 */
void mem_init()
{
    segment_count = 1;
    
    mem[0].freedom = 0;
    mem[0].address = START_ADDRESS;
    mem[0].mem_size = MEM_SIZE;
    
    for (int i = 1; i < MAX_SEGMENTS; i++) {
        mem[i].freedom = 0;
        mem[i].address = 0;
        mem[i].mem_size = 0;
    }
}

/** 
 * Выделяет необходимый размер памяти 
 * 
 * @param size размер выделяемой памяти 
 */
void *malloc(int size)
{
    
    int temp = -1;//запоминает номер сегмента, если сегмент большего размера, чем необходимо
    
    for(int i = 0; i < segment_count; i++)//поиск первого пустого сегмента
    {
        
        if(mem[i].freedom == 0 && i == MAX_SEGMENTS - 1)//если дошли до последнего сегмента
        {
            if(mem[i].mem_size < size)//если недостаточно памяти
            {
                return 0;
            }
            
            //добавление в последний сегмент
            mem[i].freedom = 1;
            return (int*)mem[i].address;
        }
		
        if(mem[i].freedom == 0)//если сегмент свободен
        {
			
            if(i < segment_count - 1 && mem[i].mem_size < size)//пропускается неподходящий сегмент 
            {
                continue;
            }
            if(i < segment_count - 1 && mem[i].mem_size >= size)//найден подходящий сегмент 
            {
				
                if(mem[i].mem_size > size && mem[i+1].freedom == 0)//найденный сегмент большено размера чем нужен и следующий сегмент свободен
                {
                    mem[i].freedom = 1;
                    mem[i+1].address = mem[i].address + size;//указываем адрес следующей сегмента
                    mem[i+1].mem_size += mem[i].mem_size - size;//указываем оставшийся размер памяти в следующем сегменте
                    mem[i].mem_size = size;
                    return (int*)mem[i].address;
                }
				
                if(mem[i].mem_size > size && mem[i+1].freedom == 1)//найденный сегмент большено размера чем нужен и следующий сегмент занят
                {
                    temp = i;//запоминаем этот сегмент на тот случай, если остальные будут заняты
                    continue;
                }
                
                //сегмет того размера, который требуется (равен size)
                mem[i].freedom = 1;
                return (int*)mem[i].address;
            }
            
            //добавление нового сегмента
            mem[i].freedom = 1;
            mem[i+1].address = mem[i].address + size;
            mem[i+1].mem_size = mem[i].mem_size - size;
            mem[i].mem_size = size;
            segment_count++;
            return (int*)mem[i].address;
        }
    }
    
    if(temp == -1)//если нет ни одного подходящего сегмента
    {
        return 0;
    }
    else//возвращается оставшийся сегмент большего размера
    {
        mem[temp].freedom = 1;
        return (int*)mem[temp].address;
    }
}

/** 
 * Освобождает область памяти 
 * 
 * @param addr адрес освобождаемой памяти 
 */
void free(void *addr)
{
    
    for(int i = 0; i < segment_count; i++)//поиск сегмента по адресу
    {
		
        if((int*)mem[i].address == addr)
        {
            
            mem[i].freedom = 0;
            
            
            if(i != 0 && i != MAX_SEGMENTS - 1)//проверяет на пустоту крайние ячейки
            {
                
                if((int*)mem[1+i].freedom == 0)//соединяет текущий и следующий сегмент
                {
                    mem[i].mem_size += mem[i+1].mem_size;
                    mem[i+1].mem_size = 0;
                    if(i + 1 == segment_count - 1 && segment_count > 1)//если удаляется последний используемый сегмент, уменьщается кол-во сегментов
                    {
                        segment_count--;
                    }
                }
                
                if(i == segment_count - 1 && segment_count > 1)//если удаляется последний используемый сегмент, уменьщается кол-во сегментов
                {
                    segment_count--;
                }
                
                int temp = i;
                
                while(temp > 0 && (int*)mem[temp-1].freedom == 0)//соединяет текущий сегмент c предыдущими сегментами
                {
                    
                    if(temp == segment_count - 1 && segment_count > 1)//если удаляется последний используемый сегмент, уменьщается кол-во сегментов
                    {
                        segment_count--;
                    }
                    mem[temp-1].mem_size += mem[temp].mem_size;
                    mem[temp].mem_size = 0;
                    temp--;
                }
            }
			
            if(i == 0 && (int*)mem[i+1].freedom == 0)//проверяет на пустоту следующую ячейку
            {
                if(i + 1 == segment_count - 1 && segment_count > 1)//если удаляется последний используемый сегмент, уменьщается кол-во сегментов
                {
                    segment_count--;
                }
                
                mem[i].mem_size += mem[i+1].mem_size;
                mem[i+1].mem_size = 0;
                
            }
			
            if(i == MAX_SEGMENTS - 1 && (int*)mem[i-1].freedom == 0)//проверяет на пустоту предыдущую ячейку
            {
                
                if(i == segment_count - 1 && segment_count > 1)//если удаляется последний используемый сегмент, уменьщается кол-во сегментов
                {
                    segment_count--;
                }
                mem[i-1].mem_size += mem[i].mem_size;
                mem[i].mem_size = 0;
                
                int temp = i;
                
                while(temp > 0 && (int*)mem[temp-1].freedom == 0)//соединяет последний сегмент c предыдущими сегментами
                {
                    
                    if(temp == segment_count - 1 && segment_count > 1)//если удаляется последний используемый сегмент, уменьщается кол-во сегментов
                    {
                        segment_count--;
                    }
                    mem[temp-1].mem_size += mem[temp].mem_size;
                    mem[temp].mem_size = 0;
                    temp--;
                }
                
            }
            break;
        }
    }
}

/** 
 * Тестирование памяти 
 * 
 * @param num_test номер теста 
 */
void test_mem(int num_test)
{
    switch(num_test)
    {
        case 1:{
	  kprint("1 block\n");
	  char* a = malloc(100);
            _print_mem();
	    kprint("free\n");
	    free(a);
            _print_mem();
	  kprint("2 block\n");
	  char* a1 = malloc(100);
	  char* a2 = malloc(200);
            _print_mem();
	    kprint("free\n");
	    free(a);
            _print_mem();
            break;
        }
        case 2:{
            //Инициализировать 5 переменных
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            int*  c2 = (int *)malloc(sizeof(int));
            char* c3 = (char *)malloc(5 * sizeof(char)); 
            int*  c4 = (int *)malloc(sizeof(int));
            char* c5 = (char *)malloc(10 * sizeof(char));
            _print_mem();
            break;
        }
        case 3:{
            //Инициализировать 5 переменных, удалить переменную типа char
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            int*  c2 = (int *)malloc(sizeof(int));
            char* c3 = (char *)malloc(5 * sizeof(char)); 
            int*  c4 = (int *)malloc(sizeof(int));
            char* c5 = (char *)malloc(10 * sizeof(char));
            free(c3);
            _print_mem();
            break;
        }
        case 4:{
            //Инициализировать 5 переменных, удалить переменную типа int
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            int*  c2 = (int *)malloc(sizeof(int));
            char* c3 = (char *)malloc(5 * sizeof(char)); 
            int*  c4 = (int *)malloc(sizeof(int));
            char* c5 = (char *)malloc(10 * sizeof(char));
            free(c2);
            _print_mem();
            break;
        }
        case 5:{
            //Инициализировать 5 переменных, удалить переменную типа char и инициализировать переменную меньшнго размера
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            int*  c2 = (int *)malloc(sizeof(int));
            char* c3 = (char *)malloc(5 * sizeof(char)); 
            int*  c4 = (int *)malloc(sizeof(int));
            char* c5 = (char *)malloc(10 * sizeof(char));
            free(c3);
            int*  c6 = (int *)malloc(sizeof(int));
            _print_mem();
            break;
        }
        case 6:{
            //Инициализировать 5 переменных, удалить переменную типа int и инициализировать переменную большего размера
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            int*  c2 = (int *)malloc(sizeof(int));
            char* c3 = (char *)malloc(5 * sizeof(char)); 
            int*  c4 = (int *)malloc(sizeof(int));
            char* c5 = (char *)malloc(10 * sizeof(char));
            free(c2);
            char* c6 = (char *)malloc(5 * sizeof(char));
            _print_mem();
            break;
        }
        case 7:{
            //Инициализировать 5 переменных, удалить переменную типа char и инициализировать переменную идентичного размера
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            int*  c2 = (int *)malloc(sizeof(int));
            char* c3 = (char *)malloc(5 * sizeof(char)); 
            int*  c4 = (int *)malloc(sizeof(int));
            char* c5 = (char *)malloc(10 * sizeof(char));
            free(c3);
            char* c6 = (char *)malloc(5 * sizeof(char));
            _print_mem();
            break;
        }
        case 8:{
            //Инициализировать 5 переменных и удалить их в порядке создания
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            int*  c2 = (int *)malloc(sizeof(int));
            char* c3 = (char *)malloc(5 * sizeof(char)); 
            int*  c4 = (int *)malloc(sizeof(int));
            char* c5 = (char *)malloc(10 * sizeof(char));
            free(c1);
            free(c2);
	    //            free(c3);
	    //            free(c4);
	    //            free(c5);
            _print_mem();
            break;
        }
        case 9:{
            //Инициализировать 5 переменных и удалить их в обратном порядке
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            int*  c2 = (int *)malloc(sizeof(int));
            char* c3 = (char *)malloc(5 * sizeof(char)); 
            int*  c4 = (int *)malloc(sizeof(int));
            char* c5 = (char *)malloc(10 * sizeof(char));
            free(c5);
            free(c4);
            free(c3);
            free(c2);
            free(c1);
            _print_mem();
            break;
        }
        case 10:{
            //Заполнить все сегменты
            for(int i = 0; i < MAX_SEGMENTS; i++)
            {
                char* c = (char *)malloc(10 * sizeof(char));
            }
            _print_mem();
            break;
        }
        case 11:{
            //Заполнить все сегменты кроме последнего
            for(int i = 0; i < MAX_SEGMENTS - 1; i++)
            {
                char* c = (char *)malloc(1551 * sizeof(char));
            }
            _print_mem();
            break;
        }
        case 12:{
            //Заполнить все сегменты
            //очистить сегмент в начале и заполнить переменной меньшего размера
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            for(int i = 0; i < MAX_SEGMENTS-1 ; i++)
            {
                char* c = (char *)malloc(1551 * sizeof(char));
            }
            free(c1);
            int*  c2 = (int *)malloc(sizeof(int));
            _print_mem();
            break;
        }
        case 13:{
            //Заполнить все сегменты
            //очистить сегмент в начале и заполнить переменной большего размера
            char* c1 = (char *)malloc(10 * sizeof(char)); 
            for(int i = 0; i < MAX_SEGMENTS-1; i++)
            {
                char* c = (char *)malloc(1551 * sizeof(char));
            }
            free(c1);
            char* c2 = (char *)malloc(11 * sizeof(char));
            _print_mem();
            break;
        }
        case 14:{
            char* c1 = (char *)malloc(10 * sizeof(char));
            char* c2 = (char *)malloc(10 * sizeof(char));
            for(int i = 0; i < MAX_SEGMENTS - 2; i++)
            {
                char* c = (char *)malloc(1551 * sizeof(char));
            }
            free(c1);
            free(c2);
            char* c3 = (char *)malloc(15 * sizeof(char));
            char* c4 = (char *)malloc(2 * sizeof(char));
            _print_mem();
            break;
        }
        case 15:{
            char* c1 = (char *)malloc(10 * sizeof(char));
            char* c2 = (char *)malloc(10 * sizeof(char));
            for(int i = 0; i < MAX_SEGMENTS - 2; i++)
            {
                char* c = (char *)malloc(1551 * sizeof(char));
            }
            free(c1);
            free(c2);
            char* c3 = (char *)malloc(15 * sizeof(char));
            char* c4 = (char *)malloc(10 * sizeof(char));
            _print_mem();
            break;
        }
        case 16:{
            /*for(int i = 0; i < MAX_SEGMENTS - 4; i++)
            {
                char* c = (char *)malloc(1551 * sizeof(char));
            }
            char* c1 = (char *)malloc(10 * sizeof(char));
            char* c2 = (char *)malloc(10 * sizeof(char));
            char* c3 = (char *)malloc(15 * sizeof(char));
            char* c4 = (char *)malloc(10 * sizeof(char));
            free(c1);
            free(c2);
            free(c3);
            free(c4);*/
            int students[1024];
            for(int i = 0; i < 1024; i++)//Заполнить все сегменты памяти
            {
                students[i] = (int)malloc(sizeof(int));
            }
            _print_mem();
            kprint("free\n");
            for(int i = 3; i < 1024; i++)//Освободить все сегменты памяти кроме первых 3
            {
                free((int *)students[i]);
            }
            _print_mem();
            kprint("add 2 blocks\n");
            char* a1 = malloc(100);
            char* a2 = malloc(200);
            _print_mem();
            break;
        }
        default:
            kprint("Test not found");
            break;
        
    }
    
    mem_init();
}

/** 
 * Выводит занятую память в консоль
 * 
 * 
 */
void _print_mem()
{
                    
    for(int i = 0; i<segment_count; i++)
    {
        if((int*)mem[i].freedom == 0)
        {
            kprint("0");
        }
        else
        {
            kprint("1");
        }
    }
    kprint("\n");
}
