#include <portable/mem.h>
/**
 * @file   mem.c
 * @author yri066 <yri066@ubuntu>
 * @date   Mon Nov  9 08:25:46 2020
 * 
 * @brief  Выделение памяти malloc
 * 
 * 
 */

/** 
 * Структура сегмента памяти
 * freedom - признак сводобне ли сегмент: 0 - свободен, 1 - занят
 * address - адрес сегмента памяти
 * mem_size - размер занятой памяти
 * @return 
 */
struct memory
{
    int freedom;
    int address;
    int mem_size;
};

///массив сегментов
struct memory mem[MAX_SEGMENTS];

/** 
 * mem_init() инициализирует массив сегментов
 * 
 */
void mem_init()
{
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
    for(int i = 0; i < MAX_SEGMENTS; i++)//поиск первого пустого сегмента
    {
        if(mem[i].freedom == 0 && i == MAX_SEGMENTS - 1)//если дошли до последнго сегмента ... ?
        {
	    if(mem[i].mem_size < size)//если недостаточно памяти
            {
	        return 0;
            }
	    //добавление в сегмент
            mem[i].freedom = 1;
            mem[i].mem_size = size;
	    return (int*)mem[i].address;
        }
        if(mem[i].freedom == 0)
        {
            if(mem[i].mem_size < size)//если недостаточно памяти
            {
	        return 0;
            }
            //добавление в сегмент
            mem[i].freedom = 1;
            mem[i+1].address = mem[i].address + size;
            mem[i+1].mem_size = mem[i].mem_size - size;
            mem[i].mem_size = size;
            return (int*)mem[i].address;
        }
    }
    
    return 0;
}
