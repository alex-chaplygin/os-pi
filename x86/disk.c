#include <x86/disk.h>
#include <portable/types.h>

/** 
 * Возвращает данные с тестового жесткого диска
 * @param buffer - буфер (размер 512), в который будут записаны данные
 * @param block_num - номер блока диска, с которого будут считываться данные
 */
void disk_read_block(byte *buffer, int block_num)
{
    byte super_blok[] = {0x71,0x71,0x04,0x00,0x00,0x01,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
    byte first_blok[] = {0x66,0x69,0x6C,0x65,0x2E,0x74,0x78,0x74,0x00,0x00,0x00,0x00,0x00,0x0B,0x00,0x01};
    byte eleventh_blok[] = {0x54,0x65,0x6D,0x70,0x6F,0x72,0x61,0x72,0x79,0x20,0x63,0x6F,0x6E,0x74,0x65,0x6E,0x74,0x20,0x6F,0x66,0x20,0x66,0x69,0x6C,0x65,0x2E,0x74,0x78,0x74,0x20,0x66,0x69,0x6C,0x65};
    
    byte bufData[BLOCK_COUNT][BLOCK_SIZE];

    //Инициализация диска
    for(int i =0; i < BLOCK_COUNT; i++)
    {
        for(int j = 0; j<BLOCK_SIZE;j++)
        {
            bufData[i][j] = 0x00;
        }
    }

    //Заполнение суперблока
    for(int i =0; i < 512; i++)
    {
        if(super_blok[i] == '\0')
        {
            break;
        }
        bufData[0][i] = super_blok[i];
    }
    
    //Заполнение 1 блока
    for(int i =0; i < 512; i++)
    {
        if(first_blok[i] == '\0')
        {
            break;
        }
        bufData[1][i] = first_blok[i];
    }

    //Заполнение 11 блока
    for(int i =0; i < 512; i++)
    {
        if(eleventh_blok[i] == '\0')
        {
            break;
        }
        bufData[11][i] = eleventh_blok[i];
    }


    if(block_num == 0)//суперблок
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[0][i];
        }

        return;
    }
    if(block_num == 1)//каталок
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[1][i];
        }
        
        return;
    }
    if(block_num == 11)//содержимое файлаs
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[11][i];
        }
        
        return;
    }

    for(int i =0; i < BLOCK_SIZE; i++)
    {
        buffer[i] = 0;
    }
    
    return;
}