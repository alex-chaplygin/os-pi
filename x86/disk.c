#include <x86/disk.h>
#include <portable/types.h>

/** 
 * Возвращает данные с тестового жесткого диска
 * @param buffer - буфер (размер 512), в который будут записаны данные
 * @param block_num - номер блока диска, с которого будут считываться данные
 */
void disk_read_block(byte *buffer, int block_num)
{
    byte super_blok[] = {0x71,0x71,0x04,0x00,0x00,0x01,0x02};
    byte first_blok[] = "file.txt";
    byte eleventh_blok[] = "Temporary content of file.txt file";
    
    byte bufData[BUFFER_SIZE][BLOCK_SIZE];

    //Инициализация диска
    for(int i =0; i < BUFFER_SIZE; i++)
    {
        for(int j = 0; j<BLOCK_SIZE;j++)
        {
            bufData[i][j] = 0x0;
        }
    }

    //Заполнение суперблока
    for(int i =0; i < 9; i++)
    {
        bufData[0][i] = super_blok[i];
    }
    
    //Заполнение 1 блока
    for(int i =0; i < 8; i++)
    {
        bufData[1][i] = first_blok[i];
    }

    //Заполнение 11 блока
    for(int i =0; i < 34; i++)
    {
        bufData[11][i] = eleventh_blok[i];
    }


    if(block_num == 0)//суперблок
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[0][i];
        }

        return buffer;
    }
    if(block_num == 1)//каталок
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[1][i];
        }
        
        return buffer;
    }
    if(block_num == 11)//содержимое файлаs
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[11][i];
        }
        
        return buffer;
    }

    for(int i =0; i < BLOCK_SIZE; i++)
    {
        buffer[i] = 0;
    }
    
    return buffer;
}