#include <x86/disk.h>

/** 
 * Возвращает данные с тестового жесткого диска
 * @param buffer - буфер (размер 512), в который будут записаны данные
 * @param block_num - номер блока диска, с которого будут считываться данные
 * @return int - 0, если всё успешно, и -1, если возникла ошибка.
 */
int disk_read_block(byte *buffer, int block_num)
{
    if(block_num < 0)
    {
        return -1;
    }
    
    //суперблок
    byte super_blok[] = {0x71,0x71,0x04,0x00,0x00,0x01,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
    //каталог с единственным файлом - file.txt
    byte first_blok[] = {0x66,0x69,0x6C,0x65,0x2E,0x74,0x78,0x74,0x00,0x00,0x00,0x00,0x00,0x0B,0x00,0x01};
    //содержимое файла file.txt - Temporary content of file.txt file.
    byte eleventh_blok[] = {0x54,0x65,0x6D,0x70,0x6F,0x72,0x61,0x72,0x79,0x20,0x63,0x6F,0x6E,0x74,0x65,0x6E,0x74,0x20,0x6F,0x66,0x20,0x66,0x69,0x6C,0x65,0x2E,0x74,0x78,0x74,0x20,0x66,0x69,0x6C,0x65,0x2E};
    
    byte bufData[BLOCK_COUNT][BLOCK_SIZE];

    for(int i =0; i < BLOCK_COUNT; i++)
    {
        for(int j = 0; j<BLOCK_SIZE;j++)
        {
            bufData[i][j] = 0x00;
        }
    }

    //Заполнение суперблока
    for(int i =0; i < sizeof(super_blok); i++)
    {
        bufData[0][i] = super_blok[i];
    }
    
    //Заполнение 1 блока
    for(int i =0; i < sizeof(first_blok); i++)
    {
        bufData[1][i] = first_blok[i];
    }

    //Заполнение 11 блока
    for(int i =0; i < sizeof(eleventh_blok); i++)
    {
        bufData[11][i] = eleventh_blok[i];
    }


    if(block_num == 0)//суперблок
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[0][i];
        }

        return 0;
    }
    if(block_num == 1)//каталок
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[1][i];
        }
        
        return 0;
    }
    if(block_num == 11)//содержимое файлаs
    {
        for(int i =0; i < BLOCK_SIZE; i++)
        {
            buffer[i] = bufData[11][i];
        }
        
        return 0;
    }

    for(int i =0; i < BLOCK_SIZE; i++)
    {
        buffer[i] = 0;
    }
    
    return 0;
}