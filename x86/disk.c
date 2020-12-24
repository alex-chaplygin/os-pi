#include <x86/disk.h>

///массив сегментов диска
byte disk_data[BLOCK_COUNT][BLOCK_SIZE];

/** 
 * @brief init_disk() инициализирует массив сегментов диска
 * 
 */
void init_disk()
{
    //Инициализация диска
    for(int i =0; i < BLOCK_COUNT; i++)
    {
        for(int j = 0; j<BLOCK_SIZE;j++)
        {
            disk_data[i][j] = 0x00;
        }
    }

    //Тестовые данные

    //Суперблок
    byte super_blok[] = {0x71,0x71,0x04,0x00,0x00,0x01,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
    //Каталог с единственным файлом - file.txt
    byte first_blok[] = {0x66,0x69,0x6C,0x65,0x2E,0x74,0x78,0x74,0x00,0x00,0x00,0x00,0x00,0x0B,0x00,0x01};
    //Содержимое файла file.txt - Temporary content of file.txt file.
    byte eleventh_blok[] = {0x54,0x65,0x6D,0x70,0x6F,0x72,0x61,0x72,0x79,0x20,0x63,0x6F,0x6E,0x74,0x65,0x6E,0x74,0x20,0x6F,0x66,0x20,0x66,0x69,0x6C,0x65,0x2E,0x74,0x78,0x74,0x20,0x66,0x69,0x6C,0x65,0x2E};

    //Добавление тестовых данных

    //Заполнение суперблока
    for(int i =0; i < sizeof(super_blok); i++)
    {
        disk_data[0][i] = super_blok[i];
    }
    
    //Заполнение 1 блока
    for(int i =0; i < sizeof(first_blok); i++)
    {
        disk_data[1][i] = first_blok[i];
    }

    //Заполнение 11 блока
    for(int i =0; i < sizeof(eleventh_blok); i++)
    {
        disk_data[11][i] = eleventh_blok[i];
    }
}

/** 
 * Возвращает данные с тестового жесткого диска
 * @param buffer - буфер (размер 512), в который будут записаны данные
 * @param block_num - номер блока диска, с которого будут считываться данные
 * @return int - 0, если всё успешно, и -1, если возникла ошибка.
 */
int disk_read_block(byte *buffer, int block_num)
{
    init_disk();

    if(block_num < 0 || block_num >= BLOCK_COUNT)
    {
        return -1;
    }
    
    for(int i =0; i < BLOCK_SIZE; i++)
    {
        buffer[i] = disk_data[block_num][i];
    }

    return 0;
}