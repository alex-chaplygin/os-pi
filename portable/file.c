#include <portable/syscall.h>
#include <portable/file.h>
#include <portable/device.h>
#include <x86/disk.h>
#include <portable/libc.h>


///таблица открытых файлов
struct file_entry file_table[NUM_FILES];

/** 
 * init_files() инициализирует таблицу файлов
 * 
 */
void init_files()
{
  for (int i = 0; i < NUM_FILES; i++)
  {
    file_table[i].dev = -1;
    file_table[i].pos = 0;
    file_table[i].start_block = 0;
    file_table[i].size = 0;
    file_table[i].attrib = 0;
    
  }
}

/** 
 * Открытие файла
 * 
 * @param name имя файла
 * 
 * @return идентификатор файла, -1, если возникла ошибка
 */
int open(char *name)
{
  if (name[0] == 0x00 || sizeof(name) > FILE_NAME_SIZE + 1)
  {
    return ERROR_INVALID_PARAMETERS; //если имя файла не задано или превышает лимит длины
  }
  
  byte buffer[FILE_RECORD_SIZE - FILE_NAME_SIZE];

  if (find_file(name, buffer) < 0)
  {
    return ERROR_NOFILE; //если файл не найден
  }

  int start_block_file = buffer[0] + buffer[1] * 256; //первый блок файла
  int size_file = buffer[2] + buffer[3] * 256;        //размер файла (в блоках)
  int position_file = start_block_file * BLOCK_SIZE;  //позиция на диске (в байтах)

  for (int i = 0; i < NUM_FILES; i++) //поиск первой пустой записи
  {
    if (file_table[i].dev == -1)
    {
      file_table[i].dev = SYMDEVICE_CONSOLE;
      file_table[i].pos = position_file;
      file_table[i].start_block = start_block_file;
      file_table[i].size = size_file;
      file_table[i].attrib = ATTR_REGULAR;
      return i;
    }
  }

  return ERROR_MAXFILE; //если нет свободного блока
}

/** 
 * Создание файла
 * 
 * @param name имя файла
 * 
 * @return идентификатор файла, меньше нуля если ошибка
 */
int create(char *name)
{
  return 0;
}

/** 
 * Закрытие файла
 * 
 * @param id идентификатор файла
 * 
 * @return 0 - успешно, меньше нуля если ошибка
 */
int close(int id)
{
  if (id < 0 || id >= NUM_FILES)
  {
    return ERROR_INVALID_PARAMETERS;
  }

  file_table[id].dev = -1;
  file_table[id].pos = 0;
  file_table[id].start_block = 0;
  file_table[id].size = 0;
  file_table[id].attrib = 0;

  return 0;
}

/** 
 * Перемещение позиции в файле
 * 
 * @param id идентификатор файла
 * @param offset смещение относительно начала файла
 * 
 * @return если offset равен 0, то текущая позиция в файле, иначе 0 - успех, меньше нуля если ошибка
 */
int seek(int id, int offset)
{
  return 0;
}

/** 
 * Получить информацию о файле
 * 
 * @param id идентификатор файла
 * @param info указатель на структуру информации
 * 
 * @return 0 - успешно, меньше нуля если ошибка
 */

int fstat(int id, struct file_info *info)
{
  if(file_table[id].dev<0){
    return ERROR_NOFILE;
  }
  else if(!info){
    return ERROR_INVALID_PARAMETERS;
  }
  else{    
    info->length =  file_table[id].size;
    info->attrib =  file_table[id].attrib;
    info->device_num = file_table[id].dev;
   
    return 0;
  }
}

/** 
 * Установка атрибутов файла
 * 
 * @param id идентификатор файла
 * @param attr атрибуты файла
 * 
 * @return 0 - успешно, меньше нуля если ошибка
 */
int set_attr(int id, int attr)
{
  return 0;
}

/** 
 * Чтение из файла
 * 
 * @param id идентификатор файла
 * @param buf указатель на буфер, куда будет чтение
 * @param size число байт, сколько прочитать
 * 
 * @return число прочитанных байт, меньше нуля если ошибка
 */
int read(int id, void *buf, int size)
{
  byte *bufByte = (byte*)malloc(BLOCK_SIZE);
  int readenCount = 0;
  if (id == 1) return read_char(buf);
  if(disk_read_block(bufByte, 0) == 0)
    {
      memcpy(buf, bufByte, size);
      readenCount = size;
    }
  
  return readenCount;
}

/** 
 * Запись в файл
 * 
 * @param id идентификатор файла
 * @param buf указатель на буфер для записи
 * @param size число байт, сколько записать
 * 
 * @return число записанных байт, меньше нуля если ошибка
 */
int write(int id, void *buf, int size)
{
  byte *bufByte = (byte *)buf;
  int writenCount = 0;
  for (int i = 0; i < size; i++)
  {
    if (write_sym_device(SYMDEVICE_CONSOLE, *(bufByte + i * sizeof(byte))) == 0)
      writenCount++;
  }
  return writenCount;
}

/** 
 * @brief Производит поиск файла в каталоге
 * 
 * @param name - имя файла, который требуется найти
 * @param buffer - буфер (размер 4), в который будет записано расположение и размер файла
 * @return int - 0, если всё успешно, и -1, если файл не найден.
 */
int find_file(char *name, byte *buffer)
{
    byte block[BLOCK_SIZE];

    for (char i = 1; i <= CATALOG_SIZE; i++)
    {
        if (disk_read_block(block, i) < 0)
        {
            return -1;
        }
        for (int j = 0; j < BLOCK_SIZE; j += 16)
        {
            byte temp[8];

            for (int x = 0; x < FILE_NAME_SIZE; x++)
            {
                temp[x] = block[j + x];
            }

            if (str_compare(name, temp) < 0)
                continue;

            for (char y = FILE_NAME_SIZE; y < FILE_RECORD_SIZE; y++)
            {
                buffer[y - FILE_NAME_SIZE] = block[j + y];
            }
            return 0;
        }
    }
    return -1;
}
