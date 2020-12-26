#include <portable/syscall.h>
#include <portable/file.h>
#include <portable/device.h>
#include <portable/types.h>
#include <x86/disk.h>

/** 
 * Запись в таблице файлов
 * dev - номер устройства (-1 - свободный слот)
 * pos - позиция в файле (в байтах)
 * start_block - первый блок файла
 * size - размер файла (в блоках)
 * @return 
 */
struct file_entry
{
    int dev;
    int pos;
    int start_block;
    int size;
};

///таблица открытых файлов
struct file_entry file_table[NUM_FILES];

///текущее количество открытых файлов
//int file_count = 0;

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
  }
}

/** 
 * Открытие файла
 * 
 * @param name имя файла
 * 
 * @return идентификатор файла, меньше нуля если ошибка
 */
int open(char *name)
{
  for(int i = 0; i < NUM_FILES; i++)//поиск первой пустой записи
  {
    if(file_table[i].dev == -1)
    {
      file_table[i].dev = 1;
      return i;
    }
  }

  return -1;//если ошибка
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
  if(id < 0 || id >= NUM_FILES)
  {
    return -1;
  }
  
  file_table[id].dev = -1;
  file_table[id].pos = 0;
  file_table[id].start_block = 0;
  file_table[id].size = 0;

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
  return 0;
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
  byte *bufByte = (byte*)buf;
  int readenCount = 0;
  
  if(disk_read_block(*bufByte, 0) == 0)
    readenCount++;
  
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
  byte *bufByte = (byte*)buf;
  int writenCount = 0;
  for(int i = 0; i < size; i++)
    {
      if(write_sym_device(SYMDEVICE_CONSOLE, *(bufByte+i*sizeof(byte))) == 0)
	writenCount++;
    }
  return writenCount;
}
