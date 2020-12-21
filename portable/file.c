#include <portable/syscall.h>
#include <portable/device.h>
#include <portable/types.h>

/** 
 * Открытие файла
 * 
 * @param name имя файла
 * 
 * @return идентификатор файла, меньше нуля если ошибка
 */
int open(char *name)
{
  return 0;
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
  return 0;
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
  for(int i = 0; i < size; i++)
    {
      return write_sym_device(SYMDEVICE_CONSOLE, *(bufByte+i*sizeof(byte)));
    }
}
