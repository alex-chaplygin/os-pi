#include <portable/types.h>

#define NUM_FILES 320		/**< максимальное число открытых файлов в системе */
#define FS_MAGIC 0x7171		/**< идентификатор файловой системы */
#define CATALOG_SIZE 10		/**< размер каталога */
#define DATA_START 11		/**< блок начала данных */
#define FILE_RECORD_SIZE 16
#define FILE_NAME_SIZE 12	/**< максимальный размер имени файла */

/// блок описания файловой системы
struct superblock
{
  ushort magic;			/**< уникальный идентификатор */
  ushort num_blocks;		/**< всего блоков на диске */
  ushort num_files;		/**< всего файлов в файловой системе */
  ushort block_size;		/**< размер блока в байтах */
};

/// запись для файла в каталоге
struct disk_file_entry
{
  char file_name[FILE_NAME_SIZE]; /**< имя файла */
  ushort first_block;		/**< номер первого блока файла */
  ushort block_count;		/**< количество блоков у файла */
};
  
/** 
 * 
 * dev - номер устройства (-1 - свободный слот)
 * pos - позиция в файле (в байтах)
 * start_block - первый блок файла
 * size - размер файла (в блоках)
 * attr - атрибуты файла
 * @return 
 */
/// Запись в таблице файлов 
struct file_entry
{
  int dev;
  int pos;
  int start_block;
  int size;
  int attr;
};

int set_attr(int id, int attr);
int get_attr(int id);
int open(char *name);
int close(int id);
int create(char *name);
int read(int id, void *buf, int size);
int write(int id, void *buf, int size);
int find_file(char *name, byte *buffer);
void init_files();
