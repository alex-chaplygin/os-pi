int sys_call(int num, int param1, int param2, int param3);

#define ERROR_NOFILE -1		/**< файл не найден */
#define ERROR_MAXFILE -2	/**< достигнут максимум файлов */
#define ERROR_ACCESS_DENIED -3	/**< доступ запрещен */
#define ERROR_INVALID_PARAMETERS -4 /**< неверные параметры */
#define ERROR_IO -5		/**< ошибка ввода-вывода, аппаратная ошибка */
#define ERROR_MAXPROC -6	/**< достигнут максимум процессов */

/** 
 * Открытие файла
 * 
 * @param name имя файла
 * 
 * @return идентификатор файла, меньше нуля если ошибка
 */
int open(char *name);

/** 
 * Создание файла
 * 
 * @param name имя файла
 * 
 * @return идентификатор файла, меньше нуля если ошибка
 */
int create(char *name);

/** 
 * Закрытие файла
 * 
 * @param id идентификатор файла
 * 
 * @return 0 - успешно, меньше нуля если ошибка
 */
int close(int id);

/** 
 * Перемещение позиции в файле
 * 
 * @param id идентификатор файла
 * @param offset смещение относительно начала файла
 * 
 * @return если offset равен 0, то текущая позиция в файле, иначе 0 - успех, меньше нуля если ошибка
 */
int seek(int id, int offset);

#define ATTR_REGULAR    1		/**< обычный файл */
#define ATTR_DIRECTORY 2	/**< директория */
#define ATTR_DEVICE        3	/**< файл устройство */

/// информация о файле
struct file_info {
  int length;			/**< длина файла */
  int attrib:2;			/**< атрибуты файла */
  int device_num:8;		/**< номер устройства */
};

/** 
 * Получить информацию о файле
 * 
 * @param id идентификатор файла
 * @param info указатель на структуру информации
 * 
 * @return 0 - успешно, меньше нуля если ошибка
 */
int fstat(int id, struct file_info *info);

/** 
 * Установка атрибутов файла
 * 
 * @param id идентификатор файла
 * @param attr атрибуты файла
 * 
 * @return 0 - успешно, меньше нуля если ошибка
 */
int set_attr(int id, int attr);

/** 
 * Чтение из файла
 * 
 * @param id идентификатор файла
 * @param buf указатель на буфер, куда будет чтение
 * @param size число байт, сколько прочитать
 * 
 * @return число прочитанных байт, меньше нуля если ошибка
 */
int read(int id, void *buf, int size);

/** 
 * Запись в файл
 * 
 * @param id идентификатор файла
 * @param buf указатель на буфер для записи
 * @param size число байт, сколько записать
 * 
 * @return число записанных байт, меньше нуля если ошибка
 */
int write(int id, void *buf, int size);

/** 
 * Создание дочернего процесса, копии родительского
 * 
 * 
 * @return для родительского процесса возвращается идентификатор дочернего процесса, -1 - для дочернего
 */
int fork();

/** 
 * Заменяет текущий процесс на новый
 * 
 * @param name имя исполняемого файла
 * 
 * @return 0 - успешно, меньше нуля если ошибка
 */
int exec(char *name);

/** 
 * Завершает текущий процесс
 * 
 * @param code код возврата
 */
void exit(int code);

/** 
 * Ждет завершения процесса
 * 
 * @param id идентификатор процесса
 */
void wait(int id);
