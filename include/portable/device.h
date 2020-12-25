#include "types.h"

#define NUM_BLOCK_DEVICES 1	/**< число блочных устройств */
#define NUM_SYMBOLIC_DEVICES 2	/**< число символьных устройств */

#define HARD_DISK 0

#define SYMDEVICE_CONSOLE 1	/**< символьное устройство консоль */

#define ERROR_NODEVICE -1 /**< нет устройства с таким именем */
#define ERROR_NOMETHOD -2 /**< пустой указатель на метод */

/// Структура для блочного устройства
struct block_device {
  /// инициализация устройства
  void (*init)();
  /// чтение блоков в буффер
  int (*read)(void *buf, int block_count);
  /// запись блоков в буффер
  int (*write)(void *buf, int block_count);
  /// установка позиции на блок
  int (*seek)(int block_num);
};

/// Структура для символьного устройства
struct symbolic_device{
  /// инициализация устройства
  void (*init)();
  /// чтение байта из устройства
  int (*read)(byte *b);
  /// запись байта в устройство
  int (*write)(byte b);
};

void init_devices();
int read_block_device(int device, void *buf, int count);
int write_block_device(int device, void *buf, int count);
int seek_block_device(int device, int pos);
int read_sym_device(int device, byte *b);
int write_sym_device(int device, byte b);
