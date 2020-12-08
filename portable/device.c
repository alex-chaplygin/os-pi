
/**
 * @file   device.c
 * @author alex <alex@alex-Inspiron-N5040>
 * @date   Mon Nov 23 14:22:05 2020
 * 
 * @brief  Общий код для подсистемы ввода-вывода
 * 
 * 
 */
#include <portable/device.h>

/// таблица блочных устройств
struct block_device block_devices[NUM_BLOCK_DEVICES] = {0, 0, 0, 0};

/// таблица символьных устройств
struct symbolic_device symbolic_devices[NUM_SYMBOLIC_DEVICES] = {0, 0, 0};

/** 
 * Инициализация всех блочных и символьных устройств (вызов методов инициализации)
 * 
 */
void init_devices()
{
}

/** 
 * Вызывает метод чтения блочного устройства
 * 
 * @param device Номер устройства
 * @param buf Буфер куда будет чтение
 * @param count Число блоков, сколько прочитать
 * 
 * @return 0 - если операция успешная, иначе код ошибки
 */
int read_block_device(int device, void *buf, int count)
{
  int i = 0;
  while(i<NUM_BLOCK_DEVICES){
    if(block_devices[i].read==0){
      i++;
      if(i==NUM_BLOCK_DEVICES){return ERROR_NOMETHOD;}
    }
    else{
      block_devices[i].read(buf,count);
      return 0;
    }
  }
}

/** 
 * Вызывает метод записи блочного устройства
 * 
 * @param device Номер устройства
 * @param buf Буфер данных для записи
 * @param count Число блоков, сколько записать
 * 
 * @return 0 - если операция успешная, иначе код ошибки
 */
int write_block_device(int device, void *buf, int count)
{
  int i = 0;
  while(i<NUM_BLOCK_DEVICES){
    if(block_devices[i].write==0){
      i++;
     if(i==NUM_BLOCK_DEVICES){return ERROR_NOMETHOD;}
    }
    else{
      block_devices[i].write(buf,count);
      return 0;
    }
  }
}

/** 
 * Вызывает метод перемотки блочного устройства
 * 
 * @param device Номер устройства
 * @param pos Позиция, куда нужно переместить указатель
 * 
 * @return 0 - если операция успешная, иначе код ошибки
 */
int seek_block_device(int device, int pos)
{
  int i = 0;
  while(i<NUM_BLOCK_DEVICES){
    if(block_devices[i].seek==0){
      i++;
      if(i==NUM_BLOCK_DEVICES){return ERROR_NOMETHOD;}
    }
    else{
      block_devices[i].seek(pos);
      return 0;
    }
  }
}

/** 
 * Вызывает метод чтения символьного устройства
 * 
 * @param device Номер устройства
 * @param b Указатель, куда будет прочитан байт
 * 
 * @return 0 - если операция успешная, иначе код ошибки
 */
int read_sym_device(int device, byte *b)
{
  int i = 0;
  while(i<NUM_SYMBOLIC_DEVICES){
    if(symbolic_devices[i].read==0){
      i++;
      if(i==NUM_SYMBOLIC_DEVICES){return ERROR_NOMETHOD;}
    }
    else{
      symbolic_devices[i].read(b);
      return 0;
    }
  }
}

/** 
 * Вызывает метод записи символьного устройства
 * 
 * @param device Номер устройства
 * @param b Байт, который нужно записать в устройство
 * 
 * @return 0 - если операция успешная, иначе код ошибки
 */
int write_sym_device(int device, byte b)
{
  int i = 0;
  while(i<NUM_SYMBOLIC_DEVICES){
    if(symbolic_devices[i].write==0){
      i++;
      if(i==NUM_SYMBOLIC_DEVICES){return ERROR_NOMETHOD;}
    }
    else{
      symbolic_devices[i].write(b);
      return 0;
    }
  }
}
