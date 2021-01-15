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
#include <x86/console.h>
#include <x86/disk.h>
#include <x86/ide.h>

/// таблица блочных устройств
struct block_device block_devices[NUM_BLOCK_DEVICES] = {0, 0, 0, 0, init_ide, disk_read_block, disk_write_block, 0};

/// таблица символьных устройств
struct symbolic_device symbolic_devices[NUM_SYMBOLIC_DEVICES] = {0, 0, 0, console_clear, 0, putchar};

/** 
 * Инициализация всех блочных и символьных устройств (вызов методов инициализации)
 * 
 */
void init_devices()
{
  int i=0, a=0;
  kprint("Init devices\n");
  while(i<NUM_BLOCK_DEVICES){
    if(block_devices[i].init==0){
      //return ERROR_NODEVICE;
      i++;
    }
    else{
      block_devices[i].init();
      i++;
    } 
  }
  while(a<NUM_SYMBOLIC_DEVICES){
    if(symbolic_devices[a].init==0){
      //return ERROR_NODEVICE;
      a++;
    }
    else{
      symbolic_devices[a].init();
      a++;
    } 
  }
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
    if(block_devices[device].read==0){
      return ERROR_NOMETHOD;
    }
    else{
      block_devices[device].read(buf,count);
      return 0;
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
    if(block_devices[device].write==0){
      return ERROR_NOMETHOD;
    }
    else{
      block_devices[device].write(buf,count);
      return 0;
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
    if(block_devices[device].seek==0){
      return ERROR_NOMETHOD;
    }
    else{
      block_devices[device].seek(pos);
      return 0;
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
    if(symbolic_devices[device].read==0){
      return ERROR_NOMETHOD;
    }
    else{
      symbolic_devices[device].read(b);
      return 0;
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
    if(symbolic_devices[device].write==0){
      return ERROR_NOMETHOD;
    }
    else{
      symbolic_devices[device].write(b);
      return 0;
    }
}
