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
 * Инициализация всех блочных и символьных устройств
 * 
 */
void init_devices()
{
}

int read_block_device(int device, void *buf, int count)
{
}

int write_block_device(int device, void *buf, int count)
{
}

int seek_block_device(int device, int pos)
{
}

int read_sym_device(int device, byte *b)
{
}

int write_sym_device(int device, byte b)
{
}
