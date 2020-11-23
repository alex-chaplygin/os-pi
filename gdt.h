/**
 * @file   gdt.h
 * @author Дмитрий <senior@seniorPC>
 * @date   Mon Oct 26 09:06:50 2020
 * 
 * @brief  Header-файл для инициализации в gdt.c
 * 
 * 
 */
#include "types.h"
// Each define here is for a specific flag in the descriptor.
// Refer to the intel documentation for a description of what each one does.

#define DPL(x)     (((x) &  0x03) << 0x05)   // Set privilege level (0 - 3)
#define GRAN_ENABLE 0xC0
#define GRAN_DISABLE 0x0
 
#define SEG_DATA_RD        0x00 // Read-Only
#define SEG_DATA_RDA       0x01 // Read-Only, accessed
#define SEG_DATA_RDWR      0x02 // Read/Write
#define SEG_DATA_RDWRA     0x03 // Read/Write, accessed
#define SEG_DATA_RDEXPD    0x04 // Read-Only, expand-down
#define SEG_DATA_RDEXPDA   0x05 // Read-Only, expand-down, accessed
#define SEG_DATA_RDWREXPD  0x06 // Read/Write, expand-down
#define SEG_DATA_RDWREXPDA 0x07 // Read/Write, expand-down, accessed
#define SEG_CODE_EX        0x08 // Execute-Only
#define SEG_CODE_EXA       0x09 // Execute-Only, accessed
#define SEG_CODE_EXRD      0x0A // Execute/Read
#define SEG_CODE_EXRDA     0x0B // Execute/Read, accessed
#define SEG_CODE_EXC       0x0C // Execute-Only, conforming
#define SEG_CODE_EXCA      0x0D // Execute-Only, conforming, accessed
#define SEG_CODE_EXRDC     0x0E // Execute/Read, conforming
#define SEG_CODE_EXRDCA    0x0F // Execute/Read, conforming, accessed
 
#define GDT_SIZE 5

/// Эта структура содержит значения для одной записи GDT
struct gdt_entry_struct {
    u16int limit_low;		/**< Младшие 16 бит смещения */
    u16int base_low;		/**< Младшие 16 бит базы */
    u8int  base_middle;		/**< Следующие восемь бит базы */
    byte  access;		/**< Флаг определяет уровень доступа */
  byte gran;			/**< гранулярность */
    u8int  base_high;		/**< Cтаршие 8 бит базы */
} __attribute__((packed));
/// Указатель на структуру
typedef struct gdt_entry_struct gdt_entry_t;

struct gdt_ptr_struct {
    u16int limit;		/**< Старшие 16 бит смещения селектора */
    u32int base;		/**< Адрес первой структуры gdt_entry_t */
} __attribute__((packed));
/// Указатель на структуру
typedef struct gdt_ptr_struct gdt_ptr_t;

/** 
 * Создание дескриптора сегмента
 * 
 * @param uint32_t Селектор сегмента
 * @param uint32_t Базовый адрес сегмента
 * @param uint32_t Предельный адрес сегмента
 * @param uint8_t Флаг доступа к сегменту
 * @param byte Гранулярность
 */
void create_descriptor(uint32_t, uint32_t, uint32_t, uint8_t, byte);

/** 
 * Инициализирующая GDT функция
 * 
 */
void init_gdt();

/** 
 * добавляет сегмент в GDT под очередным номером и перезагружает GDT
 * 
 * @param uint32_t Селектор сегмента
 * @param uint32_t Базовый адрес сегмента
 * @param uint32_t Предельный адрес сегмента
 * @param uint8_t Флаг доступа к сегменту
 * @param byte Гранулярность
 * 
 * @return Селектор сегмента
 */
int add_descriptor(uint32_t, uint32_t, uint8_t, byte);

/** 
 * удаляет сегмент в GDT и перезагружает GDT
 * 
 * @param int Селектор сегмента
 */
void remove_descriptor(int);

extern void load_gdt(u32int);
/// количество сегментов в GDT
extern int num_segments;
