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
/// Количество записей
#define GDT_SIZE 5
/// Гранулярность
#define GRAN 0xCF
/// Размер записей таблицы
#define LIM 0xFFFFFFFF

/// Эта структура содержит значения для одной записи GDT
struct gdt_entry_struct {
    u16int limit_low;		/**< Младшие 16 бит смещения */
    u16int base_low;		/**< Младшие 16 бит базы */
    u8int  base_middle;		/**< Следующие восемь бит базы */
    u8int  access;		/**< Флаг определяет уровень доступа */
    u8int  granularity;
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
 * Инициализирующая GDT функция
 * 
 */
void init_gdt();
extern void load_gdt(u32int); 
