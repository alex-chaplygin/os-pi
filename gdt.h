#include "types.h"

#define GDT_SIZE 5

// Эта структура содержит значения для одной записи GDT
struct gdt_entry_struct {
    u16int limit_low;    // Младшие 16 бит смещения
    u16int base_low;    // Младшие 16 бит базы
    u8int  base_middle;    // Следующие восемь бит базы
    u8int  access;        // Флаг определяет уровень доступа
    u8int  granularity;
    u8int  base_high;    // старшие 8 бит базы
} __attribute__((packed));

typedef struct gdt_entry_struct gdt_entry_t;

struct gdt_ptr_struct {
    u16int limit;    // старшие 16 бит смещения селектора
    u32int base;    // адрес первой структуры gdt_entry_t
} __attribute__((packed));

typedef struct gdt_ptr_struct gdt_ptr_t;

// Инициализирующая GDT функция
void init_gdt();
