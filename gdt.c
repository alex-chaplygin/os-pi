/**
 * @file   gdt.c
 * @author Дмитрий <senior@seniorPC>
 * @date   Mon Oct 26 09:00:21 2020
 * 
 * @brief  Инициализация GDT
 * 
 * 
 */

#include "gdt.h"

/// Указатель на записи GDT
gdt_entry_t    gdt_entries[GDT_SIZE];
/// Указатель на местонахождение GDT
gdt_ptr_t    gdt_ptr;

/** 
 * @brief Устанавливает значения для пяти дескрипторов
 * 
 * @param num Количество записей
 * @param base Базовый адрес сегмента
 * @param limit Размер записей таблицы
 * @param access Флаг определяет уровень доступа
 * @param gran Гранулярность
 */
static void gdt_set_gate(u32int num, u32int base, u32int limit, u8int access, u8int gran)
{
    gdt_entries[num].base_low     = (base & 0xFFFF);
    gdt_entries[num].base_middle = (base >> 16) & 0xFF;
    gdt_entries[num].base_high     = (base >> 24) & 0xFF;

    gdt_entries[num].limit_low     = (limit & 0xFFFF);
    gdt_entries[num].granularity = (limit >> 16) & 0x0F;

    gdt_entries[num].granularity |= gran & 0xF0;
    gdt_entries[num].access         = access;
}


/** 
 * @brief  Инициализация GDT
 * 
 */
void init_gdt()
{
    gdt_ptr.limit = (sizeof(gdt_entry_t)*5) - 1;
    gdt_ptr.base  = (u32int) & gdt_entries;

    gdt_set_gate(0, 0, 0, 0, 0); /**< Нулевой сегмент */
    gdt_set_gate(1, 0, LIM, 0x9A, GRAN);	/**< Сегмент кода */
    gdt_set_gate(2, 0, LIM, 0x92, GRAN);	/**< Сегмент данных */
    gdt_set_gate(3, 0, LIM, 0xFA, GRAN);	/**< Сегмент кода уровня пользовательских процессов */
    gdt_set_gate(4, 0, LIM, 0xF2, GRAN);	/**< Сегмент данных уровня пользовательских процессов */

    load_gdt((u32int)&gdt_ptr);
}


