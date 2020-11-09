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
/// Указатель на местонахождение GDT
gdt_ptr_t    gdt_ptr;
/// Указатель на записи GDT
gdt_entry_t    gdt_entries[GDT_SIZE];


/** 
 * Создание дескриптора сегмента.
 * 
 * @param num Селектор сегмента.
 * @param base Базовый адрес сегмента
 * @param limit Предельный адрес сегмента.
 * @param flag Флаги доступа к сегменту.
 */
void create_descriptor(uint32_t num,uint32_t base, uint32_t limit, uint16_t flag)
{ 
    // Create the high 32 bit segment
    gdt_entries[num].access = (flag <<  8) & 0x00F0FF00;         // set type, p, dpl, s, g, d/b, l and avl fields
    gdt_entries[num].base_middle = (base >> 16) & 0x000000FF;         // set base bits 23:16
    gdt_entries[num].base_high =  base & 0xFF000000;         // set base bits 31:24
     // Shift by 32 to allow for low part of segment

 
    // Create the low 32 bit segment
    gdt_entries[num].base_low = base  << 16;                       // set base bits 15:0
    gdt_entries[num].limit_low = limit  & 0x0000FFFF;               // set limit bits 15:0
}

/** 
 * @brief  Инициализация GDT
 * 
 */
void init_gdt()
{
	gdt_ptr.limit = (sizeof(gdt_entry_t)*5) - 1;
    gdt_ptr.base  = (uint32_t) & gdt_entries;
	
    create_descriptor(0, 0, 0, 0);
    create_descriptor(1, 0, 0xFFFFFFFF, (GDT_CODE_PL0));
    create_descriptor(2, 0, 0xFFFFFFFF, (GDT_DATA_PL0));
    create_descriptor(3, 0, 0xFFFFFFFF, (GDT_CODE_PL3));
    create_descriptor(4, 0, 0xFFFFFFFF, (GDT_DATA_PL3));

    load_gdt((uint32_t)&gdt_ptr);
}
