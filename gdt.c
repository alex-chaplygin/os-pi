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
void create_descriptor(uint32_t num,uint32_t base, uint32_t limit, uint8_t access, byte gran)
{ 
  gdt_entries[num].base_low = (base & 0xFFFF);                       // set base bits 15:0
    gdt_entries[num].base_middle = (base >> 16) & 0xFF;         // set base bits 23:16
    gdt_entries[num].base_high =  (base >> 24) & 0xFF;         // set base bits 31:24
    gdt_entries[num].limit_low = limit  & 0xFFFF;               // set limit bits 15:0
    gdt_entries[num].gran = (limit >> 16) & 0x0F;
    gdt_entries[num].gran |= gran & 0xF0;
    gdt_entries[num].access = access;  
}

/** 
 * @brief  Инициализация GDT
 * 
 */
void init_gdt()
{
    gdt_ptr.limit = sizeof(gdt_entry_t)*5 - 1;
    gdt_ptr.base  = (uint32_t) & gdt_entries;
	
    create_descriptor(0, 0, 0, 0, 0);
    create_descriptor(1, 0, 0xFFFFFFFF, 0x9A, 0xCF);
    create_descriptor(2, 0, 0xFFFFFFFF, 0x92, 0xCF);
    create_descriptor(3, 0, 0xFFFFFFFF, 0xFA, 0xCF);
    create_descriptor(4, 0, 0xFFFFFFFF, 0xF2, 0xCF);

    load_gdt((uint32_t)&gdt_ptr);
}
