/**
 * @file   gdt.c
 * @author Дмитрий <senior@seniorPC>
 * @date   Mon Oct 26 09:00:21 2020
 * 
 * @brief  Инициализация GDT
 * 
 * 
 */

#include <x86/gdt.h>
/// Указатель на местонахождение GDT
gdt_ptr_t    gdt_ptr;
/// Указатель на записи GDT
gdt_entry_t    gdt_entries[GDT_SIZE];
/// количество сегментов в GDT
int num_segments = GDT_SIZE;

/** 
 * добавляет сегмент в GDT под очередным номером и перезагружает GDT
 * 
 * @param num Селектор сегмента.
 * @param base Базовый адрес сегмента
 * @param limit Предельный адрес сегмента
 * @param access Флаг доступа к сегменту
 * @param gran Гранулярность
 * 
 * @return Селектор сегмента
 */
int add_descriptor(uint32_t base, uint32_t limit, uint8_t access, byte gran)
{
	uint32_t num = 2;
	for (uint32_t i=2; i < num_segments; i++)
	{
		if (gdt_entries[num].base_low == 0 && gdt_entries[num].base_middle == 0 && gdt_entries[num].base_high == 0 && gdt_entries[num].limit_low == 0 && gdt_entries[num].gran == 0 && gdt_entries[num].access == 0x90)
		{
			num = i;
			break;
		}
	}
    create_descriptor(num, base, limit, access, gran);
	++num_segments;
	gdt_ptr.limit = sizeof(gdt_entry_t)*num_segments - 1;
    load_gdt((uint32_t)&gdt_ptr);
    return num;
}

/** 
 * удаляет сегмент в GDT и перезагружает GDT
 * 
 * @param num Селектор сегмента
 */
void remove_descriptor(int num)
{
    create_descriptor(num, 0, 0, 0, 0);
	--num_segments;
	gdt_ptr.limit = sizeof(gdt_entry_t)*num_segments - 1;
    load_gdt((uint32_t)&gdt_ptr);
}

/** 
 * Создание дескриптора сегмента
 * 
 * @param num Селектор сегмента.
 * @param base Базовый адрес сегмента
 * @param limit Предельный адрес сегмента
 * @param access Флаг доступа к сегменту
 * @param gran Гранулярность
 */
void create_descriptor(uint32_t num, uint32_t base, uint32_t limit, uint8_t access, byte gran)
{ 
    gdt_entries[num].base_low = (base & 0xFFFF);                       // set base bits 15:0
    gdt_entries[num].base_middle = (base >> 16) & 0xFF;         // set base bits 23:16
    gdt_entries[num].base_high =  (base >> 24) & 0xFF;         // set base bits 31:24
    gdt_entries[num].limit_low = limit  & 0xFFFF;               // set limit bits 15:0
    gdt_entries[num].gran = (limit >> 16) & 0x0F;
    gdt_entries[num].gran |= gran & 0xF0;
    gdt_entries[num].access = !access ? 0 : access | 0x90;  
}

/** 
 * @brief  Инициализация GDT
 * 
 */
void init_memory()
{
    gdt_ptr.limit = sizeof(gdt_entry_t)*num_segments - 1;
    gdt_ptr.base  = (uint32_t) & gdt_entries;
	
    create_descriptor(0, 0, 0, 0, 0);
    create_descriptor(1, 0, 0xFFFFFFFF, SEG_CODE_EXRD | DPL(0), GRAN_ENABLE); 
    create_descriptor(2, 0, 0xFFFFFFFF, SEG_DATA_RDWR | DPL(0), GRAN_ENABLE); 
    create_descriptor(3, 0, 0xFFFFFFFF, SEG_CODE_EXRD | DPL(3), GRAN_ENABLE);
    create_descriptor(4, 0, 0xFFFFFFFF, SEG_DATA_RDWR | DPL(3), GRAN_ENABLE);

    load_gdt((uint32_t)&gdt_ptr);
    mem_init();
}
