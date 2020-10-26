#include "gdt.h"

gdt_entry_t    gdt_entries[GDT_SIZE];
gdt_ptr_t    gdt_ptr;

static void init_gdt()
{
    gdt_ptr.limit = (sizeof(gdt_entry_t)*5) - 1;
    gdt_ptr.base  = (u32int) & gdt_entries;

    gdt_set_gate(0, 0, 0, 0, 0);                // Нулевой сегмент
    gdt_set_gate(1, 0, 0xFFFFFFFF, 0x9A, 0xCF);    // Сегмент кода
    gdt_set_gate(2, 0, 0xFFFFFFFF, 0x92, 0xCF);    // Сегмент данных
    gdt_set_gate(3, 0, 0xFFFFFFFF, 0xFA, 0xCF);    // Сегмент кода уровня пользовательских процессов
    gdt_set_gate(4, 0, 0xFFFFFFFF, 0xF2, 0xCF);    // Сегмент данных уровня пользовательских процессов

    //название_ассемблерной_функции((u32int)&gdt_ptr); нужна ассемблерная функция
}

static void gdt_set_gate(s32int num, u32int base, u32int limit, u8int access, u8int gran)
{
    gdt_entries[num].base_low     = (base & 0xFFFF);
    gdt_entries[num].base_middle = (base >> 16) & 0xFF;
    gdt_entries[num].base_high     = (base >> 24) & 0xFF;

    gdt_entries[num].limit_low     = (limit & 0xFFFF);
    gdt_entries[num].granularity = (limit >> 16) & 0x0F;

    gdt_entries[num].granularity |= gran & 0xF0;
    gdt_entries[num].access         = access;
}
