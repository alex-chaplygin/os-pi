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
#include <x86/tss.h>
#include <portable/proc.h>

/// Указатель на местонахождение GDT
gdt_ptr_t gdt_ptr;
/// Указатель на записи GDT
gdt_entry_t gdt_entries[GDT_SIZE];
/// количество сегментов в GDT
int num_segments = GDT_SIZE;
/// Переменная записи TSS структуры
tss_entry_t tss_entry;

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
int add_descriptor(uint32_t base, uint32_t limit, uint8_t access, byte gran) {
  uint32_t num = 0;
  for (uint32_t i = 0; i < num_segments; i++) {
    if (gdt_entries[num].base_low == 0 && gdt_entries[num].base_middle == 0 && gdt_entries[num].base_high == 0 && gdt_entries[num].limit_low == 0 && gdt_entries[num].gran == 0 && gdt_entries[num].access == 0x90) {
      num = i;
      break;
    }
  }
  create_descriptor(num, base, limit, access, gran);
  ++num_segments;
  gdt_ptr.limit = sizeof(gdt_entry_t) * num_segments - 1;
  load_gdt((uint32_t) & gdt_ptr);
  return num;
}

/** 
 * удаляет сегмент в GDT и перезагружает GDT
 * 
 * @param num Селектор сегмента
 */
void remove_descriptor(int num) {
  create_descriptor(num, 0, 0, 0, 0);
  --num_segments;
  gdt_ptr.limit = sizeof(gdt_entry_t) * num_segments - 1;
  load_gdt((uint32_t) & gdt_ptr);
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
void create_descriptor(uint32_t num, uint32_t base, uint32_t limit, uint8_t access, byte gran) {
  gdt_entries[num].base_low = (base & 0xFFFF); // set base bits 15:0
  gdt_entries[num].base_middle = (base >> 16) & 0xFF; // set base bits 23:16
  gdt_entries[num].base_high = (base >> 24) & 0xFF; // set base bits 31:24
  gdt_entries[num].limit_low = limit & 0xFFFF; // set limit bits 15:0
  gdt_entries[num].gran = (limit >> 16) & 0x0F;
  gdt_entries[num].gran |= gran & 0xF0;
  gdt_entries[num].access = !access ? 0 : access | 0x90;
}

/** 
 * Инициализация TSS
 * 
 * @param num Селектор сегмента в GDT для TSS
 * @param ss0 Сегмент стека, загружаемый при переходе в режим ядра
 * @param esp0 Указатель стека, загружаемый при переходе в режим ядра
 */
static void init_tss(s32int num, u16int ss0, u32int esp0) {
  /// База и предельное значение записи в таблице GDT
  u32int base = (u32int) & tss_entry;
  u32int limit = base + sizeof(tss_entry);

  /// Добавление в таблицу GDT адрес дескриптора этого TSS
  create_descriptor(num, base, limit, SEG_CODE_EXRD | DPL(0), GRAN_DISABLE);
  memset(&tss_entry, 0, sizeof(tss_entry));
  tss_entry.ss0 = ss0; // Сегмент стека ядра.
  tss_entry.esp0 = esp0; // Указатель стека ядра.
  tss_entry.ss1 = ss0; // Сегмент стека ядра.
  tss_entry.esp1 = esp0; // Указатель стека ядра.
  tss_entry.ss2 = ss0; // Сегмент стека ядра.
  tss_entry.esp2 = esp0; // Указатель стека ядра.

  // Заносятся в таблицу TSS записи cs, ss, ds, es, fs и gs. В них указывается, какие сегменты
  // должны быть загружены в случае,  когда процессор переключается в режим ядра. Поэтому
  // они являются обычными сегментами кода/данных ядра - 0x08 и 0x10 соответственно,
  // но в последних двух битах будут указаны значения 0x0b и 0x13. Значения этих битов указывают,
  // что уровень запрашиваемых привилегий RPL (requested privilege level) равен 3; это означает, что
  // этот сегмент TSS  можно использовать для переключения в режим ядра из кольца 3.
  //tss_entry.cs = 0x0b;
  //tss_entry.ss = tss_entry.ds = tss_entry.es = tss_entry.fs = tss_entry.gs = 0x13;
}

extern int stack_space;
/** 
 * @brief  Инициализация GDT
 * 
 */
void init_memory() {
  num_segments = 6;
  gdt_ptr.limit = sizeof(gdt_entry_t) * num_segments - 1;
  gdt_ptr.base = (uint32_t) & gdt_entries;

  create_descriptor(0, 0, 0, 0, 0);
  create_descriptor(KERNEL_CODE, 0, 0xFFFFFFFF, SEG_CODE_EXRD | DPL(0), GRAN_ENABLE);
  create_descriptor(KERNEL_DATA, 0, 0xFFFFFFFF, SEG_DATA_RDWR | DPL(0), GRAN_ENABLE);
  create_descriptor(USER_CODE, 0, 0xFFFFFFFF, SEG_CODE_EXRD | DPL(3), GRAN_ENABLE);
  create_descriptor(USER_DATA, 0, 0xFFFFFFFF, SEG_DATA_RDWR | DPL(3), GRAN_ENABLE);
  init_tss(TSS, KERNEL_DS, stack_space - 8);

  load_gdt((uint32_t) & gdt_ptr); /**< Ассемблерная функция загрузки GDT */
  load_tss(TSS_CS); /// Ассемблерная функция загрузки TSS */
  mem_init();
}

/** 
 * Переключение контекста на новый процесс с помощью TSS
 * 
 */
void proc_switch()
{
  tss_entry.ss0 = current_proc->regs[55]; // ds
  tss_entry.esp0 = current_proc->stack_pointer;
  tss_entry.eip = current_proc->program_counter;
  tss_entry.eax = current_proc->regs[63];
  tss_entry.ecx = current_proc->regs[62];
  tss_entry.edx = current_proc->regs[61];
  tss_entry.ebx = current_proc->regs[60];
  tss_entry.ebp = current_proc->regs[58];
  tss_entry.esi = current_proc->regs[57];
  tss_entry.edi = current_proc->regs[56];
  tss_entry.ds = current_proc->regs[55];
  tss_entry.es = current_proc->regs[54];
  tss_entry.fs = current_proc->regs[53];
  tss_entry.gs = current_proc->regs[52];
  tss_entry.eflags = current_proc->regs[51];
  tss_entry.cs = current_proc->regs[50];
  load_tss();
}
