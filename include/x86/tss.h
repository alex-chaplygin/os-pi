// Структура, описывающая сегмент состояния задачи Task State Segment.
struct tss_struct
{
   u32int prev_tss;   // Предыдущий TSS – если используется аппаратное переключение задач, то это поле нужно создания связного списка.
   u32int esp0;       // Указатель стека, загружаемый при переходе в режим ядра.
   u32int ss0;        // Сегмент стека, загружаемый при переходе в режим ядра.
   u32int esp1;       // Не используется ...
   u32int ss1;
   u32int esp2;
   u32int ss2;
   u32int cr3;
   u32int eip;
   u32int eflags;
   u32int eax;
   u32int ecx;
   u32int edx;
   u32int ebx;
   u32int esp;
   u32int ebp;
   u32int esi;
   u32int edi;
   u32int es;         // Значение, загружаемое в ES при переходе в режим ядра.
   u32int cs;         // Значение, загружаемое в  CS при переходе в режим ядра
   u32int ss;         // Значение, загружаемое в  SS при переходе в режим ядра
   u32int ds;         // Значение, загружаемое в  DS при переходе в режим ядра
   u32int fs;         // Значение, загружаемое в  FS при переходе в режим ядра
   u32int gs;         // Значение, загружаемое в  GS при переходе в режим ядра
   u32int ldt;        // Не используется ...
   u16int trap;
   u16int iomap_base;
}  
