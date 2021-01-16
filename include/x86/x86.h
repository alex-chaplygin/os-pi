#include <portable/types.h>

extern int read_port(int num);
extern void write_port(int num, int data);
extern void save_regs();
extern void restore_regs();
extern void disable_interrupts();
extern void enable_interrupts();
extern void load_idt(void *);
extern void *get_sp();
extern ushort kernel_code;
