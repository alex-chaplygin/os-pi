#include "types.h"

extern int read_port(int num);
extern void write_port(int num, int data);
extern void save_regs(int adress);
extern void restore_regs(int adress);
extern void load_idt(void *);
extern ushort kernel_code;
