#include <portable/types.h>

extern int read_port(int num);
extern void write_port(int num, int data);
extern void load_idt(void *);
extern ushort kernel_code;