#include <portable/types.h>

#define setjmp(x) _setjmp(x)

extern uint inb(int num);
extern void outb(int num, int data);
extern uint inw(int num);
extern void outw(int num, int data);
extern uint indw(int num);
extern void outdw(int num, int data);
extern void inw_arr(int port, int size, void *buf);
extern void outw_arr(int port, int size, void *buf);
extern void save_regs();
extern void restore_regs();
extern void disable_interrupts();
extern void enable_interrupts();
extern void load_idt(void *);
extern void *get_sp();
extern ushort kernel_code;
// тип для хранения состояния стека, используемый
//   функциями setjmp, longjmp
typedef long int jmp_buf[8];
extern int _setjmp(jmp_buf env);
extern void longjmp(jmp_buf env, int code);
