#include "types.h"

#define IDT_SIZE 256

// idtGateDescriptor represents
// a row for interrupt descriptors
// table.
typedef struct idtGateDescriptor {
    ushort lowOffset;
    ushort selector;
    uchar zero;
    uchar type;
    ushort highOffset;
} idtGateDescriptor;

void idtSetDescriptor(int index, uint handler, ushort selector, uchar type);
void idtInit();

extern void load_idt(void *);
extern ushort kernel_code;
