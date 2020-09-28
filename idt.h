#include "types.h"

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

void idtInit();
void idtSetDescriptor(int index, uint handler, ushort selector, uchar type);