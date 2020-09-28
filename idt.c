#include "idt.h"

idtGateDescriptor idt[256];

void idtSetDescriptor(int index, uint handler, ushort selector, uchar type) {
    idtGateDescriptor* d = &idt[index];

    ushort lowOffset = handler & 0xFFFF;
    ushort highOffset = handler >> 16;

    d->lowOffset = lowOffset;
    d->selector = selector;
    d->type = type;
    d->highOffset = highOffset;
}