#include "idt.h"
#include "isr.h"

idtGateDescriptor idt[IDT_SIZE];
unsigned long idt_ptr[2];

// idtInit initializes IDT in the static memory.
void idtInit() {
    idtSetDescriptor(0, (uint)a_isrZeroDivisionException, kernel_code, 0x8E);
    idtSetDescriptor(1, (uint)isrDebugException, kernel_code, 0x8E);
    idtSetDescriptor(2, (uint)isrNonMaskableInterruptException, kernel_code, 0x8E);
    idtSetDescriptor(3, (uint)isrBreakpointException, kernel_code, 0x8E);
    idtSetDescriptor(4, (uint)isrIntoDetectedOverflowException, kernel_code, 0x8E);
    idtSetDescriptor(5, (uint)isrOutOfBoundsException, kernel_code, 0x8E);
    idtSetDescriptor(6, (uint)isrInvalidOpcodeException, kernel_code, 0x8E);
    idtSetDescriptor(7, (uint)isrNoCoprocessorException, kernel_code, 0x8E);
    idtSetDescriptor(8, (uint)isrDoubleFaultException, kernel_code, 0x8E);
    idtSetDescriptor(9, (uint)isrCoprocessorSegmentOverrunException, kernel_code, 0x8E);
    idtSetDescriptor(10, (uint)isrBadTSSException, kernel_code, 0x8E);
    idtSetDescriptor(11, (uint)isrSegmentNotPresentException, kernel_code, 0x8E);
    idtSetDescriptor(12, (uint)isrStackFaultException, kernel_code, 0x8E);
    idtSetDescriptor(13, (uint)isrGeneralProtectionFaultException, kernel_code, 0x8E);
    idtSetDescriptor(14, (uint)isrPageFaultException, kernel_code, 0x8E);
    idtSetDescriptor(15, (uint)isrUnknownInterruptException, kernel_code, 0x8E);
    idtSetDescriptor(16, (uint)isrCoprocessorFaultException, kernel_code, 0x8E);
    idtSetDescriptor(17, (uint)isrAlignmentCheckException, kernel_code, 0x8E);
    idtSetDescriptor(18, (uint)isrMachineCheckException, kernel_code, 0x8E);

    for (int i = 19; i < 32; i++) {
        idtSetDescriptor(i, (uint)isrNonExistent, kernel_code, 0x8E);
    }
    idt_ptr[0] = (sizeof (idtGateDescriptor) * IDT_SIZE) + (((ulong)idt & 0xffff) << 16);
    idt_ptr[1] = (ulong)idt >> 16 ;

    load_idt(idt_ptr);
}

// idtSetDescriptor sets a gate descriptor in the IDT.
void idtSetDescriptor(int index, uint handler, ushort selector, uchar type) {
    idtGateDescriptor* d = &idt[index];

    ushort lowOffset = handler & 0xFFFF;
    ushort highOffset = (handler & 0xFFFF0000) >> 16;

    d->lowOffset = lowOffset;
    d->selector = selector;
    d->type = type;
    d->highOffset = highOffset;
}
