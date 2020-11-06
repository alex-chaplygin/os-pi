
#include "idt.h"
#include "isr.h"

idtGateDescriptor idt[IDT_SIZE];
unsigned long idt_ptr[2];


void idtInit() {
    idtSetDescriptor(0, (uint)a_isrZeroDivisionException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(1, (uint)a_isrDebugException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(2, (uint)a_isrNonMaskableInterruptException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(3, (uint)a_isrBreakpointException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(4, (uint)a_isrIntoDetectedOverflowException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(5, (uint)a_isrOutOfBoundsException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(6, (uint)a_isrInvalidOpcodeException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(7, (uint)a_isrNoCoprocessorException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(8, (uint)a_isrDoubleFaultException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(9, (uint)a_isrCoprocessorSegmentOverrunException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(10, (uint)a_isrBadTSSException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(11, (uint)a_isrSegmentNotPresentException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(12, (uint)a_isrStackFaultException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(13, (uint)a_isrGeneralProtectionFaultException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(14, (uint)a_isrPageFaultException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(15, (uint)a_isrUnknownInterruptException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(16, (uint)a_isrCoprocessorFaultException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(17, (uint)a_isrAlignmentCheckException, kernel_code, INTERRUPT_GATE | DPL0);
    idtSetDescriptor(18, (uint)a_isrMachineCheckException, kernel_code, INTERRUPT_GATE | DPL0);
    

    for (int i = 19; i < 32; i++) {
        idtSetDescriptor(i, (uint)a_isrNonExistent, kernel_code, INTERRUPT_GATE | DPL0);
    }
    
    idtSetDescriptor(0x80, (uint)a_syscall, kernel_code, INTERRUPT_GATE | DPL0);
    
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
    d->type = 0x80 | type;
    d->highOffset = highOffset;
}

