#include "types.h"

#define IDT_SIZE 256
/// DPL
#define DPL0 0
#define DPL1 (1 << 5)
#define DPL2 (2 << 5)
#define DPL3 (3 << 5)
/// IDT TYPE
#define CALL_GATE_286 0x4
#define TASK_GATE_286 0x5
#define INTERRUPT_GATE_286 0x6
#define TRAP_GATE_286 0x7
#define CALL_GATE 0xC
#define TASK_GATE 0xD
#define INTERRUPT_GATE 0xE
#define TRAP_GATE 0xF

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

extern void a_isrZeroDivisionException();
extern void  a_isrDebugException();
extern void  a_isrNonMaskableInterruptException();
extern void  a_isrBreakpointException();
extern void  a_isrIntoDetectedOverflowException();
extern void  a_isrOutOfBoundsException();
extern void  a_isrInvalidOpcodeException();
extern void  a_isrNoCoprocessorException();
extern void  a_isrDoubleFaultException();
extern void  a_isrCoprocessorSegmentOverrunException();
extern void  a_isrBadTSSException();
extern void  a_isrSegmentNotPresentException();
extern void  a_isrStackFaultException();
extern void  a_isrGeneralProtectionFaultException();
extern void  a_isrPageFaultException();
extern void  a_isrUnknownInterruptException();
extern void  a_isrCoprocessorFaultException();
extern void  a_isrAlignmentCheckException();
extern void  a_isrMachineCheckException();
extern void  a_isrNonExistent();
extern void a_syscall();

