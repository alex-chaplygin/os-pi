#include "console.h"

void isrZeroDivisionException(){ kprint("division by zero"); }
void isrDebugException(){}
void isrNonMaskableInterruptException(){}
void isrBreakpointException(){}
void isrIntoDetectedOverflowException(){}
void isrOutOfBoundsException(){}
void isrInvalidOpcodeException(){}
void isrNoCoprocessorException(){}
void isrDoubleFaultException(){}
void isrCoprocessorSegmentOverrunException(){}
void isrBadTSSException(){}
void isrSegmentNotPresentException(){}
void isrStackFaultException(){}
void isrGeneralProtectionFaultException(){}
void isrPageFaultException(){}
void isrUnknownInterruptException(){}
void isrCoprocessorFaultException(){}
void isrAlignmentCheckException(){}
void isrMachineCheckException(){}
void isrNonExistent(){}