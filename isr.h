/*********************************************
 * Entries 0-31 of IDT are reserved for pro- *
 * cessor exceptions, and first 19 of them   *
 * must be handled by the functions declared *
 * below.                                    *
 *********************************************/
void exception_handler(int num);
void isrZeroDivisionException();
void isrDebugException();
void isrNonMaskableInterruptException();
void isrBreakpointException();
void isrIntoDetectedOverflowException();
void isrOutOfBoundsException();
void isrInvalidOpcodeException();
void isrNoCoprocessorException();
void isrDoubleFaultException();
void isrCoprocessorSegmentOverrunException();
void isrBadTSSException();
void isrSegmentNotPresentException();
void isrStackFaultException();
void isrPageFaultException();
void isrUnknownInterruptException();
void isrCoprocessorFaultException();
void isrAlignmentCheckException();
void isrMachineCheckException();
void isrNonExistent();
