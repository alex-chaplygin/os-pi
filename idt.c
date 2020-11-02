/**
 * @file   idt.c
 * @author alex <alex@alex-Inspiron-N5040>
 * @date   Mon Oct 19 13:10:13 2020
 * 
 * @brief  Установка таблицы IDT - дескриптеров прерываний
 * 
 * 
 */

#include "idt.h"
#include "isr.h"
#include "x86.h"

/// таблица дескриптеров прерываний
idtGateDescriptor idt[IDT_SIZE];

/** 
 * idtInit Инициализирует таблицу дескриптеров прерываний, устанавливает обработчики исключений
 * 
 */
void idtInit() {
  unsigned long idt_ptr[2];
    idtSetDescriptor(0, (uint)a_isrZeroDivisionException, kernel_code, 0x8E);
    idtSetDescriptor(1, (uint)a_isrDebugException, kernel_code, 0x8E);
    idtSetDescriptor(2, (uint)a_isrNonMaskableInterruptException, kernel_code, 0x8E);
    idtSetDescriptor(3, (uint)a_isrBreakpointException, kernel_code, 0x8E);
    idtSetDescriptor(4, (uint)a_isrIntoDetectedOverflowException, kernel_code, 0x8E);
    idtSetDescriptor(5, (uint)a_isrOutOfBoundsException, kernel_code, 0x8E);
    idtSetDescriptor(6, (uint)a_isrInvalidOpcodeException, kernel_code, 0x8E);
    idtSetDescriptor(7, (uint)a_isrNoCoprocessorException, kernel_code, 0x8E);
    idtSetDescriptor(8, (uint)a_isrDoubleFaultException, kernel_code, 0x8E);
    idtSetDescriptor(9, (uint)a_isrCoprocessorSegmentOverrunException, kernel_code, 0x8E);
    idtSetDescriptor(10, (uint)a_isrBadTSSException, kernel_code, 0x8E);
    idtSetDescriptor(11, (uint)a_isrSegmentNotPresentException, kernel_code, 0x8E);
    idtSetDescriptor(12, (uint)a_isrStackFaultException, kernel_code, 0x8E);
    idtSetDescriptor(13, (uint)a_isrGeneralProtectionFaultException, kernel_code, 0x8E);
    idtSetDescriptor(14, (uint)a_isrPageFaultException, kernel_code, 0x8E);
    idtSetDescriptor(15, (uint)a_isrUnknownInterruptException, kernel_code, 0x8E);
    idtSetDescriptor(16, (uint)a_isrCoprocessorFaultException, kernel_code, 0x8E);
    idtSetDescriptor(17, (uint)a_isrAlignmentCheckException, kernel_code, 0x8E);
    idtSetDescriptor(18, (uint)a_isrMachineCheckException, kernel_code, 0x8E);
    

    for (int i = 19; i < 32; i++) {
        idtSetDescriptor(i, (uint)a_isrNonExistent, kernel_code, 0x8E);
    }
    
    idtSetDescriptor(0x20, (uint)a_timer, kernel_code, 0x8E);
    idtSetDescriptor(0x80, (uint)a_syscall, kernel_code, 0x8E);
    
    idt_ptr[0] = (sizeof (idtGateDescriptor) * IDT_SIZE) + (((ulong)idt & 0xffff) << 16);
    idt_ptr[1] = (ulong)idt >> 16 ;

    load_idt(idt_ptr);
}

/** 
 *  idtSetDescriptor Установка обработчика прерывания
 * 
 * @param index  вектор прерывания
 * @param handler адрес обработчика
 * @param selector селектор сегмента, где находится обработчик
 * @param type флаги
 */
void idtSetDescriptor(int index, uint handler, ushort selector, uchar type) {
    idtGateDescriptor* d = &idt[index];

    ushort lowOffset = handler & 0xFFFF;
    ushort highOffset = (handler & 0xFFFF0000) >> 16;

    d->lowOffset = lowOffset;
    d->selector = selector;
    d->type = type;
    d->highOffset = highOffset;
}

