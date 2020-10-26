/**
 * @file irq.c
 * @author your name (you@domain.com)
 * @brief 
 * @version 0.1
 * @date 2020-10-26
 * 
 * @copyright Copyright (c) 2020
 * 
 */
#include "types.h"
#include "irq.h"

/**
 * @brief Инициализирует контроллер прерываний
 * 
 * @param offset1 адрес первого контроллера (по умолчанию 20)
 * @param offset2 адрес второго контроллера (по умолчанию 20+8)
 */
void init_interrupts(int offset1, int offset2)
{
	unsigned char a1, a2;
 
	a1 = read_port(PIC1_DATA);                        // save masks
	a2 = read_port(PIC2_DATA);
 
	write_port(PIC1_COMMAND, ICW1_INIT | ICW1_ICW4);  // starts the initialization sequence (in cascade mode)
	write_port(PIC2_COMMAND, ICW1_INIT | ICW1_ICW4);

	write_port(PIC1_DATA, offset1);                 // ICW2: Master PIC vector offset
	write_port(PIC2_DATA, offset2);                 // ICW2: Slave PIC vector offset

	write_port(PIC1_DATA, 4);                       // ICW3: tell Master PIC that there is a slave PIC at IRQ2 (0000 0100)

	write_port(PIC2_DATA, 2);                       // ICW3: tell Slave PIC its cascade identity (0000 0010)

 
	write_port(PIC1_DATA, ICW4_8086);
	write_port(PIC2_DATA, ICW4_8086);

	//write_port(PIC1_DATA, a1);   // restore saved masks.
	//write_port(PIC2_DATA, a2);
}

/**
 * @brief Вызывается после исполнения обработчика прерывания
 * 
 * @param irq номер обрабатываемого прерывания (0-16) 
 */
void end_of_interrupt(unsigned char irq)
{
	if(irq >= 8)
		write_port(PIC2_COMMAND,PIC_EOI);
 
	write_port(PIC1_COMMAND,PIC_EOI);
}