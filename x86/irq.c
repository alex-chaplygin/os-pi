/**
 * @file irq.c
 * @author your name (you@domain.com)
 * @brief Контроллер прерываний
 * @version 0.1
 * @date 2020-10-26
 * 
 * @copyright Copyright (c) 2020
 * 
 */
#include <objects.h>
#include <x86/x86.h>
#include <x86/irq.h>

/**
 * @brief Инициализирует контроллер прерываний
 * 
 * @param offset1 базовый вектор первого контроллера (по умолчанию 20)
 * @param offset2 базовый вектор второго контроллера (по умолчанию 20+8)
 */
void init_pic(int offset1, int offset2)
{
	unsigned char a1, a2;
 
	a1 = ~0; // все запрещаем
	a2 = inb(PIC2_DATA);
 
	outb(PIC1_COMMAND, ICW1_INIT | ICW1_ICW4);  // starts the initialization sequence (in cascade mode)
	outb(PIC2_COMMAND, ICW1_INIT | ICW1_ICW4);

	outb(PIC1_DATA, offset1);                 // ICW2: Master PIC vector offset
	outb(PIC2_DATA, offset2);                 // ICW2: Slave PIC vector offset

	outb(PIC1_DATA, 4);                       // ICW3: tell Master PIC that there is a slave PIC at IRQ2 (0000 0100)

	outb(PIC2_DATA, 2);                       // ICW3: tell Slave PIC its cascade identity (0000 0010)

 
	outb(PIC1_DATA, ICW4_8086);
	outb(PIC2_DATA, ICW4_8086);

	outb(PIC1_DATA, a1);   // установка масок 2х контроллеров прерываний
	outb(PIC2_DATA, a2);
}

/**
 * @brief Вызывается после исполнения обработчика прерывания
 * 
 * @param irq номер обрабатываемого прерывания (0-16) 
 */
void end_of_interrupt(unsigned char irq)
{  
  if (irq >= 8)
    outb(PIC2_COMMAND,PIC_EOI);
  
  outb(PIC1_COMMAND,PIC_EOI);
}

/** 
 * Включает линию обработки прерываний
 * 
 * @param irq номер линии
 */
void enable_irq(byte irq)
{
  int port;
  if (irq >= 8)
    port = PIC2_DATA;
  else 
    port = PIC1_DATA;
  
  byte mask = inb(port);
  mask &= ~(1 << irq);
  outb(port, mask);
}

/** 
 * Запрещение линии прерывания
 * 
 * @param irq номер линии 
 */
void disable_irq(byte irq)
{
  int port;
  if (irq >= 8)
    port = PIC2_DATA;
  else 
    port = PIC1_DATA;

  byte mask = inb(port);
  mask |= (1 << irq);
  outb(port, mask);
}
