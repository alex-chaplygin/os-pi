/**
 * @file   keyboard.c
 * @author Pavel <pavel@pavel-VirtualBox>
 * @date   Mon Oct 26 09:32:37 2020
 * 
 * @brief Инициализация клавиатуры  
 * 
 * 
 */
#include <x86/x86.h>
#include <portable/console.h>
#include <portable/types.h>
#include <x86/idt.h>
#include <portable/keyboard.h>
/** 
 * проверка подключения клавиатуры
 * 
 */

extern void a_keyboard_interrupt();

void init_keyboard() {
  uchar data; 
  write_port(0x64, 0xAA);
  data = read_port(0x60);
  if (data == 0x55)
    kprint("Кeyboard is enabled\n");
  else
    kprint("No keyboard\n");

  // Записать дескриптор клавиатуры в IDT.
  idtSetDescriptor(0x21, (uint)a_keyboard_interrupt, kernel_code, INTERRUPT_GATE | DPL0);
  // Включить линию IRQ клавиатуры.
  write_port(0x21, 0xFD);
}

void keyboard_interrupt() {
  uchar status;
  char key_code;

  write_port(0x20, 0x20);

  status = read_port(0x64);

  if (status & 0x01) {
    key_code = read_port(0x60);

    if (key_code >= 0) {
      kprint("Key code: 0x%x\n", key_code);
    }
  }
}


