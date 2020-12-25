/**
 * @file   keyboard.c
 * @author Pavel <pavel@pavel-VirtualBox>
 * @author zergon321 <maximgradan@gmail.com>
 * @date   Mon Oct 26 09:32:37 2020
 * 
 * @brief Инициализация клавиатуры  
 * 
 * 
 */
#include <x86/x86.h>
#include <portable/libc.h>
#include <portable/types.h>
#include <x86/idt.h>
#include <portable/keyboard.h>
#include <portable/proc.h>

/** 
 * проверка подключения клавиатуры
 * 
 */

extern void a_keyboard_interrupt();

/**
 * @brief Таблица соответствия скан-кодов
 * клавиш печатным символам.
 * 
 */
byte keyboard_map[128] =
{
    0,  27, '1', '2', '3', '4', '5', '6', '7', '8',	/* 9 */
  '9', '0', '-', '=', '\b',	/* Backspace */
  '\t',			/* Tab */
  'q', 'w', 'e', 'r',	/* 19 */
  't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',	/* Enter key */
    0,			/* 29   - Control */
  'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',	/* 39 */
 '\'', '`',   0,		/* Left shift */
 '\\', 'z', 'x', 'c', 'v', 'b', 'n',			/* 49 */
  'm', ',', '.', '/',   0,				/* Right shift */
  '*',
    0,	/* Alt */
  ' ',	/* Space bar */
    0,	/* Caps lock */
    0,	/* 59 - F1 key ... > */
    0,   0,   0,   0,   0,   0,   0,   0,
    0,	/* < ... F10 */
    0,	/* 69 - Num lock*/
    0,	/* Scroll Lock */
    0,	/* Home key */
    0,	/* Up Arrow */
    0,	/* Page Up */
  '-',
    0,	/* Left Arrow */
    0,
    0,	/* Right Arrow */
  '+',
    0,	/* 79 - End key*/
    0,	/* Down Arrow */
    0,	/* Page Down */
    0,	/* Insert Key */
    0,	/* Delete Key */
    0,   0,   0,
    0,	/* F11 Key */
    0,	/* F12 Key */
    0,	/* All other keys are undefined */
};

/**
 * @brief Буфер клавиатуры, в который записываются
 * скан-коды нажатых клавиш.
 * 
 */
byte keyboard_buffer[MAX_KEYBUFFER];
/**
 * @brief Позиция в буфере клавиатуры.
 * При заполнении сбрасывается в 0.
 * 
 */
int keybuffer_pos = 0;
/**
 * @brief Позиция чтения буфера клавиатуры.
 * При достижении конца буфера сбрасывается в 0.
 * 
 */
int keybuffer_read_pos = 0;

/**
 * @brief Инициализирует клавиатуру, проверяя
 * её наличие в системе, назначая системное
 * прерывание для обработки нажатия клавиши
 * и включая линию IRQ клавиатуры.
 *
 * 
 */
void init_keyboard() {
  clear_buffer(keyboard_buffer, 0, MAX_KEYBUFFER);
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
  //  write_port(0x21, 0xFD);
  enable_irq(1);
}

/**
 * @brief Возвращает соответствующий
 * скан-коду клавиши печатный символ.
 * 
 * @param scan_code скан-код нажатой клавиши.
 * @return char - печатный символ.
 */
char key_map(int scan_code) {
  if (scan_code > 127 || scan_code < 0) {
    return 0;
  }

  return keyboard_map[scan_code];
}

/**
 * @brief Считывает символ из буфера клавиатуры.
 * Блокирует вызов и погружает текущий процесс в сон,
 * если буфер пуст.
 * 
 * @param c адрес, по которому необходимо записать считанный символ.
 * @return int - текущая позиция чтения буфера клавиатуры.
 */
int read_char(char* c) {
  // Если буфер скан-кодов пуст.
  if (keyboard_buffer[keybuffer_read_pos] == '\0') {
    // Погрузить текущий процесс в сон.
    sleep(SLEEP_KEYBOARD, &&jump);
    // Блокировать вызов, пока в буфер не поступит значение.
    while (keyboard_buffer[keybuffer_read_pos] == '\0') {
      jump:;
    }
  }

  *c = key_map(keyboard_buffer[keybuffer_read_pos++]);

  return keybuffer_read_pos;
}

/**
 * @brief Обрабатывает нажатие клавиши
 * и записывает её скан-код в буфер.
 * 
 */
void keyboard_interrupt() {
  uchar status;
  char key_code;

  //write_port(0x20, 0x20);

  status = read_port(0x64);

  if (status & 0x01) {
    key_code = read_port(0x60);

    if (key_code >= 0) {
      keyboard_buffer[keybuffer_pos++] = key_code;
      wakeup(SLEEP_KEYBOARD);

      if (keybuffer_pos > MAX_KEYBUFFER) {
        keybuffer_pos = 0;
        // При заполнении буфера очистить его до позиции чтения.
        clear_buffer(keyboard_buffer, 0, keybuffer_read_pos);
      }

      if (key_code == PAGE_UP_CODE) {
        screen_up();
        return;
      }

      if (key_code == PAGE_DOWN_CODE) {
        screen_down();
        return;
      }

      char symbol = key_map(key_code);

      if (key_code == ENTER_CODE) {
        symbol = '\n';
      }

      if (symbol != '\0') {
        char str[2] = {symbol, '\0'};

        kprint(str);
      }
    }
  }
}


