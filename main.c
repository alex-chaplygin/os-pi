#include "console.h"
#include "idt.h"
#include "libc.h"
#include "timer.h"
#include "mouse.h"
/* video memory begins at address 0xb8000 */

extern void test_syscall();

void kmain(void)
{
  idtInit();
  //init_timer(1000);
  console_clear();
  // int a = 1 / 0;
  //kprint("qwerty\n");
  //kprint("ASDFGHJK\n");
  //kprint("Azxcvbn\n");

  for (int i = 0; i < 60; i++){
    kprint(intToStr(i));
    kprint("\n");
  }
  
  kprint(int_to_str_hex(255));

  if (mouseCheck() == 0xAA) {
    kprint("Mouse detected\n");
  } else {
    kprint("No mouse\n");
  }
  

  //test_syscall();
  //while(1);
}
