#include "console.h"
#include "idt.h"
#include "libc.h"
#include "timer.h"
/* video memory begins at address 0xb8000 */


void kmain(void)
{
  idtInit();
  init_timer(1000);
  console_clear();
  // int a = 1 / 0;
  //kprint("qwerty\n");
  //kprint("ASDFGHJK\n");
  //kprint("Azxcvbn\n");

  for (int i = 0; i < 30; i++){
    kprint(intToStr(i));
    kprint("\n");
  }

  while(1);
}
