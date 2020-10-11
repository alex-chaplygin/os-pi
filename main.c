#include "console.h"
#include "idt.h"
#include "libc.h"
/* video memory begins at address 0xb8000 */


void kmain(void)
{
  idtInit();
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
