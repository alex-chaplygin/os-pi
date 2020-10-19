#include "console.h"
#include "idt.h"
#include "libc.h"
/* video memory begins at address 0xb8000 */

extern void test_syscall();

void kmain(void)
{
  idtInit();
  console_clear();
  // int a = 1 / 0;
  //kprint("qwerty\n");
  //kprint("ASDFGHJK\n");
  //kprint("Azxcvbn\n");

  for (int i = 0; i < 60; i++){
    kprint(intToStr(i));
    kprint("\n");
  }

  test_syscall();
  while(1);
}
