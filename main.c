#include "console.h"
#include "idt.h"
/* video memory begins at address 0xb8000 */


void kmain(void)
{
  idtInit();
  console_clear();
  int a = 1 / 0;
  kprint("TEST\n");
  for (int i = 0; i < 22; i++){
    kprint("123456789\n");
  }
	while(1);
}
