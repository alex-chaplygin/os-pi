#include "console.h"
/* video memory begins at address 0xb8000 */


void kmain(void)
{
/*  int pos = 0;
  for (int i = 0; i < 80 * 25; i++) {
    videoptr[pos++] = '#';
    videoptr[pos++] = i % 0xff;
  }*/
  console_clear();
  kprint("TEST\n");
  for (int i = 0; i < 22; i++){
    kprint("123456789\n");
  }
	while(1);
}
