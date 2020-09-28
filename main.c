#include "console.h"
#include "libc.h"
/* video memory begins at address 0xb8000 */


void kmain(void)
{
/*  int pos = 0;
  for (int i = 0; i < 80 * 25; i++) {
    videoptr[pos++] = '#';
    videoptr[pos++] = i % 0xff;
  }*/
  console_clear();
  //kprint("qwerty\n");
  //kprint("ASDFGHJK\n");
  //kprint("Azxcvbn\n");

  for (int i = 0; i < 30; i++){
    kprint(intToStr(i));
    kprint("\n");
  }

	while(1);
}
