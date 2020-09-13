/* video memory begins at address 0xb8000 */
char *videoptr = (char*)0xb8000;

void kmain(void)
{
  int pos = 0;
  for (int i = 0; i < 80 * 25; i++) {
    videoptr[pos++] = '#';
    videoptr[pos++] = i % 0xff;
  }

	while(1);
}
