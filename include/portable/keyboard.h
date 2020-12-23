#define MAX_KEYBUFFER 128

#define PAGE_UP_CODE 0x49
#define PAGE_DOWN_CODE 0x51
#define ENTER_CODE 0x1C

void init_keyboard();
int read_char(char* c);
void keyboard_interrupt();
char key_map(int scan_code);
