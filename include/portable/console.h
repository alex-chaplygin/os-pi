#define CONSOLE_ROWS 25
#define CONSOLE_COLS 80
#define SCREEN_NUMBER 10
#define SCREEN_BUFFER_SIZE SCREEN_NUMBER*CONSOLE_COLS*CONSOLE_ROWS*2

void putchar(char c);

void kprint(char *str, ...);

void console_clear();

void scrollConsole(int n);
