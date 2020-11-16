#define CONSOLE_ROWS 25
#define CONSOLE_COLS 80

void putchar(char c);

void kprint(char *str, ...);

void console_clear();

void scrollConsole(int n);
