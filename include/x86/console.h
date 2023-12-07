#include <portable/types.h>

#define CONSOLE_ROWS 25		/**< Количество строк на экране */
#define CONSOLE_COLS 80		/**< Количество столбцов на экране */

void putchar(char c);
void console_clear();
void enable_cursor(byte start, byte end);
void disable_cursor();
void set_cursor(int x, int y);
void set_color(int col);
void set_back_color(int col);
