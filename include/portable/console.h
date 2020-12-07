#define CONSOLE_ROWS 25		/**< Количество строк на экране */
#define CONSOLE_COLS 80		/**< Количество столбцов на экране */
#define SCREEN_NUMBER 10	/**< Количество экранов */
#define BUFFER_SIZE SCREEN_NUMBER*CONSOLE_COLS*CONSOLE_ROWS*2 /**< Размер буфера */
#define SHIFT 12		/**< Строки для сдвига */

/// Копирует экран из буфера в видеопамять
void copy_to_screen();

/// Переключает экран на следующий
void screen_up();

/// Переключает экран на предыдущий
void screen_down();

void putchar(char c);

void kprint(char *str, ...);

void console_clear();


