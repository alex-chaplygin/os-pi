#include <portable/types.h>

#define CONSOLE_ROWS 25		/**< Количество строк на экране */
#define CONSOLE_COLS 80		/**< Количество столбцов на экране */
#define SCREEN_NUMBER 10	/**< Количество экранов */
#define BUFFER_SIZE SCREEN_NUMBER*CONSOLE_COLS*CONSOLE_ROWS*2 /**< Размер буфера */
#define SHIFT 12*2*CONSOLE_COLS		/**< Строки для сдвига */

/// Копирует экран из буфера в видеопамять
void copy_to_screen();

/// Переключает экран на предыдущий
void screen_up();

/// Переключает экран на следующий
void screen_down();

void putchar(char c);

void console_clear();

/** 
 * @brief Функция включения указателя
 * 
 * @param start Начальная строка
 * @param end Конечная строка
 */
void enable_cursor(byte start, byte end);

/** 
 * @brief Функция выключения курсора
 * 
 */
void disable_cursor();

/** 
 * @brief Функция перемещения курсора
 * 
 * @param pos Позиция, в которую курсор передвигается
 */
void move_cursor(int pos);

void backspace();
