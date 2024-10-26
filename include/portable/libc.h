#include <portable/types.h>

typedef char *va_list;		/**< Тип - строка из параметров */
#define NULL 0
#define EOF (-1)		/**< Конец потока */
#define MANTISSA_BITS 23	/**< Число бит в мантиссе float */
/// Инициализация параметров
#define va_start(cur_arg, args) *((char *)(cur_arg = (char *)&args))
/// Извлечение параметра
#define va_arg(cur_arg, type) *((type *)(cur_arg += sizeof(type)))
/// Конец обработки параметров
#define va_end(cur_arg) cur_arg = NULL

void memcpy(void* destptr, const void* srcptr, unsigned int size);
void memset(void *buf, int val, int count);
void vprintf(char *format, va_list args);
void printf(char *str, ...);
void clear_buffer(byte* buffer, int offset, int count);
int strcmp(char *str_one, char *str_two);
void strcpy(char *str1, char *str2);
int strlen(char *str);
char toupper(char ch);
