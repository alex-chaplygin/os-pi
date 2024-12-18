#include <stdio.h>
#include <setjmp.h>
#include <stdarg.h>
#include "objects.h"
#include "eval.h"

// точка начала цикла REPL
extern jmp_buf repl_buf;

void print_debug_lines();

/** 
 * Вывод сообщения об ошибке, трассировка стека.
 *
 * @param str форматная строка "%d %x"
 */
void error(char *str, ...)
{
    va_list vals;
    va_start(vals, str);
#ifdef OS
    void set_cursor(int x, int y);
    set_cursor(0, 0);
#endif
    if (str[0] != '\0') {
        vprintf(str, vals);
        va_end(vals);
        putchar('\n');
#ifdef DEBUG
	print_debug_stack();
        debug_stack = NULLOBJ;
	extern object_t current_env;
	printf("env: ");
	PRINT(current_env);
#endif
    }
    longjmp(repl_buf, 1);
}

/**
 * Вывод сообщения об ошибке для парсера и лексера
 * 
 * @param str строка с сообщением об ошибке
 */
void parser_error(char *str, ...)
{
    va_list(vals);
    va_start(vals, str);
    print_debug_lines();
    if (str[0] != '\0') {
	vprintf(str, vals);
	va_end(vals);
	putchar('\n');
    }
    longjmp(repl_buf,1);
}
