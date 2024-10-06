#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <unistd.h>
#include <stdarg.h>
#include "lexer.h"
#include "alloc.h"
#include "objects.h"
#include "eval.h"
#include "test.h"
#include "parser.h"
#include "arith.h"
#include "str.h"
#include "array.h"
#include "pair.h"
#include "predicates.h"
#include "../init.c"

extern token_t token;

// точка начала цикла REPL
extern jmp_buf repl_buf;

char *itoa(int num, char *str, int rad)
{
    int i = 15;
    int neg = 0;
    str[i - 1] = 0;
    char *p = &str[i - 1];
    if (num == 0)
	    *--p = '0';
    if (num < 0) {
        neg = 1;
        num *= -1;
    }
    while (num > 0) {
        int currchar = num % rad;
        p--;
        *p = '0' + currchar;
        num = num / rad;
    }
    if (neg)
        *--p = '-';
    return p;
}

// Вывод сообщения об ошибке и выход из программы
// str - сообщение об ошибке
void error(char *str, ...)
{
    if (str[0] != '\0') {
        va_list vals;
        va_start(vals, str);
        vprintf(str, vals);
        va_end(vals);
        putchar('\n');
    }
    longjmp(repl_buf, 1);
}

int main()
{
    init_all();
    
    do {
	if (setjmp(repl_buf) == 0) {
	    object_t o = parse();
	    if (o == NOVALUE)
		longjmp(repl_buf, 1);
	    //printf("parse: "); PRINT(o);
	    object_t res = eval(o, NULLOBJ, NULLOBJ);
	    //printf("res: "); PRINT(res);
#ifdef REPL
	    PRINT(res);
#endif
	}
    } while (token.type != END);
    return 0;
}

