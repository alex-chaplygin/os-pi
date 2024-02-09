#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <unistd.h>
#include <stdarg.h>
#include "lexer.h"
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

// состояние стека
jmp_buf jmp_env;

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
    longjmp(jmp_env, 1);
}

int main()
{
    init_all();
    
    //    int jmp_code = setjmp(jmp_env);
    //    if (jmp_code == 1)
    //        return jmp_code;
    do {
	if (setjmp(jmp_env) == 0) {
	    object_t *o = parse();
	    if (o == NOVALUE)
		longjmp(jmp_env, 1);
	    //printf("parse: "); PRINT(o);
	    object_t *res = eval(o, NULL);
	    //printf("res: "); PRINT(res);
	    print_counter++;
	    PRINT(res);
	}
	garbage_collect();
    } while (token.type != END);
    return 0;
}

