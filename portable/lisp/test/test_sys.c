#include <stdio.h>
#include <string.h>
#include <unistd.h>
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
#include <x86/x86.h>

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
void error(char *str)
{
    printf("%s\n", str);
    longjmp(jmp_env, 1);
}

int main()
{
    init_all();
    
    int jmp_code = setjmp(jmp_env);
    if (jmp_code == 1)
        return jmp_code;
    do {
	object_t *o = parse();
        //printf("parse: "); PRINT(o);
	if (o != ERROR) {
	    object_t *res = eval(o, NULL);
	    //printf("res: "); PRINT(res);
	    if (res != ERROR) {
		print_counter++;
		print_obj(res);
	    }
	    printf("\n");
	}
	garbage_collect();
    } while (token.type != END);
    return 0;
}

