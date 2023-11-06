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

extern token_t token;

int main()
{
    init_eval();
    init_arith();
	init_strings();
    init_regions();
    init_arrays();
    do {
	object_t *o = parse();
        //printf("parse: "); PRINT(o);
	if (o != ERROR) {
	    object_t *res = eval(o, NULL);
	    //printf("res: "); PRINT(res);
	    if (res != ERROR) {
		print_obj(res);
	    }
	    printf("\n");
	}
	garbage_collect();
    } while (token.type != END);
    return 0;
}

