/**
 * @file   test_comp.c
 * @author alex <alex@alex-home>
 * @date   Thu Apr 10 11:26:19 2025
 * 
 * @brief  Системный тест запуска скомпилированного кода виртуальной машиной
 */

#include <setjmp.h>
#include "../init.c"
#include "objects.h"
#include "alloc.h"
#include "parser.h"
#include "vm.h"

extern jmp_buf repl_buf;
object_t consts;
/// Таблица символов с адресами
object_t symbol_table;

char *itoa(int num, char *str, int rad)
{
    int i = 14;
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

int main()
{
    init_all();
    if (setjmp(repl_buf) == 0) {
    int prog_size = get_value(parse());
    int *prog = alloc_region(prog_size * sizeof(int));
    int *p = prog;
    for (int i = 0; i < prog_size; i++)
	*p++ = get_value(parse());
    consts = parse();
    array_t *const_a = GET_ARRAY(consts);
    int num_vars = get_value(parse());
    symbol_table = parse();
    vm_init(prog, prog_size, const_a->data, const_a->length, num_vars);
	vm_run();
	return 0;
    } else {
#ifndef VM	    
	    vm_dump();
#else
    ;
#endif    
    return 1;
    }
}
