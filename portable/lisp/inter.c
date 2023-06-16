#include "list.h" 
#include "parser.h"
#include "eval.h"

unsigned int hash(char *str);
void print_table();

int main()
{
    init_eval();
    object_t *head = parse();
    print_elem(eval(head));
    return 0;
}
