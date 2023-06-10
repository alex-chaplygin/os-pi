#include "list.h" 
#include "parser.h"

unsigned int hash(char *str);
void print_table();

int main()
{
    element_t *head = parse();
    print_elem(head);
    return 0;
}