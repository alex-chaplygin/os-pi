#include "list.h" 
#include "parser.h"

unsigned int hash(char *str);
void print_table();

int main()
{
    list_t *head = parse();
    print_list(head);
    //    print_table(); 
    return 0;
}
