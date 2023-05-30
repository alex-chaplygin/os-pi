#include "list.h" 
#include "parser.h"

int main()
{
    list_t *head = parse();
    print_list(head);

    return 0;
}
