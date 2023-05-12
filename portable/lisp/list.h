typedef enum {
    NUMBER,
    ATOM,
    LIST
} type_t;

void list_add(type_t type, void *data);
void print_list();