#define MAX_STR 50

typedef struct atom_s
{
    char str[MAX_STR];
    struct atom_s *next;
} atom_t;

atom_t *find_atom(char *str);
