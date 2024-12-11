#define PROTECTED_SIZE 50

/// Структура элемента привязки
typedef struct bind_s
{
    object_t obj; // Хранимый объект
    struct bind_s *next; //Указатель на следующий элемент в списке
} bind_t;

extern bind_t *global_env;

/// Добавить одну переменную в список защиты
#define PROTECT1(var)

#define UNPROTECT

void bind_global(object_t symbol);

void set_global(symbol_t *symbol);

