#define PROTECTED_SIZE 50

/// Структура элемента привязки
typedef struct bind_s
{
    object_t obj; // Хранимый объект
    struct bind_s *next; //Указатель на следующий элемент в списке
} bind_t;

/// Добавить один объект в список защиты protected
#define PROTECT1(o)\
   bind_t p_obj;\
   p_obj.obj = o;\
   p_obj.next = protected;\
   bind_t *old_ped = protected;\
   protected = &p_obj;

/// Добавить 2 объекта в список защиты protected
#define PROTECT2(o1, o2)\
   bind_t p_obj1;\
   bind_t p_obj2;\
   p_obj1.obj = o1;\
   p_obj1.next = &p_obj2;\
   p_obj2.obj = o2;\
   p_obj2.next = protected;\				
   bind_t *old_ped = protected;\
   protected = &p_obj1;


#define UNPROTECT\
   protected = old_ped;

extern bind_t *global_env;
extern bind_t* protected;

void bind_global(object_t symbol);
void set_global(symbol_t *symbol);
