#define PROTECTED_SIZE 1000
#define MAX_GLOBALS 10000

/// Структура элемента привязки
typedef struct bind_s
{
    object_t obj; // Хранимый объект
    struct bind_s *next; //Указатель на следующий элемент в списке
} bind_t;

/// Структура для защиты переменной временного объекта
typedef struct temp_bind_s
{
    object_t *obj; // Указатель на переменную
    struct temp_bind_s *next; // Указатель на следующий элемент в списке
} temp_bind_t;

#define PRINTPROT\
    temp_bind_t *pprot = protected;\
    printf("protected list: count = %d\n", last_protected);	\
    for (int i = 0; i < last_protected; i++, pprot++) {		\
    printf("%d: ", i);\
	PRINT(*(pprot->obj));					\
    }\

/// Добавить один объект в список защиты protected
#define PROTECT1(o)\
   int old_ped = last_protected;		\
   temp_bind_t *p_obj = &protected[last_protected++];\
   p_obj->obj = &o;\
   //   PRINTPROT

/// Добавить 2 объекта в список защиты protected
#define PROTECT2(o1, o2)\
   int old_ped = last_protected;		\
   temp_bind_t *p_obj = &protected[last_protected++];\
   p_obj->obj = &o1;\
   p_obj = &protected[last_protected++];\
   p_obj->obj = &o2;\
   //PRINTPROT

/// Добавить 3 объекта в список защиты protected
#define PROTECT3(o1, o2, o3)			\
   int old_ped = last_protected;		\
   temp_bind_t *p_obj = &protected[last_protected++];\
   p_obj->obj = &o1;\
   p_obj = &protected[last_protected++];\
   p_obj->obj = &o2;\
   p_obj = &protected[last_protected++];\
   p_obj->obj = &o3;\
   //PRINTPROT

/// Добавить 3 объекта в список защиты protected
#define PROTECT4(o1, o2, o3, o4)			\
   int old_ped = last_protected;		\
   temp_bind_t *p_obj = &protected[last_protected++];\
   p_obj->obj = &o1;\
   p_obj = &protected[last_protected++];\
   p_obj->obj = &o2;\
   p_obj = &protected[last_protected++];\
   p_obj->obj = &o3;\
   p_obj = &protected[last_protected++];\
   p_obj->obj = &o4;\
   //PRINTPROT

#define UNPROTECT\
    last_protected = old_ped;

extern bind_t global_env[];
extern temp_bind_t protected[];
extern int last_protected;

void bind_global(object_t symbol);
void set_global(symbol_t *symbol);
