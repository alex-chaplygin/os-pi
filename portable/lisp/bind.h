#define MAX_STATIC 10000	/**< размер массива статических привязок */
#define MAX_PROTECTED 1000	/**< размер массива временных объектов */
/// Печать массива текущих временных объектов
#define PRINTPROT\
    object_t **pprot = protected;\
    printf("protected list: count = %d\n", last_protected);    \
    for (int i = 0; i < last_protected; i++, pprot++) {                \
    printf("%d: ", i);\
       PRINT(**pprot);                                   \
    }\

/// Добавить один объект в список защиты protected
#define PROTECT1(o)\
   int old_ped = last_protected;               \
   object_t **p_obj = &protected[last_protected++];\
   *p_obj = &o;\
   //   PRINTPROT

/// Добавить 2 объекта в список защиты protected
#define PROTECT2(o1, o2)\
   int old_ped = last_protected;               \
   object_t **p_obj = &protected[last_protected++];\
   *p_obj = &o1;\
   p_obj = &protected[last_protected++];\
   *p_obj = &o2;\
   //PRINTPROT

/// Добавить 3 объекта в список защиты protected
#define PROTECT3(o1, o2, o3)                   \
   int old_ped = last_protected;               \
   object_t **p_obj = &protected[last_protected++];\
   *p_obj = &o1;\
   p_obj = &protected[last_protected++];\
   *p_obj = &o2;\
   p_obj = &protected[last_protected++];\
   *p_obj = &o3;\
   //PRINTPROT

/// Откатить массив защиты назад
#define UNPROTECT\
    last_protected = old_ped;

extern object_t static_bind[];
extern object_t *protected[];
extern int last_static;
extern int last_protected;

void bind_static(object_t symbol);
void set_global(symbol_t *symbol);
