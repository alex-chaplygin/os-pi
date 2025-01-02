/// Объект - продолжение
typedef struct {
    jmp_buf buffer;
    object_t environment;
    object_t func_environment;
    int last_protected;
} continuation_t;
