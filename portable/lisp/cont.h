/// Объект - продолжение
typedef struct {
    jmp_buf buffer;
    object_t environment;
    object_t func_environment;
} continuation_t;
