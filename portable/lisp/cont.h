/// Объект - продолжение
typedef struct {
    jmp_buf buffer;
    object_t environment;
} continuation_t;
