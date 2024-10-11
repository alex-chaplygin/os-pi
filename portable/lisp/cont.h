/// Объект - продолжение
typedef struct {
    jmp_buf buffer;
    object_t enviroment;
} continuation_t;
