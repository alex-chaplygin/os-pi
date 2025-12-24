#define MAX_CONT_STACK (1024 * 16)

/// Объект - продолжение
typedef struct continuation_s {
    jmp_buf buffer;		/**< Сохраненная точка перехода */
    object_t environment;	/**< Сохраненное окружение переменных */
    object_t func_environment;	/**< Сохраненное окружение функций */
    int last_protected;		/**< Сохраненное число временных объектов */
    unsigned char stack[MAX_CONT_STACK]; /**< Сохраненный стек */
    int size;			/**< Размер стека */
    struct continuation_s *next;
    int free; // Если 1 - продолжение свободно
    int pad[3]; 
} continuation_t;

/// Объект - точка возврата с меткой
typedef struct {
    continuation_t buff; /**< Объект - продолжение */
    object_t tag; /**< Сохраненная метка*/
} catch_t;

#define GET_CONTINUATION(obj) ((continuation_t *)(GET_ADDR(obj)))

object_t new_continuation(jmp_buf buf);
void free_continuation(continuation_t *c);
