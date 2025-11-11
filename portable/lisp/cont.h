/// Объект - продолжение
typedef struct {
    jmp_buf buffer;		/**< Сохраненная точка перехода */
    object_t environment;	/**< Сохраненное окружение переменных */
    object_t func_environment;	/**< Сохраненное окружение функций */
    int last_protected;		/**< Сохраненное число временных объектов */
} continuation_t;

/// Объект - точка возврата с меткой
typedef struct {
    continuation_t buff; /**< Объект - продолжение */
    object_t tag; /**< Сохраненная метка*/
} catch_t;
