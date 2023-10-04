#define MAX_STR 50

typedef enum {
    T_NUMBER, //число
    T_SYMBOL, // строка +a4s6d7fd23
    LPAREN, //левая скобка (
    RPAREN, // правая скобка )
    END, // конец строки
    QUOTE, // символ '
    T_STRING, // строка в кавычках
    INVALID // неизвестный объект
} tokentype_t; //тип ликсемы 

typedef struct {
    tokentype_t type; // тип ликсемы
    int value; // значение числа
    char str[MAX_STR]; //значение строки 
} token_t; // ликсема

token_t *get_token();
void print_token(token_t *token);
