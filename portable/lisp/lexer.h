#define MAX_STR 500
#define MAX_SYMBOL 25

typedef enum {
    T_NUMBER, // целое число
    T_FLOAT, // вещественное число
    T_SYMBOL, // строка +a4s6d7fd23
    LPAREN, //левая скобка (
    RPAREN, // правая скобка )
    END, // конец потока
    QUOTE, // символ '
    BACKQUOTE, // символ `
    COMMA, // символ ,
    COMMA_AT, // символ ,@
    T_STRING, // строка в кавычках
    SHARP, // символ #
    DOT,// точка .
    INVALID // неизвестный объект
} tokentype_t; //тип ликсемы 

typedef struct {
    tokentype_t type; // тип ликсемы
    int value; // значение числа
    char str[MAX_STR]; //значение строки 
} token_t; // ликсема

token_t *get_token();
void print_token(token_t *token);
void reset_buffer();

extern char *boot_code;
extern int boot_load;
