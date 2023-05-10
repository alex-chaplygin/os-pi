#include <stdio.h>
#include <stdlib.h>

#define MAX_NUM 100
#define MAX_STR 50

typedef enum {
    T_NUMBER, //
    T_ATOM, // +a4s6d7fd23
    LPAREN,
    RPAREN,
    END,
    INVALID
} token_t;

typedef enum {
    NUMBER,
    ATOM,
    LIST
} type_t;

typedef struct list_s
{
    type_t type; // тип элемента списка
    union {
        int value; // если элемент число, то его значение
        char atom[MAX_STR]; // если элемент атом, то строка атома
        struct list_s *list;
    } u;
    struct list_s *next;
} list_t;

char cur_symbol;
int current_number; // глобальная переменная для текущего числа
char cur_str[MAX_STR]; // текущий атом
int flag = 0; // если true, не считывать символ
int numbers[MAX_NUM]; // создаем массив для хранения чисел
int count = 0; // счетчик чисел в массиве
list_t numbers_heap[MAX_NUM];
list_t *head = NULL;//глобальный список
list_t *cur_list;//текущий список куда добавляются элементы

list_t *alloc()
{
    return &numbers_heap[count++];
}

void get_cur_char()
{
    if (flag)
        flag = 0;
    else
        cur_symbol = getchar();
}

void unget_cur_char() 
{
    flag = 1;
}

void skip_white_space()
{
    while (cur_symbol == ' ')
        get_cur_char();
    unget_cur_char();
}

int is_digit(char c)
{
    return c >= '0' && c <= '9';
}

int is_alpha(char c)
{
    return c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z';
}

// копирование строки str1 в строку str2
void str_copy (char *str1, char *str2)
{
    //int i = 0; //"abc" ['a', 'b', 'c', 0]
    while (*str1)
        *str2++ = *str1++;
    *str2 = 0;
}

int get_num()
{
    get_cur_char();
    int cur_num = 0;
    while (is_digit(cur_symbol))
    {
        cur_num = cur_num * 10 + cur_symbol - '0';
        get_cur_char();
    }
    unget_cur_char();
    return cur_num;
}

void get_atom()
{
    get_cur_char();
    int c = 0;
    while (is_alpha(cur_symbol) || is_digit(cur_symbol))
    {
        cur_str[c++] = cur_symbol;
        get_cur_char();
    }
    unget_cur_char();
    cur_str[c] = 0;
}

int get_token()
{
    get_cur_char();
    skip_white_space();
    int token = INVALID;
    switch (cur_symbol) {
        case '(':
            get_cur_char();
            token = LPAREN;
            break;
        case ')':
            get_cur_char();
            token = RPAREN;
            break;
        case '\n':
            token = END;
            break;
        default:
            if (is_digit(cur_symbol)) {
                token = T_NUMBER;
                current_number = get_num();
            } else if (is_alpha(cur_symbol)) {
               token = T_ATOM;
               get_atom();
            } else {
                get_cur_char();
                token = INVALID;
            }
        
    }
    return token;
}

// Вывод сообщения об ошибке и выход из программы
// str - сообщение об ошибке
void error(char *str)
{
    printf("%s\n", str);
    exit(1);
}

void list_add(list_t *new, list_t **head)
{
   if (*head == NULL)
        *head = new;
    else {
        list_t *last = *head;
        while (last->next != NULL)
            last = last->next;
        last->next = new;
    }
}

// head->1 -> 2 -> 3 -> NULL
void add_number(int num)
{
    list_t *new = alloc();
    new->u.value = num;
    new->type = NUMBER;
    new->next = NULL;
    list_add(new, &cur_list);
}

void add_atom()
{
    list_t *new = alloc();
    str_copy(cur_str, new->u.atom);
    new->type = ATOM;
    new->next = NULL;
    list_add(new, &cur_list);
}

void print_numbers()
{
    printf("Numbers entered by the user:\n");
    printf("[ ");
    for (list_t *cur = head; cur != NULL; cur = cur->next) {
        if (cur->type == NUMBER)
            printf("%d ", cur->u.value); // выводим содержимое массива в кавычках и разделяем запятой
        else if (cur->type == ATOM)
            printf("%s ", cur->u.atom);
    }
    printf("]\n");  
}

int cur_token;

void parse_list(list_t **cur_list)
{
    if (cur_token != LPAREN)
        error("expected (");
    cur_token = get_token();
    while (cur_token != END && cur_token != RPAREN) {
        if (cur_token == T_NUMBER) 
            add_number(current_number);
        else if (cur_token == T_ATOM)
            add_atom();
        else if  (cur_token == LPAREN)
            parse_list();
        else if (cur_token == INVALID)
            error("expected number or atom");
        cur_token = get_token();
    }
}

int main()
{
   cur_token = get_token(); // считывается левая скобка
   parse_list(&head); // (1 (2 3))
    if (cur_token != RPAREN)
        error("expected )");
    print_numbers();
    return 0;
}
