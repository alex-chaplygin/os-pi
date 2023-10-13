#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include "test.h"
#include "lexer.h"

void get_cur_char();
void unget_cur_char();
void skip_white_space();
int get_num();
int is_alpha(char);
int is_digit(char);
int is_symbol(char c);
void get_symbol(char *cur_str);
extern char cur_symbol;
extern int flag;
extern int token_error;

FILE *oldstdin;

/** 
 * Функция перенаправления стандартного ввода в файл.
 * Записывает строку в файл
 *
 * @param string строка, содержащая данные
 */
void write_file(char *string)
{
    char filename[] = "/tmp/temp.txt";
    FILE *fp = fopen(filename, "w");
    if (fp) {
	fputs(string, fp);
	fclose(fp);
    }
    oldstdin = stdin;
    freopen(filename, "r", stdin);
}

/** 
 * Тест должен прочитать символ два раза, вернуть символ и прочитать ещё один 
 * символ
 */
void test_get_cur_char()
{
    printf("test_get_cur_char: ");
    write_file("ab");
    get_cur_char();
    get_cur_char();
    unget_cur_char();
    get_cur_char();
    ASSERT(cur_symbol, 'b');
}

/** 
 * Тест должен пропустить пробелs и прочитать один символ. 
 */
void test_skip_white_space()
{
    printf("test_skip_white_space: ");
    write_file("      b");
    skip_white_space();
    get_cur_char();
    ASSERT(cur_symbol, 'b');
}

/** 
 * Тест должен пропустить пробелы, переводы строк и прочитать один символ. 
 */
void test_skip_new_line()
 {
     printf("test_skip_new_line:");
     write_file("       \n   \n\n   \nb");
     skip_white_space();
     get_cur_char();
     ASSERT(cur_symbol, 'b');
 }

/** 
 * Тест должен прочитать число. 
 */
void test_get_num(char* src, int expect)
{
    printf("test_get_num: ");
    write_file(src);
    fclose(stdin);
    stdin = oldstdin;
    write_file(src);
    flag = 0;
    int curnum = get_num();
    ASSERT(curnum, expect);
}

/** 
 * Тест должен проверять что символ это цифра
 */
void test_is_digit()
{
    ASSERT(is_digit('9'), 1);
    ASSERT(is_digit('Q'), 0);
}

/** 
 * Тест должен проверять что символ это буква
 */
void test_is_alpha()
{
    printf("test_is_alpha: ");
    ASSERT(is_alpha('a'), 1);
}

/** 
 * Тест должен проверять что символ это разрешенный символ или нет
 */

void test_is_symbol(char ch, int ch_i)
{
    printf("test_is_symbol: ");
    ASSERT(is_symbol(ch), ch_i);
}

/** 
 * Тест должен прочесть символ
 */
void test_get_symbol(char* src, const char* expect)
{
    //Буфер
    char str[20];
    
    printf("test_get_symbol: ");
    write_file(src);
    get_cur_char();
    get_symbol(str);
    printf("res = '%s' exp = '%s'", str, expect);
    ASSERT(strcmp(str, expect), 0);
}

/**
 * Проверка получения токена типа exp.
 */
void test_get_token(const char* name_test, char* str, tokentype_t exp) 
{
    printf("test_get_token_%s : ", name_test); // вывод имени теста
    write_file(str); // запись в файл
    tokentype_t res = get_token()->type; // получение типа токена
    flag = 0; // считать символ (если true, не считывать символ)
    ASSERT(res, exp);
}

void test_print_token(token_t *token, const char *expected_output)
{
    printf("test_print_token: ");
    
    int outdes = dup(1);
    FILE *file = freopen("/tmp/test.txt", "w", stdout);
    print_token(token);
    fclose(file);

    stdout = fdopen(outdes, "w");

    FILE *output_file = fopen("/tmp/test.txt", "r");
    char output_buffer[20];
    fgets(output_buffer, sizeof(output_buffer), output_file);
    fclose(output_file);

    ASSERT(strcmp(output_buffer, expected_output), 0);
}

/*
* Проверка получения ошибки, если в значении встречается буква.
*/
void test_invalid_num(const char* name_test, char* str, int error) 
{
    printf("test_get_num_%s : ", name_test); // вывод имени теста
    write_file(str); // запись в файл
    get_token(str);
    flag = 0; // считать символ (если true, не считывать символ)
    ASSERT(token_error, error);
}

/*
* Тестирование строки в ""
*/
void test_string(char* str, char* exp_str) 
{
    printf("test_string :"); // вывод имени теста
    write_file(str); // запись в файл
    token_t *tok = get_token();
    printf("res = '%s' exp = '%s'", tok->str, exp_str);
    flag = 0; // считать символ (если true, не считывать символ)
    ASSERT(tok->type, T_STRING);
    ASSERT(strcmp(tok->str, exp_str), 0);
}

int main()
{
    printf("-------------test_lexer---------------\n");

    token_t token;
    token.type = T_NUMBER;
    token.value = 42;

    test_print_token(&token, "NUM 42\n");    

    test_get_cur_char();
    test_skip_white_space();
    test_skip_new_line();
    test_get_num("1234", 1234);
    test_get_num("-5    ", -5);
    test_get_num("0xF", 15);
    test_get_num("0xa", 10);
    test_get_num("0xff", 255);
    test_get_num("0x1A23", 0x1A23);
    test_is_digit();
    test_is_alpha();
    test_is_symbol('+', 1);
    test_is_symbol(';', 0);
    test_get_symbol("Hello 12", "Hello");
    test_get_symbol("* 1 2", "*");
    test_get_token("empty", " ", END);
    test_get_token("lparen", "(", LPAREN);
    test_get_token("rparen", ")", RPAREN);
    test_get_token("comment", " ; comment\n  42", T_NUMBER);
    test_get_token("comment2", ";comment\n\n\n;fffff\n  42", T_NUMBER);
    test_get_token("comment3", ";comment;dsada\n  42", T_NUMBER);
    test_get_token("tnumber", "42", T_NUMBER);
    test_get_token("quote", "\'", QUOTE);
    test_get_token("sharp", "#(1 2 3)", SHARP);
    test_invalid_num("invalid num", "11D", 1);
    test_invalid_num("valid num", "11 dd", 0);
    test_invalid_num("invalid num", "0GG", 1);
    test_invalid_num("invalid hex", "0xfrf", 1);
    test_invalid_num("invalid hex", "0xrf", 1);
    test_string("\"1 2 3\"", "1 2 3");
    return 0;
}
