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
void get_symbol(char *cur_str);
extern char cur_symbol;
extern int flag;

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
    get_cur_char();
    if (cur_symbol != -1)
        unget_cur_char();
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

int main()
{
    test_get_cur_char();
    test_skip_white_space();
    test_skip_new_line();
    test_get_num("1234", 1234);
    test_get_num("-5    ", -5);
    test_is_digit();
    test_is_alpha();
    test_get_symbol("Hello 12", "Hello");
    test_get_token("empty", " ", END);
    test_get_token("lparen", "(", LPAREN);
    test_get_token("rparen", ")", RPAREN);
    test_get_token("tnumber", "42", T_NUMBER);
    test_get_token("quote", "\'", QUOTE);
    test_get_token("invalid", "=", INVALID);
    return 0;
}
