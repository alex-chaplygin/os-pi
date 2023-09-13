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
    close(0);
    open(filename, O_RDONLY);
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
void test_get_num()
{
    printf("test_get_num: ");
    write_file("1234");
    int curnum = get_num();
    ASSERT(curnum, 1234);
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
void test_get_symbol()
{
    //Строка-источник
    char src[] = "Hello  ";
    
    //Ожидаемое значение
    char expect[] = "Hello";
    
    //Буфер
    char str[20];
    
    printf("test_get_symbol: ");
    write_file(src);
    get_symbol(str);
    printf("res = '%s' exp = '%s'", str, expect);
    ASSERT(strcmp(str, expect), 0);
}

int main()
{
    test_get_cur_char();
    test_skip_white_space();
    test_skip_new_line();
    test_get_num();
    test_is_digit();
    test_is_alpha();
    test_get_symbol();
    return 0;
}
