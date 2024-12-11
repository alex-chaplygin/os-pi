#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <setjmp.h>
#include "test.h"
#include "lexer.h"

void get_cur_char();
void unget_cur_char();
void skip_white_space();
int get_num();
int is_symbol(char c);
void get_symbol(char *cur_str);
void reset_buffer();
extern char cur_symbol;

FILE *oldstdin;

jmp_buf jmp_env;

void parser_error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
}

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

/*
* Генерация последовательности символов
*
* @param size размер последовательности
* @param fill символ заполнения
*/
char *generate_raw_string(int size, char fill)
{
    char *res = (char*)malloc((size + 1)  * sizeof(char));
    memset(res, fill, size);
    res[size] = '\0';
    return res;
}

/*
* Генерация строки
*
* @param size размер строки
* @param fill символ заполнения
*/
char *generate_string(int size, char fill)
{
    char *raw = generate_raw_string(size + 2, fill);
    raw[0] = '\"';
    raw[size + 1] = '\"';
    return raw;
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
    reset_buffer();
    token_t *t = get_token();
    int curnum = t->value;
    ASSERT(curnum, expect);
}

/** 
 * Тест должен прочитать число. 
 */
void test_get_float_num(char* src, float expect)
{
    printf("test_get_float_num: %s ", src);
    write_file(src);
    fclose(stdin);
    stdin = oldstdin;
    write_file(src);
    reset_buffer();
    token_t *t = get_token();
    int curnum = t->value;
    ASSERT(t->type, T_FLOAT);
    ASSERT(curnum, *(int *)&expect);
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
    char str[MAX_SYMBOL];
    
    printf("test_get_symbol: ");
    write_file(src);
    reset_buffer();
    token_t *t = get_token();
    printf("res = '%s' exp = '%s'", t->str, expect);
    ASSERT(strcmp(t->str, expect), 0);
}

/**
 * Проверка получения токена типа exp.
 * name_test - имя теста
 * str - входная строка
 * exp - ожидаемый тип токена
 */
void test_get_token(const char* name_test, char* str, tokentype_t exp) 
{
    printf("test_get_token_%s : ", name_test); // вывод имени теста
    write_file(str); // запись в файл
    reset_buffer();
    tokentype_t res = get_token()->type; // получение типа токена
    ASSERT(res, exp);
}

/**
 * Проверка получения токена типа exp.
 * name_test - имя теста
 * str - входная строка
 * exp1 - ожидаемый тип токена
 * exp2 - ожидаемый тип токена
 */
void test_get_token2(const char* name_test, char* str, tokentype_t exp1,tokentype_t exp2)
{
    printf("test_get_token2 %s : ", name_test); // вывод имени теста
    write_file(str); // запись в файл
    reset_buffer();
    token_t t1 = *get_token();
    print_token(&t1);
    token_t t2 = *get_token();
    print_token(&t2);
    ASSERT(t1.type, exp1);
    ASSERT(t2.type, exp2);
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
void test_invalid_token(const char* name_test, char* str) 
{
    printf("test_get_num_%s : ", name_test); // вывод имени теста
    write_file(str); // запись в файл
    reset_buffer();

    if (setjmp(jmp_env) == 0) {
	get_token();
	FAIL;
    }
    else
	OK;
}

/*
* Тестирование строки в ""
*/
void test_string(char* str, char* exp_str) 
{
    printf("test_string %s ", str); // вывод имени теста
    write_file(str); // запись в файл
    reset_buffer();
    token_t *tok = get_token();
    printf("res = '%s' exp = '%s'", tok->str, exp_str);
    ASSERT(tok->type, T_STRING);
    ASSERT(strcmp(tok->str, exp_str), 0);
}

/*
* Проверка получения ошибки, если номер hex символа строки в "" больше 255
*/
void test_invalid_string(char* str)
{
    printf("test_invalid_symbol_code %s ", str);
    write_file(str);
    reset_buffer();

    if (setjmp(jmp_env) == 0) {
	token_t *tok = get_token();
	FAIL;
    }
    else
	OK;
}

/*
* Тестирование строки максимального размера
*/
void test_string_max()
{
    char *in  = generate_string(MAX_STR, 'a');
    char *out = generate_raw_string(MAX_STR, 'a');
    test_string(in, out);
    free(in);
    free(out);
}

/*
* Тестирование превышения длины строки
*/
void test_string_overflow()
{
    printf("test_string_overflow :");
    char *src = generate_string(MAX_STR + 1, 'a');
    write_file(src); // запись в файл 
    free(src);

    if (setjmp(jmp_env) == 0) {
	get_token();
	FAIL;
    }
    else
	OK;
}

/*
* Тестирование символа максимального размера
*/
void test_symbol_max()
{
    char *in  = generate_string(MAX_SYMBOL, 'a');
    char *out = generate_raw_string(MAX_SYMBOL, 'a');
    test_get_symbol(in, out);
    free(in);
    free(out);
}

/*
* Тестирование превышения длины строки
*/
void test_symbol_overflow()
{
    printf("test_symbol_overflow :");
    char *src = generate_raw_string(MAX_SYMBOL + 1, 'a');
    write_file(src);

    if (setjmp(jmp_env) == 0) {
	get_token();
	FAIL;
    }
    else
	OK;
}


/*
 * Тестирует получение одиночного символа типа T_CHAR из входной строки
 * 
 * @param src           Входная строка, содержащая символ, который нужно проверить
 * @param expected_char Ожидаемый символ, который должен быть извлечен из src
 * @param expected_type Ожидаемый тип токена, который должен быть возвращен функцией get_token
 */
void test_get_char(char* src, char expected_char, int expected_type)
{
    printf("test_get_char: %s ", src);
    write_file(src);
    reset_buffer();
    token_t *t = get_token();
    ASSERT(t->type, expected_type);
    ASSERT(t->value, expected_char);            
}

/*
get_token
|условие               |правильный класс                                        |неправильный класс                         | 

|одиночные символы     |1) символ (                                             |16) некорректные одиночные символы         |
|                      |2) символ )                                             |                                           |
|                      |3) символ EOF                                           |                                           |
|                      |4) символ '                                             |                                           |
|                      |5) символ `                                             |                                           |
|                      |6) символ ,                                             |                                           |
|                      |7) символ ,@                                            |                                           |
|                      |8) символ #                                             |
|                      |21) символ .                                            |
|строка                |9) корректная строка                                    |17) строка без закрывающей кавычки         |
|комментарии           |10) комментарий - начинается c ';' оканчивается '\n'    |                                           |
|число                 |11) положительное dec-число                             |18) некорректное Dec-число                 |
|                      |12) отрицательное dec-число                             |19) некорректное Hex-число                 |
|                      |13) Hex-число                                           |                                           |
|символ                |14) допустимый символ                                   |20) недопустимый символ                    |
|пустоты               |15) пустоты - любая последовательность                  |                                           |
|                      |    '\r', '\n', '\t', ' '                               |                                           |
*/
 
int main()
{
    printf("-------------test_lexer---------------\n");    
    // Пропуск пустот 
    test_get_cur_char();
    test_skip_white_space(); 
    test_skip_new_line();   
     
    // Числа (T_NUMBER)
    test_get_token("tnumber", "42", T_NUMBER); // 11
    test_get_num("1234", 1234); // 11
    test_get_num("-5    ", -5); // 12
    test_get_num("-2147483648", -2147483648); // граничный тест минимальное число
    test_get_num("2147483647", 2147483647); // граничный тест максимальное число
    test_get_num("0xF", 15); // 13
    test_get_num("0xa", 10); // 13
    test_get_num("0xff", 255); // 13
    test_get_num("0x1A23", 0x1A23); // 13
    test_get_num("0xFFFFFFFF", 0xFFFFFFFF); // граничный тест максимальное число
    test_get_num("0x00000000", 0x00000000); // граничный тест минимальное число
    test_get_num("000123", 123); // число с нулями
    test_get_num("-00025", -25); // отрицательное число с нулями

    // Ошибки в числах    
    test_invalid_token("invalid num", "-2147483649"); // граничный тест минимальное число - 1
    test_invalid_token("invalid num", "2147483648"); // граничный тест максимальное число + 1
     test_invalid_token("invalid hex", "0x100000000"); // граничный тест максимальное число + 1
    test_invalid_token("invalid num", "11D"); // 18
    test_invalid_token("invalid num", "0GG"); // 19
    test_invalid_token("invalid hex", "0xfrf"); // 19
    test_invalid_token("invalid hex", "0xrf"); // 19
    test_invalid_token("hexadecimal_float", "0x1.23p2"); // некорректное hex число

    // Вещественные числа (T_FLOAT)
    test_get_float_num("1.0", 1.0f);
    test_get_float_num("0.0", 0.0f);
    test_get_float_num("10.567", 10.567f);
    test_get_float_num("1024.1024", 1024.1024f);
    test_get_float_num("-1024.1024", -1024.1024f);
    test_get_float_num("0.25", 0.25f);
    test_get_float_num("-0.25", -0.25f);
    // Проверка граничных значений
    //test_get_float_num("3.4028235e38", 3.4028235e38f);
    //test_get_float_num("-3.4028235e38", -3.4028235e38f);     
    // Ошибки с вещественными числами
    //test_invalid_token("3.5028235e38", "3.5028235e38f"); //чило больше максимально допустимого
    //test_invalid_token("-3.5028235e38", "-3.5028235e38f"); //чило меньше максимально допустимого
    test_invalid_token("invalid format 1.0.0", "1.0.0"); // неправильный формат
    test_invalid_token("invalid float 3..1", "3..14"); // две точки
    test_invalid_token("invalid float 1.14s", "1.14s"); // буква
    
    // Одиночные символы
    test_get_token("lparen", "(", LPAREN); // 1
    test_get_token("rparen", ")", RPAREN); // 2
    test_get_token("quote", "\'", QUOTE); // 4
    test_get_token("backquote", "`", BACKQUOTE); // 5
    test_get_token("comma", ",", COMMA); // 6
    test_get_token("comma_at", ",@", COMMA_AT); // 7
    test_get_token("sharp", "#(1 2 3)", SHARP); // 8
    test_get_token("function", "#'", T_FUNCTION);
    test_get_token("symbol", "abc", T_SYMBOL);  // 14
    test_get_token("dot", ".", DOT); //21
    test_get_token("empty", " ", END); // 3

    // Коментарии
    test_get_token("comment", " ; comment\n  42", T_NUMBER); // 10, 11
    test_get_token("comment_board", " ; \n  42", T_NUMBER); // граничный тест пустой комментарий
    test_get_token("comment2", ";comment\n\n\n;fffff\n  42", T_NUMBER); // 10, 11, 15
    test_get_token("comment3", ";comment;dsada\n  42", T_NUMBER); // 10, 11, 15
    test_get_token("comment4", ";comment", END); // 10

    // Строки - T_STRING
    test_string("\"1 2 3\"", "1 2 3"); // 9
    test_string("\"\\x31\\x32\"", "12");
    test_string("\"a b\\n\"", "a b\n"); // 9
    test_string("\"a b\\n\\n\"", "a b\n\n"); // 9
    test_invalid_string("\"\\x299\\x230\"");//
    test_string("\"\"", ""); // граничный тест на пустую строку
    test_string_max(); // граничный тест на максимальную строку
    test_string_overflow(); // граничный тест на превышение допустимого размера строки   
    test_invalid_token("invalid string", "\"1 2 3"); // 17
    test_invalid_string("\"abc\\\""); // незакрытая escape-последовательность.
    test_invalid_string("\"\\xG1\""); // неправильный символ после \\x.
    test_invalid_string("\"invalid escape \\q\""); // неправильная escape-последовательность(\q)
    
    // Корректные классы T_CHAR
    test_get_char("#\\a", 'a', T_CHAR);
    test_get_char("#\\Z", 'Z', T_CHAR);
    test_get_char("#\\ ", ' ', T_CHAR);
    test_get_char("#\\1", '1', T_CHAR);
    test_get_char("#\\@", '@', T_CHAR);

    // Тесты символов
    test_get_symbol("Hello 12", "Hello");
    test_get_symbol("Hello\b", "Hell");
    test_get_symbol("+ plus", "+");
    test_get_symbol("- minus", "-");
    test_get_symbol("* multiply", "*");
    test_get_symbol("/ divide", "/");
    test_get_symbol("= equals", "=");
    test_get_symbol("_ underscore", "_");
    test_get_symbol("& ampersand", "&");
    test_get_symbol("| pipe", "|");
    test_get_symbol("< less_than", "<");
    test_get_symbol("> greater_than", ">");
    test_get_symbol("% percent", "%");

    // Тесты для символов с комбинированным содержимым
    test_get_symbol("setq_rec", "setq_rec");
    test_get_symbol("setq_rec setq_rec ", "setq_rec");
    test_get_symbol("symbol123", "symbol123"); // буквы и цифры должны распознаться как валидный символ типа T_SYMBOL
    test_get_symbol(" symbol1.23", "symbol1.23"); 

    // Граничный тест на максимальный символ
    test_symbol_max();

    // Тесты для некорректных символов
    test_invalid_token("invalid_symbol", "^"); // 16
    test_invalid_token("invalid_symbol", "!~"); // 16
    test_invalid_token("invalid symbol", "ss^s?"); // 20

    // Разрешенные символы
    test_is_symbol(';', 0);
    test_is_symbol('+', 1);
    // Ошибочные символы - INVALID
    test_symbol_overflow(); // граничный тест на превышение допустимого размера символа
    return 0;
}     
