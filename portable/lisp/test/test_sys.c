#include <stdio.h>
#include <string.h>
#include "objects.h"
#include "eval.h"
#include "test.h"
#include "parser.h"

FILE *old_stdin;

/** 
 * Функция перенаправления стандартного ввода в файл.
 * Записывает строку в файл
 *
 * @param string строка, содержащая данные
 */
void write_in_file(char *string)
{
    char filename[] = "/tmp/in.txt";
    FILE *fp = fopen(filename, "w");
    if (fp) {
	fputs(string, fp);
	fclose(fp);
    }
    old_stdin = stdin;
    freopen(filename, "r", stdin);
}

/** 
 * Функция перенаправления стандартного вывода в файл.
 * Записывает строку в файл
 *
 * @param string строка, содержащая данные
 */
void redirect_out_file()
{
    char filename[] = "/tmp/out.txt";
    freopen(filename, "w", stdout);
}
/** 
 * Тестирование интерпретатора
 *
 * @param in_str строка, содержащая входное выражение
 * @param out_str строка, содержащая эталонное вычисленное выражение
 */
void test(char *in_str, char *out_str)
{
    write_in_file(in_str);
    //redirect_out_file();
    object_t *o = parse();
    printf("parse: ");
    PRINT(o);
    object_t *res = eval(o, NULL);
    printf("res: ");
    PRINT(res);
    //print_obj(res);
    //fclose(stdout);
    //stdout = fdopen(0, "w");
    /*char filename[] = "/tmp/out.txt";
    FILE *fp = fopen(filename, "r");
    char str[100];
    fgets(str, 100, fp);
    fclose(fp);
    printf("str=%s\n", str);
    if (strcmp(str, out_str))
	printf("fail\t%s\n\t%s\n\t%s\n", in_str, str, out_str);
    else
    printf("OK\t%s\n\t%s\n", in_str, str);*/
    fclose(stdin);
    stdin = fdopen(1, "r");
    //    stdin = old_stdin;	
}

void main()
{
    init_eval();
    printf("--------------SYSTEM TEST---------------------\n");
    //test("'a", "A");
    test("((lambda (x) x) 1)", "1");
}

