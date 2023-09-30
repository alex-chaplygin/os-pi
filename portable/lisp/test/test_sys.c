#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "lexer.h"
#include "objects.h"
#include "eval.h"
#include "test.h"
#include "parser.h"
#include "arith.h"

void get_cur_char();

FILE *old_stdin;
FILE *old_stdout;
extern token_t token;

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
    old_stdout = stdout;
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
    printf("test: %s\n", in_str);
    write_in_file(in_str);
    //redirect_out_file();
    object_t *o = parse();
    //PRINT(o);
    if (o != ERROR) {
	object_t *res = eval(o, NULL);
	//printf("res: ");
	//PRINT(res);
	print_obj(res);
    }
    fclose(stdout);
    stdout = fopen("/dev/tty", "w");    
    char filename[] = "/tmp/out.txt";
    FILE *fp = fopen(filename, "r");
    char str[100];
    fgets(str, 100, fp);
    fclose(fp);
    
    if (strcmp(str, out_str))
	printf("fail\t'%s' != '%s'\n", str, out_str);
    else
	printf("OK\t%s\n", str);
    fclose(stdin);
    stdin = old_stdin;//fdopen(STDIN_FILENO, "r");
    get_cur_char();
}

int main()
{
    init_eval();
    init_arith();
    do {
	object_t *o = parse();
        //printf("parse: "); PRINT(o);
	if (o != ERROR) {
	    object_t *res = eval(o, NULL);
	    //printf("res: "); PRINT(res);
	    print_obj(res);
	}
    } while (token.type != END);
    /*
    printf("--------------SYSTEM TEST---------------------\n");
    test("'a", "A");
    test("'()", "NIL");
    test("(quote())","NIL");
    test("((lambda (x y) (cons x y)) 1 '(2))", "(1 2)");
    test("(defun null (x) (eq x (quote())))", "NULL");
    test("(defun f(x)				\
             (cond \
                ((= x 1) 1) \
                (T (* x (f (- x 1))))))", "F");
    //test("(f 1)", "1");
    test("(f 2)", "6");
    /*test("(null 'a)", "NIL");
    test("(null (quote ()))", "T");
    test("(null '())", "T");
    test("(+ 1 2 3 4)", "10");
    test("(1", "expected )\n");
    test("(+ 1 2 3 5)", "11");
    test("(- 9 5 1 1)", "2");
    test("(* 1 2 3 5)", "30");*/
    return 0;
}

