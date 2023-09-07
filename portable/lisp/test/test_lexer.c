#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include "test.h"
#include "lexer.h"

void get_cur_char();
void unget_cur_char();
void skip_white_space();
int get_num();
extern char cur_symbol;

//Функция пзаписи строки в файл
void write_file(char *string)
{
    char filename[] = "/tmp/temp.txt";
    FILE *fp = fopen(filename, "w");
    if (fp)
    {
        fputs(string, fp);
        fclose(fp);
    }
    close(0);
    open(filename, O_RDONLY);
}

//Тест должен прочитать символ два раза, вернуть символ и прочитать ещё один символ.
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

//Тест должен пропустить пробелs и прочитать один символ.
void test_skip_white_space()
{
  printf("test_skip_white_space: ");
  write_file("      b");
  skip_white_space();
  get_cur_char();
  ASSERT(cur_symbol, 'b');
}

//Тест должен прочитать число.
void test_get_num()
{
  printf("test_get_num: ");
  write_file("1234");
  int curnum = get_num();
  ASSERT(curnum, 1234);
}


int main()
{
  test_get_cur_char();
  test_skip_white_space();
  test_get_num();
  return 0;
}
