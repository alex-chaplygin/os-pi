#include <stdio.h>
#include <string.h>
#include "mpa.h"
#include "test.h"
#include "../../float.h"

/*
 * Тест печати числа с плавающей точкой
 *
 * @param num - число с плавающей точкой для печати
*/
void test_print_double(double number) {
    printf("test_print_double: %f ", number);
    print_double_num(number);
    printf("\n");
}

int main()
{
    int n;
    long long l;
    test_print_double(0.5);
    test_print_double(16.0);
    test_print_double(12.97);
    test_print_double(-0.0);
    test_print_double(-16.123456);
    l = 0x7ff0000000000000;
    test_print_double(*(double *) &l);
    l = 0xfff0000000000000;
    test_print_double(*(double *) &l);
    l = 0x7ff8000000000000;
    test_print_double(*(double *) &l);
    return 0;
}
