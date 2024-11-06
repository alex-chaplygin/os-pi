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
void test_print_float(float number)
{
    printf("test_print_float: %f ", number);
    print_float_num(number);
    printf("\n");
}

int main()
{
    test_print_float(0.5f);
    test_print_float(16.0f);    
    test_print_float(0.0f);    
    //test_print_float(-16.123456f);    
    return 0;
}
