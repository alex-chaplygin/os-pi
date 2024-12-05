#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include "mpa.h"
#include "test.h"


jmp_buf jmp_env;

/** 
 * @brief Функция ошибки
 *
 * @param str - строка
 */
void error(char *str, ...)
{
    printf("%s", str);
    longjmp(jmp_env, 1);
}

/** 
 * @brief Проверяем конвертацию числа из строки 
 *
 * @param num1 - число из строки
 * @param r - ожидаемый результат (>=0 - без ошибок, <0 - ошибка)
 */
void test_new_bignum_from_str(const char* num1, int r)
{
    printf("test_new_bignum_from_str: %s\n", num1);
    bignum_t bignum1;
    if (r >= 0) {
	bignum1 = new_bignum_from_str(num1);
	if (num1[0] == '-'){
	    ASSERT(bignum1->size, strlen(num1)-1);
	    ASSERT(bignum1->sign, -1);
	} else {
	    ASSERT(bignum1->size, strlen(num1));
	    ASSERT(bignum1->sign, 1);
	}
    } else if (setjmp(jmp_env) == 0) {
	bignum1 = new_bignum_from_str(num1);
	FAIL;
    } else
	OK;
    free_bignum(bignum1);
}

/** 
 * @brief Проверяем создание числа с дробной частью
 *
 * @param num - число
 * @param exp - ожидаемая экспонента 
 */
void test_new_bignum_from_str_exp(const char* num, int exp)
{
    printf("test_new_bignum_from_str_exp: %s\n", num);
    bignum_t bignum1;
    bignum1 = new_bignum_from_str(num);
    print_bignum(bignum1);
    printf("\n");
    ASSERT(bignum1->exponent, exp); 
    free_bignum(bignum1);
}

/** 
 * @brief Проверяем сложение больших чисел
 *
 * @param num1 - 1-е большое число
 * @param num2 - 2-е большое число
 * @param expected_result - ожидаемый результат
 */
void test_bignum_sum(const char* num1, const char* num2, const char* expected_result)
{
    printf("test_bignum_sum: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);
    bignum_sum(bignum1, bignum2);
    ASSERT(bignum1->size, result_bignum->size);
    ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    ASSERT(bignum1->sign, result_bignum->sign);
    free_bignum(bignum1);
    free_bignum(bignum2);
    free_bignum(result_bignum);
}

/** 
 * @brief Проверяем умножение больших чисел
 *
 * @param num1 - 1-е большое число
 * @param num2 - 2-е большое число
 * @param expected_result - ожидаемый результат
 */
void test_bignum_mult(const char* num1, const char* num2, const char* expected_result)
{
    printf("test_bignum_mult: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    bignum_mult(bignum1, bignum2);
    printf("\n");
    print_bignum(bignum1);
    printf("\n");
    
    ASSERT(bignum1->size, result_bignum->size);
    ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    free_bignum(bignum1);
    free_bignum(bignum2);
    free_bignum(result_bignum);
}

/** 
 * @brief Проверяем вычитание больших чисел
 *
 * @param num1 - 1-е большое число
 * @param num2 - 2-е большое число
 * @param expected_result - ожидаемый результат
 */
void test_bignum_sub(const char* num1, const char* num2, const char* expected_result)
{
    printf("test_bignum_sub: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    bignum_sub(bignum1, bignum2);
    printf("res: ");
    print_bignum(bignum1);
    printf("\n");
    ASSERT(bignum1->size, result_bignum->size);
    ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    ASSERT(bignum1->sign, result_bignum->sign);
    free_bignum(bignum1);
    free_bignum(bignum2);
    free_bignum(result_bignum);
}

/** 
 * 
 *
 * @param num1 
 * @param num2 
 * @param expected_result 
 * @param r 
 */
void test_bignum_div(const char* num1, const char* num2, const char* expected_result, int r)
{
    printf("test_bignum_div: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    /*
    bignum_div(bignum1, bignum2);
    printf("\n");
    print_bignum(bignum1);
    printf("\n");
    */
    if (r >= 0) {
	bignum_div(bignum1, bignum2);
	printf("\n");
	print_bignum(bignum1);
	printf("\n");
	ASSERT(bignum1->size, result_bignum->size);
	ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    }
    else if (setjmp(jmp_env)==0) {
	bignum_div(bignum1, bignum2);
	printf("\n");
	print_bignum(bignum1);
	printf("\n");
	FAIL;
    }
    else
	OK;
    free_bignum(bignum1);
    free_bignum(bignum2);
    free_bignum(result_bignum);
}
/**
 * @brief проверяем конвертацию числа int в большое число
 * @brief печатаем число и проверяем размер
 *
 *@param num - целое число
 *@param size - ожидаемый размер целого числа
 */
void test_bignum_from_int(int num, int size)
{
    printf("test_bignum_from_int: %d ", num);
    
    bignum_t res = bignum_from_int(num);
    print_bignum(res);
    printf("\n");
    ASSERT(res->size, size);
    free_bignum(res);
}

void test_round_bignum(const char* num, int n)
{
    printf("test_round_bignum: %s, result: ", num);
    bignum_t bignum = new_bignum_from_str(num);
    round_bignum(bignum, n);
    print_bignum(bignum);
    printf("\n");
}


int main()
{
    test_new_bignum_from_str("123", 0);
    test_new_bignum_from_str_exp("123.34", 2); // exp = 2
    test_new_bignum_from_str_exp("123.3", 1); // exp = 1
    test_new_bignum_from_str_exp("123.", 0); // exp = 0
    test_new_bignum_from_str_exp("123", 0); // exp = 0
    test_new_bignum_from_str_exp("-123.4567", 4); // exp = 4
    test_new_bignum_from_str("-123", 0);
    test_new_bignum_from_str("45567a1", -1);
    test_new_bignum_from_str("abc", -1);
    
    test_bignum_from_int(23453435, 8);
    test_bignum_from_int(-34, 2);
    test_bignum_from_int(0, 1);
    
    test_bignum_sum("10220100434343002222", "1022010043434300222", "11242110477777302444");
    test_bignum_sum("0", "0", "0");
    test_bignum_sum("10220100434343002222", "0", "10220100434343002222");
    test_bignum_sum("0", "10220100434343002222", "10220100434343002222");
    test_bignum_sum("10","12.25","22.25");
    test_bignum_sum("12.3","14.75","27.05");
    test_bignum_sum("12.3","14.0005","26.3005");
    test_bignum_sum("12.4","13.6","26.0");
    test_bignum_sum("10.375","10.205","20.580");

    /*
      Условия   Правильные классы    Неправильные классы
      n1 > 0        n1 > 0
      n1 = 0        n1 = 0
      n1 < 0        n1 < 0
      n2 > 0        n2 > 0
      n2 = 0        n2 = 0
      n2 < 0        n2 < 0
      n1 содержит     n1 = "432"       n1 = "3443b3"
      только цифры
      n2 содержит     n2 = "4444"      n2 = "12b"
      только цифры
     
      Тесты      
*/
    test_bignum_sum("-10", "10", "0");
    test_bignum_sum("-10", "-10", "-20");
    test_bignum_sum("10", "-20", "-10");
    test_bignum_sum("20", "-10", "10");
    test_bignum_sum("501", "503", "1004");    

    test_bignum_sub("10", "10", "0");
    test_bignum_sub("20", "-10", "30");
    test_bignum_sub("503", "501", "2");
    test_bignum_sub("0", "0", "0");
    test_bignum_sub("0", "1", "-1");
    test_bignum_sub("0", "10", "-10");
    //test_bignum_sub("1", "10", "-9");
    test_bignum_sub("10", "20", "-10");
    test_bignum_sub("0", "-1", "1");
    test_bignum_sub("-1", "0", "-1");
    test_bignum_sub("-1", "1", "-2");
    test_bignum_sub("-1", "-1", "0");
    test_bignum_sub("1", "0", "1");
    test_bignum_sub("1", "1", "0");
    test_bignum_sub("1", "-1", "2");

    test_bignum_mult("1.25", "5.3", "6.625");
    test_bignum_mult("1.25", "-5.3", "-6.625");
    test_bignum_mult("5.3", "1.25", "6.625");
    test_bignum_mult("1.897", "153.4", "290.9998");
    test_bignum_mult("567.28313", "8742.15", "4959274.2149295");
    test_bignum_mult("1", "0.5", "0.5");
    test_bignum_mult("0.5", "0.5", "0.25");
    test_bignum_mult("0.5", "0.000000476837158203125", "0.0000002384185791015625");
    
    test_bignum_mult("732", "841", "615612");
    test_bignum_mult("25", "2", "50");
    test_bignum_mult("5486", "1475", "8091850");
    test_bignum_mult("23", "5762", "132526");
    test_bignum_mult("0", "275", "0");
    test_bignum_mult("512", "0", "0");
    test_bignum_mult("5", "10", "50");

    test_bignum_div("120", "2", "60", 0);
    //test_bignum_div("4879652", "2", "2439826", 0);
    //test_bignum_div("48", "240", "0", 0);
    //test_bignum_div("4879652", "4", "1219913", 0);
    //test_bignum_div("10000000", "1000000", "10", 0);
    //test_bignum_div("154894561", "0", "", -1);
    //test_bignum_div("100000000000000000000000", "2", "50000000000000000000000", 0);

    test_round_bignum("120.123155",4);
    test_round_bignum("120.5", 4);
    return 0;
}
