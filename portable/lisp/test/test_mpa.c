#include <stdio.h>
#include <string.h>
#include "mpa.h"
#include "test.h"

void test_bignum_sum(const char* num1, const char* num2, const char* expected_result, int r)
{
    printf("test_bignum_sum: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    int res = bignum_sum(bignum1, bignum2);
    ASSERT(res, r);
    if (r >= 0) {
	ASSERT(bignum1->size, result_bignum->size);
	ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    }
    free_bignum(bignum1);
    free_bignum(bignum2);
    free_bignum(result_bignum);
}

void test_bignum_sub(const char* num1, const char* num2, const char* expected_result, int r)
{
    printf("test_bignum_sub: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    int res = bignum_sub(bignum1, bignum2);
    ASSERT(res, r);
    if (r >= 0) {
	ASSERT(bignum1->size, result_bignum->size);
	ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    }
    free_bignum(bignum1);
    free_bignum(bignum2);
    free_bignum(result_bignum);
}

void test_bignum_mult(const char* num1, const char* num2, const char* expected_result)
{
    printf("test_bignum_mult: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    int res = bignum_mult(bignum1, bignum2);
    printf("\n");
    print_bignum(bignum1);
    printf("\n");
    
    ASSERT(bignum1->size, result_bignum->size);
    ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    free_bignum(bignum1);
    free_bignum(bignum2);
    free_bignum(result_bignum);
}

void test_bignum_div(const char* num1, const char* num2, const char* expected_result, int r)
{
    printf("test_bignum_div: %s %s\n", num1, num2);
    bignum_t bignum1 = new_bignum_from_str(num1);
    bignum_t bignum2 = new_bignum_from_str(num2);
    bignum_t result_bignum = new_bignum_from_str(expected_result);

    int res = bignum_div(bignum1, bignum2);
    printf("\n");
    print_bignum(bignum1);
    printf("\n");
    ASSERT(res, r);
    if (r >= 0) {
	ASSERT(bignum1->size, result_bignum->size);
	ASSERT(memcmp(bignum1->data, result_bignum->data, bignum1->size * sizeof(bignum1->data[0])), 0);
    }
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

int main()
{
    test_bignum_from_int(23453435, 8);
    test_bignum_from_int(-34, 2);
    test_bignum_from_int(0, 1);
    
    test_bignum_sum("10220100434343002222", "1022010043434300222", "11242110477777302444", 0);
    test_bignum_sum("0", "0", "0", 0);
    test_bignum_sum("10220100434343002222", "0", "10220100434343002222", 0);
    test_bignum_sum("0", "10220100434343002222", "10220100434343002222", 0);

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
    test_bignum_sum("-10", "10", "0", 0);
    test_bignum_sum("20", "-10", "10", 0);
    test_bignum_sum("2a0", "10", "", -2);
    test_bignum_sum("20", "10z", "", -2);
    test_bignum_sum("501", "503", "1004", 0);    

    test_bignum_sub("10", "10", "0", 0);
    test_bignum_sub("20", "-10", "30", 0);
    test_bignum_sub("503", "501", "2", 0);
    test_bignum_sub("0", "0", "0", 0);
    test_bignum_sub("0", "1", "-1", 0);
    test_bignum_sub("0", "-1", "1", 0);
    test_bignum_sub("-1", "0", "-1", 0);
    test_bignum_sub("-1", "1", "-2", 0);
    test_bignum_sub("-1", "-1", "0", 0);
    test_bignum_sub("1", "0", "1", 0);
    test_bignum_sub("1", "1", "0", 0);
    test_bignum_sub("1", "-1", "2", 0);

    test_bignum_mult("732", "841", "615612");
    test_bignum_mult("25", "2", "50");
    test_bignum_mult("5486", "1475", "8091850");
    test_bignum_mult("23", "5762", "132526");
    test_bignum_mult("0", "275", "0");
    test_bignum_mult("512", "0", "0");
    test_bignum_mult("5", "10", "50");

    test_bignum_div("4879652", "2", "2439826", 0);
    test_bignum_div("48", "240", "0", 0);
    test_bignum_div("4879652", "4", "1219913", 0);
    test_bignum_div("10000000", "1000000", "10", 0);
    test_bignum_div("154894561", "0", "", -1);

    return 0;
}
