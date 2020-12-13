#define CMOS_SECONDS 0x00	/**< Номер регистра секунд 0-59 */
#define CMOS_MINUTES 0x02	/**< Номер регистра минут 0-59 */
#define CMOS_HOURS 0x04		/**< Номер регистра часов 0-23 для 24-х часового формата, 1-12 для 12-ти часового формата */
#define CMOS_WEEKDAY 0x06	/**< Номер регистра дней недели 1-7 */
#define CMOS_DAY 0x07		/**< Номер регистра дней месяца 1-31 */
#define CMOS_MONTH 0x08		/**< Номер регистра месяца 1-12 */
#define CMOS_YEAR 0x09		/**< Номер регистра года 0-99 */
#define CMOS_CENTURY 0x32	/**< Номер регистра века 19-21 */

#include <portable/types.h>

/** 
 * @brief Фунция чтения регистра cmos
 * 
 * @param num_reg Номер регистра cmos
 * 
 * @return Значние регистра
 */
byte cmos_read(int num_reg);

/** 
 * @brief Функция записи в регистр cmos
 * 
 * @param num_reg Номер регистра cmos
 * @param val Значение для записи
 */
void cmos_write(int num_reg, byte val);
