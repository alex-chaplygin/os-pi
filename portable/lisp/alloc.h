#ifndef REGIONS
#define REGIONS

/// Всего байт для регионов
#ifdef VM
#define MAX_REGION_SIZE 512 * 1024 * 1024
#else
#define MAX_REGION_SIZE 1000000000
#endif
/// Метка региона
#define MAGIC 0xABCD1234

/* Структура региона должна иметь размер, кратный 2^MARKBIT (сейчас 32 байт) */

/// создаваемый или свободный регион памяти
struct region {
    int magic; /// метка своих регионов
    int free; /// свободен ли регион
    struct region *next; /// указатель на следующий регион
    struct region *prev; /// указатель на предыдущий регион
    int size; /// размер региона в байтах
#ifndef X32
    int pad; // выравнивание 28 + 4
#else
    int pad[3]; // выравнивание 20 + 12
#endif
    char data[1]; /// Данные региона
};

void init_regions();
void *alloc_region(int size);
void free_region(void *data);
int regions_mem();

#endif
