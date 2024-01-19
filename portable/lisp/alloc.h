#ifndef REGIONS
#define REGIONS

/// Всего байт для регионов 
#define MAX_REGION_SIZE 50000000

/// Метка региона
#define MAGIC 0xABCD1234

#pragma pack(4)
/// создаваемый или свободный регион памяти
struct region {
    int magic; /// метка своих регионов
    int free; /// свободен ли регион
    struct region *next; /// указатель на следующий регион
    struct region *prev; /// указатель на предыдущий регион
    int size; /// размер региона в байтах
#ifdef X64
    int pad;
#else
    int pad[3];
#endif
    char data[1]; /// Данные региона
};

void init_regions();
void *alloc_region(int size);
void free_region(void *data);

#endif
