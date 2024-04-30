#ifndef REGIONS
#define REGIONS

/// Начальный адрес памяти
#define MEM_START 0x200000 
/// Всего байт для регионов 
#define MAX_REGION_SIZE 24000000

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
#ifndef X32
    int pad;
#else
    int pad[3];
#endif
    char data[1]; /// Данные региона
};

void init_regions();
void *alloc_region(int size);
void free_region(void *data);
int regions_mem();

#endif
