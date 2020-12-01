#define MAX_SEGMENTS 1024 
#define START_ADDRESS 0x200000
#define MEM_SIZE 0x4FFFFFFF

void mem_init(); 
void *malloc(int size);
void free(void *addr);
void test_mem(int num_test);
