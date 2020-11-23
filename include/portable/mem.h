#define MAX_SEGMENTS 1024 
#define START_ADDRESS 0x2000000 
#define MEM_SIZE 0x4FFFFFFF

void mem_init(); 
void *malloc(int size);
