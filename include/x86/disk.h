#include <portable/types.h>

#define BLOCK_SIZE 512
#define BLOCK_COUNT 512

void init_disk();
int disk_read_block(byte *buffer, int block_num, int pos);
int disk_write_block(byte *buffer, int block_num);
