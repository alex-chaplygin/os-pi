#define BLOCK_SIZE 512
#define BLOCK_COUNT 512
typedef unsigned char byte;

int disk_read_block(byte *buffer, int block_num);