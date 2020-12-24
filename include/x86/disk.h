#define BLOCK_SIZE 512
#define BUFFER_SIZE 512
typedef unsigned char byte;

void disk_read_block(byte *buffer, int block_num);