#define BUFFER_SIZE 512
typedef unsigned char byte;

byte* disk_read_block(byte *buffer, int block_num);