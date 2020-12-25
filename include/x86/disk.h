#define BLOCK_SIZE 512
#define BLOCK_COUNT 512
#define CATALOG_SIZE 10
#define METADATA_SIZE 11
#define FILE_RECORD_SIZE 16
#define FILE_NAME_SIZE 12
typedef unsigned char byte;

void init_disk();
int disk_read_block(byte *buffer, int block_num);
int find_file(char *name, byte *buffer);