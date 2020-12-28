#define NUM_FILES 320
#define FILE_TABLE_IS_FULL -1
#define FILE_NOT_FOUND -2
#define INVALID_NAME -3

void init_files();
int open(char *name);
int close(int id);
int create(char *name);
int read(int id, void *buf, int size);
int write(int id, void *buf, int size);