#define NUM_FILES 320

void init_files();
int open(char *name);
int close(int id);
int create(char *name);
int read(int id, void *buf, int size);
int write(int id, void *buf, int size);