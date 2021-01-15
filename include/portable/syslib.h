#include <portable/types.h> 

int syscall_open(int fd);
int syscall_create(int fd);
int syscall_close(int fd);
int syscall_seek(int fd, int count);
int syscall_fstat(int fd, void *buf);
int syscall_set_attr(int fd, void *buf);
void syscall_read(int fd, void *buf, int count);
void syscall_write(int fd, void *buf, int count);
int syscall_fork();
int syscall_exec(int fd);
void syscall_exit(int status);
int syscall_wait(int fd);