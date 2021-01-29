#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define MAXBUF 100

char buf[MAXBUF];
int count;

void read_file(char *name)
{
  int f = open(name, O_RDONLY);
  if (f < 0) {
    printf("File %s not found\n", name);
    return;
  }
  printf("File: %i\n", f);
  do {
    count = read(f, buf, MAXBUF);
    write(0, buf, count);
  } while (count > 0);
  close(f);
}

int main(int argc, char *argv[])
{
  int code;
  while (1) {
    write(0, "$ ", 2);
    count = read(1, buf, MAXBUF);
    if (!strncmp(buf, "exit", count - 1)) break;
    //    write(0, buf, count);
    int pid = fork();
    buf[count - 1] = 0;
    //  printf("pid: %d\n" , pid);
    if (pid == 0) {
      code = execl(buf, buf, (char *)NULL);
      printf("Program not found: %s\n", buf);
    }
  }
  return 0;
}
