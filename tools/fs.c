#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "no command specified\n");
        return 1;
    }

    printf(argv[1]);

    return 0;
}