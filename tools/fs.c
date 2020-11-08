/**
 * @file fs.c
 * @author zergon321 (maximgradan@gmail.com)
 * @brief 
 * @version 0.1
 * @date 2020-11-08
 * 
 * @copyright Copyright (c) 2020
 * 
 */

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define C_CREATE "create"
#define P_NAME "-o"
#define P_SIZE "-s"

#define C_WRITE "write"
#define P_DISK "-d"
#define P_FILE "-f"

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "no command specified\n");
        return 1;
    }

    char* command = argv[1];

    if (strcmp(command, C_CREATE) == 0) {
        char* name;
        int size;

        if (get_args_create(argc, argv, &name, &size) == 0) {
            printf("Disk name: %s\n", name);
            printf("Disk size (in blocks): %d\n", size);
        } else {
            fprintf(stderr, "invalid call format");
            return -1;
        }
    } else if (strcmp(command, C_WRITE) == 0) {
        char* diskname;
        char* filename;

        if (get_args_write(argc, argv, &diskname, &filename) == 0) {
            printf("Disk name: %s\n", diskname);
            printf("File name: %s\n", filename);
        } else {
            fprintf(stderr, "invalid call format");
            return -1;
        }
    } else {
        printf("unknown command");
        return -1;
    }

    return 0;
}

/**
 * @brief Считывает аргументы командной строки для команды 'create'.
 * 
 * @param argc число аргументов командной строки.
 * @param argv аргументы командной строки.
 * @param name переменная для хранения имени выходного файла (диска).
 * @param size переменная для хранения размера диска (в блоках).
 * @return int - 0, если всё успешно, и -1, если формат ввода неверен.
 */
int get_args_create(int argc, char* argv[], char** name, int* size) {
    if (argc < 6) {
        return -1;
    }

    char* param = argv[2];
    int name_def = 0, size_def = 0;

    if (strcmp(param, P_NAME) == 0) {
        *name = argv[3];
        name_def = 1;
    } else if (strcmp(param, P_SIZE) == 0) {
        *size = atoi(argv[3]);

        if (*size == 0) {
            return -1;
        }

        size_def = 1;
    } else {
        return -1;
    }
    
    param = argv[4];

    if (strcmp(param, P_NAME) == 0) {
        if (!size_def) {
            return -1;
        }

        *name = argv[5];
    } else if (strcmp(param, P_SIZE) == 0) {
        if (!name_def) {
            return -1;
        }

        *size = atoi(argv[5]);

        if (*size == 0) {
            return -1;
        }
    } else {
        return -1;
    }

    return 0;
}

/**
 * @brief Считывает аргументы командной строки для команды 'write'.
 * 
 * @param argc число аргументов командной строки.
 * @param argv аргументы командной строки.
 * @param disk имя виртуального диска.
 * @param file имя файла, который надо создать на виртуальном диске.
 * @return int - 0, если всё успешно, и -1, если формат ввода неверен.
 */
int get_args_write(int argc, char* argv[], char** disk, char** file) {
    if (argc < 6) {
        return -1;
    }

    char* param = argv[2];
    int disk_def = 0, file_def = 0;

    if (strcmp(param, P_DISK) == 0) {
        *disk = argv[3];
        disk_def = 1;
    } else if (strcmp(param, P_FILE) == 0) {
        *file = argv[3];
        file_def = 1;
    } else {
        return -1;
    }
    
    param = argv[4];

    if (strcmp(param, P_DISK) == 0) {
        if (!file_def) {
            return -1;
        }

        *disk = argv[5];
    } else if (strcmp(param, P_FILE) == 0) {
        if (!disk_def) {
            return -1;
        }

        *file = argv[5];
    } else {
        return -1;
    }

    return 0;
}