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
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>

typedef int FILE_HANDLER;
typedef unsigned short word;
typedef unsigned char byte;

#define C_CREATE "create"
#define P_NAME "-o"
#define P_SIZE "-s"

#define C_WRITE "write"
#define P_DISK "-d"
#define P_FILE "-f"

#define BLOCK_SIZE 512
#define CATALOG_SIZE 10

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
            if (create_virtual_disk(name, size) >= 0) {
                printf("created a new virtual disk '%s' with size of '%d' blocks", name, size);
            } else {
                fprintf(stderr, "couldn't create a new virtual disk");
                return -1;
            }
        } else {
            fprintf(stderr, "invalid call format");
            return -1;
        }
    } else if (strcmp(command, C_WRITE) == 0) {
        char* disk;
        char* file;

        if (get_args_write(argc, argv, &disk, &file) == 0) {
            printf("Disk name: %s\n", disk);
            printf("File name: %s\n", file);
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

off_t fsize(const char *filename) {
    struct stat st; 

    if (stat(filename, &st) == 0)
        return st.st_size;

    return -1; 
}

/**
 * @brief Записывает в бинарный файл значение типа 'word' (2 байта).
 * 
 * @param file файл, в который записывается значение.
 * @param value значение, которое записывается в файл.
 * @return int - 0, если всё успешно, и -1, если возникла ошибка.
 */
int write_word_to_file(FILE_HANDLER file, word value) {
    byte buffer[2];

    buffer[0] = (value >> 8) & 0x00FF;
    buffer[1] = value & 0x00FF;

    return write(file, buffer, 2);
}

/**
 * @brief Создаёт новый виртуальный диск для хранения файлов.
 * 
 * @param name имя файла, в котором будет срдержаться разметка виртуального диска.
 * @param size размер виртуального диска в блоках.
 * @return int - 
 */
int create_virtual_disk(char* name, int size) {
    if (size > USHRT_MAX) {
        return -1;
    }

    FILE_HANDLER file;
    word num_of_blocks = (word)size;
    word magic = 0x7171;

    if ((file = open(name, O_CREAT | O_WRONLY | O_TRUNC)) >= 0) {
        /* Записать суперблок. */
        // Магическое число.
        if (write_word_to_file(file, magic) < 0) {
            return -1;
        }

        // Число блоков.
        if (write_word_to_file(file, num_of_blocks) < 0) {
            return -1;
        }

        // Число файлов на диске.
        if (write_word_to_file(file, 0) < 0) {
            return -1;
        }

        // Записывает размер блока.
        if (write_word_to_file(file, BLOCK_SIZE) < 0) {
            return -1;
        }

        // Все оставшиеся байты суперблока установаить в 0.
        byte filler = 0;

        for (int i = 0; i < BLOCK_SIZE - 8; i++) {
            if (write(file, &filler, 1) < 0) {
                return -1;
            }
        }

        /* Записать область с именами и адресами файлов;
           одна запись - 16 байтов, где 12 байтов - имя
           файла, 2 байта - адрес первого блока файла на
           диске, 2 байта - длина файла (в блоках).      */
        for (int i = 0; i < CATALOG_SIZE * BLOCK_SIZE; i++) {
            if (write(file, &filler, 1) < 0) {
                return -1;
            }
        }

        /* Записать область для хранения данных файлов. */
        word file_data_size = size - 11;

        for (int i = 0; i < file_data_size * BLOCK_SIZE; i++) {
            if (write(file, &filler, 1) < 0) {
                return -1;
            }
        }
    }

    return 0;
}

int write_file_to_disk(char* diskname, char* filename) {
    FILE_HANDLER disk, file;

    if (disk = open(diskname, O_WRONLY) < 0) {
        return -1;
    }

    if (file = open(filename, O_RDONLY, 0) < 0) {
        return -1;
    }
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