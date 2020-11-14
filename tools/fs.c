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

// Размеры.
#define BLOCK_SIZE 512
#define CATALOG_SIZE 10
#define METADATA_SIZE 11
#define FILE_RECORD_SIZE 16
#define FILE_NAME_SIZE 12

// Адреса.
#define CATALOG_OFFSET 1
#define BLOCK_SIZE_OFFSET 6
#define FILES_COUNT_OFFSET 4

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "no command specified\n");
        return 1;
    }

    char* command = argv[1];

    if (strcmp(command, C_CREATE) == 0) {
        char* name = NULL;
        int size = -1;

        if (get_args_create(argc, argv, &name, &size) == 0) {
            if (create_virtual_disk(name, size) >= 0) {
                printf("created a new virtual disk '%s' with size of '%d' blocks\n", name, size);
            } else {
                fprintf(stderr, "couldn't create a new virtual disk\n");
                return -1;
            }
        } else {
            fprintf(stderr, "invalid call format\n");
            return -1;
        }
    } else if (strcmp(command, C_WRITE) == 0) {
        char* disk = NULL;
        char* file = NULL;

        if (get_args_write(argc, argv, &disk, &file) == 0) {
            if (write_file_to_disk(disk, file) >= 0) {
                printf("wrote file '%s' to disk '%s'\n", file, disk);
            } else {
                fprintf(stderr, "couldn't write file '%s' to disk '%s'\n", file, disk);
                return -1;
            }
        } else {
            fprintf(stderr, "invalid call format\n");
            return -1;
        }
    } else {
        printf("unknown command\n");
        return -1;
    }

    return 0;
}

/**
 * @brief Возвращает размер файла в байтах.
 * 
 * @param filename имя файла на диске.
 * @return off_t - размер файла в байтах.
 */
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
    clear_buffer(buffer, 2);

    buffer[0] = (value >> 8) & 0x00FF;
    buffer[1] = value & 0x00FF;

    return write(file, buffer, 2);
}

/**
 * @brief Записать в файл значение типа word по адресу position.
 * 
 * @param file файл, в который происходит запись.
 * @param position смещение, с которого необходимо произвести запись.
 * @param value значение, которое необходимо записать.
 * @return int - 0, если запись прошла успешно, и -1, если нет.
 */
int write_word_to_file_at(FILE_HANDLER file, off_t position, word value) {
    if (lseek(file, position, SEEK_SET) < 0) {
        return -1;
    }

    return write_word_to_file(file, value);
}

/**
 * @brief Считывает из файла значение типа word по адресу position.
 * 
 * @param file файл, из которого происходит чтение.
 * @param position адрес, по которому происходит чтение.
 * @param value указатель на переменную для хранения считываемого значения.
 * @return int - 0, если чтение прошло успешно, и -1, если нет.
 */
int read_word_from_file_at(FILE_HANDLER file, off_t position, word* value) {
    byte buffer[2];
    clear_buffer(buffer, 2);

    if (lseek(file, position, SEEK_SET) < 0) {
        return -1;
    }

    if (read(file, buffer, 2) <= 0) {
        return -1;
    }

    *value = (buffer[0] << 8) + buffer[1];

    return 0;
}

/**
 * @brief Создаёт новый виртуальный диск для хранения файлов.
 * 
 * @param name имя файла, в котором будет срдержаться разметка виртуального диска.
 * @param size размер виртуального диска в блоках.
 * @return int - 0, если всё прошло успешно, и -1, если возникла ошибка.
 */
int create_virtual_disk(char* name, int size) {
    if (size > USHRT_MAX) {
        return -1;
    }

    FILE_HANDLER file = -1;
    word num_of_blocks = (word)size;
    word magic = 0x7171;

    if ((file = open(name, O_CREAT | O_WRONLY | O_TRUNC, 0777)) >= 0) {
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

        // Размер блока.
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
        word file_data_size = size - METADATA_SIZE;

        for (int i = 0; i < file_data_size * BLOCK_SIZE; i++) {
            if (write(file, &filler, 1) < 0) {
                return -1;
            }
        }
    }

    return 0;
}

/**
 * @brief Считывает с виртуального диска запись о файле, расположенную по адресу position.
 * 
 * @param disk дескриптор виртуального диска.
 * @param position адрес записи о файле.
 * @param name указатель на строку для хранения имени файла.
 * @param address номер блока, с которого начинается файл.
 * @param length длина файла в блоках.
 * @return int - 0, если чтение прошло успешно, и -1,если нет.
 */
int read_file_record_at(FILE_HANDLER disk, off_t position, char* name, word* address, word* length) {
    byte buffer[FILE_RECORD_SIZE];
    clear_buffer(buffer, FILE_RECORD_SIZE);

    if (lseek(disk, position, SEEK_SET) < 0) {
        return -1;
    }

    if (read(disk, buffer, FILE_RECORD_SIZE) <= 0) {
        return -1;
    }

    if (name != NULL) {
        memcpy(name, buffer, FILE_NAME_SIZE);
    }
    
    if (address != NULL) {
        *address = (buffer[12] << 8) + buffer[13];
    }

    if (length != NULL) {
        *length = (buffer[14] << 8) + buffer[15];
    }

    return 0;
}

/**
 * @brief Очищает байтовый буфер, устанавливая все его элементы в 0.
 * 
 * @param buffer буфер, который необходимо очистить.
 * @param length длина буфера.
 */
void clear_buffer(byte buffer[], int length) {
    for (int i = 0; i < length; i++) {
        buffer[i] = 0;
    }
}

/**
 * @brief Создаёт на виртуальном диске запись о файле по указанному адресу.
 * 
 * @param disk дескриптор виртуального диска, на который производится запись.
 * @param position адрес, по которому создаётся запись о новом файле.
 * @param name имя файла.
 * @param address адрес стартового блока файла.
 * @param length длина файла в блоках.
 * @return int - 0, если запись прошла успешно, -1, если нет.
 */
int write_file_record_at(FILE_HANDLER disk, off_t position, char* name, word address, word length) {
    if (name == NULL) {
        return -1;
    }

    byte buffer[FILE_RECORD_SIZE];
    clear_buffer(buffer, FILE_RECORD_SIZE);

    if (lseek(disk, position, SEEK_SET) < 0) {
        return -1;
    }

    memcpy(buffer, name, strlen(name) + 1);
    buffer[12] = (address >> 8) & 0x00FF;
    buffer[13] = address & 0x00FF;
    buffer[14] = (length >> 8) & 0x00FF;
    buffer[15] = length & 0x00FF;

    if (write(disk, buffer, FILE_RECORD_SIZE) < 0) {
        return -1;
    }

    return 0;
}

/**
 * @brief Добавляет новый файл на виртуальный диск.
 * 
 * @param diskname имя виртуального диска.
 * @param filename имя добавляемого файла.
 * @return int - 0, если добавление нового файла на диск прошло успешно, и -1, если нет.
 */
int write_file_to_disk(char* diskname, char* filename) {
    FILE_HANDLER disk = -1, file = -1;

    if ((disk = open(diskname, O_RDWR)) < 0) {
        return -1;
    }

    if ((file = open(filename, O_RDONLY, 0)) < 0) {
        return -1;
    }

    // Узнать размер файла.
    off_t size = -1;

    if ((size = fsize(filename)) < 0) {
        return -1;
    }

    printf("file size is %d bytes\n", size);

    // Считать размер блока.
    word block_size = 0;

    if (read_word_from_file_at(disk, BLOCK_SIZE_OFFSET, &block_size) < 0) {
        return -1;
    }

    printf("disk block size is %d bytes\n", block_size);

    // Вычислить размер файла в дисковых блоках.
    word size_in_blocks = size / block_size;

    if (size % block_size > 0) {
        size_in_blocks++;
    }

    printf("file size is %d blocks\n", size_in_blocks);

    // Считать кол-во файлов на диске.
    word files_count = 0;

    if (read_word_from_file_at(disk, FILES_COUNT_OFFSET, &files_count) < 0) {
        return -1;
    }

    printf("disk has %d files in total\n", files_count);

    // Вычислить блок, с которого должны начинаться
    // данные нового файла. Для этого необходимо найти
    // конец последнего записанного на диск файла.
    word last_block = -1, length = -1;
    off_t last_record_address = CATALOG_OFFSET * block_size + (files_count == 0 ? 0 : (files_count - 1)) * FILE_RECORD_SIZE;

    if (read_file_record_at(disk, last_record_address, NULL, &last_block, &length) < 0) {
        return -1;
    }

    if (last_block == 0) {
        last_block = METADATA_SIZE;
    }

    word start_block = last_block + length;
    off_t record_address = files_count == 0 ? last_record_address : (last_record_address + FILE_RECORD_SIZE);

    printf("file record is at 0x%x\n", record_address);

    // Создать в каталоге новую запись с
    // именем файла, его стартовым блоком
    // и длиной (в блоках).
    if (write_file_record_at(disk, record_address, filename, start_block, size_in_blocks) < 0) {
        return -1;
    }

    // Записать данные файла на диск, начиная
    // с адреса стартового блока.
    off_t data_address = start_block * block_size;
    byte* buffer = (byte*)malloc(block_size);

    printf("file data is at 0x%x\n", data_address);

    if (lseek(disk, data_address, SEEK_SET) < 0) {
        return -1;
    }

    for (int i = start_block; i < start_block + size_in_blocks; i++) {
        // Считать данные из файла.
        if (read(file, buffer, block_size) < 0) {
            free(buffer);
            return -1;
        }

        // Записать данные на виртуальный диск.
        if (write(disk, buffer, block_size) < 0) {
            free(buffer);
            return -1;
        }

        // Очистить буфер.
        clear_buffer(buffer, block_size);
    }

    free(buffer);

    // Повысить на 1 счётчик числа файлов на диске.
    files_count++;

    if (write_word_to_file_at(disk, FILES_COUNT_OFFSET, files_count) < 0) {
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