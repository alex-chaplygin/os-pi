#include <x86/disk.h>
#include <portable/types.h>

byte* disk_read_block(byte *buffer, int block_num)
{
    byte bufData[BUFFER_SIZE];
    byte block[BUFFER_SIZE] = "11text.txt";
    byte path[BUFFER_SIZE] = "1text.txt";
    byte file[BUFFER_SIZE] = "Temporary content of text.txt file";

    if(block_num == 0)
    {
        buffer = block;
        return buffer;
    }
    if(block_num == 1)
    {
        buffer = path;
        return buffer;
    }
    if(block_num == 11)
    {
        buffer = file;
        return buffer;
    }

    buffer = bufData;
    return buffer;
}