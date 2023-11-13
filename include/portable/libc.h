#include <portable/types.h>

#define NULL 0

void memcpy(void* destptr, const void* srcptr, unsigned int size);
void printf(char *str, ...);
void clear_buffer(byte* buffer, int offset, int count);
int strcmp(char *str_one, char *str_two);
