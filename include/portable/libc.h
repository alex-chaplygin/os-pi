#include <portable/types.h>

#define NULL 0

void memcpy( unsigned char* destptr, unsigned char*  srcptr, int num );
char *int_to_str(int n);
char *int_to_str_hex(unsigned int num);
unsigned int str_hex_to_int(char* s);
void kprint(char *str, ...);
void clear_buffer(byte* buffer, int offset, int count);
