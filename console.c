#include "console.h"

char *videoptr = (char*)0xb8000;
int printPtr = 0;

void kprint(char *str){
    
    while (*str != '\0'){



        int currRow = printPtr / 2 / CONSOLE_COLS;
        if (*str == '\n'){
            int nextRow = CONSOLE_COLS * (currRow + 1);

            printPtr = nextRow * 2;
            
        } else {
            if (printPtr/2 >= CONSOLE_ROWS * CONSOLE_COLS){
                printPtr = 0;
            }
            videoptr[printPtr++] = *str;

        //printPtr++;
            videoptr[printPtr++] = 0x07;
        }
        
        //videoptr++;

        str++;

        
    }

    
}


void console_clear(){
  int pos = 0;
  for (int i = 0; i < 80 * 25; i++) {
    videoptr[pos++] = 0;
    videoptr[pos++] = 0;
  }
}