#include <stdio.h>
#include <syscall.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h> 
#include <string.h>
#define MAXFILENAMELENGTH 255 
#define BUFSIZE 512 
#define PMODE 0644

int main (int argc, char *argv[]) {
		char input[MAXFILENAMELENGTH] = {""}, output[MAXFILENAMELENGTH] = {""}, buf[BUFSIZE];
		int f1 = 0, f2 = 1, n, size = 0, iskip = 0, oskip = 0;

		getArgs(argc, argv, input, output, &size, &iskip, &oskip);

	    if (strlen(input) > 0) {
			if ((f1 = open(input, O_RDONLY, 0)) == -1) {
					printf("CP:CAN'T OPEN %s", input);
					return 1;
				}
	    } 	 
   
		if (strlen(output) > 0) {
			if ((f2 = open(output, O_WRONLY)) == -1) { 
					if ((f2 = creat(output, PMODE)) == -1) {
						printf("CP: CAN'T CREATE %s, mode %03o", output, PMODE);
						return 1;
					} 
				}
		}

		if (iskip > 0 && f1 != 0) {
			if (lseek(f1, iskip, 0) == -1) {
				printf("CP: INPUT_SKIP ERROR %d", errno);
				return 1;
			}
		}
			
		if (oskip > 0 && f2 != 1) {
			if (lseek(f2, oskip, 0) == -1) {
				printf("CP: OUTPUT_SKIP ERROR %d", errno);
				return 1;
			}
		}
			
	    if (size > 0) {
			for ( int i = 0; i < size && ((n = read(f1, buf, 1)) > 0); i++) {
				if (write(f2, buf, 1) != n) {
					printf("CP: WRITE ERROR %s", output);
					return 1;
				}
			}		
	    } else
			while ((n = read(f1, buf, BUFSIZE)) > 0) {
				int byteWrite = write(f2, buf, n);
				if (byteWrite != n) {
					printf("CP: WRITE ERROR %s", output);
					return 1;
				}
			}
				
        close(f1);
	close(f2);	
	return 0;
}

void getArgs(int argc, char *argv[], char *input, char *output, int *size, int *iskip, int *oskip) {
	for (int i = 1; i < argc; i++) {
		if (strlen(input) == 0) sscanf(argv[i], "input=%s", input);
		if (strlen(output) == 0) sscanf(argv[i], "output=%s", output);
		if (*size == 0) sscanf(argv[i], "size=%u", size);
		if (*iskip == 0) sscanf(argv[i], "input_skip=%u", iskip);
		if (*oskip == 0) sscanf(argv[i], "output_skip=%u", oskip);
	}
}


  
