OBJS=x86.o idt.o isr.o irq.o keyboard.o gdt.o console.o sys.o graphics.o
CFLAGS=-D X32 -DVM -g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -I../portable/lisp -fno-stack-protector -fstack-check=no

all: $(OBJS)

mem_check.o: mem_check.asm
	nasm -f elf32 -g mem_check.asm -o mem_check.o

x86.o: x86.asm /tmp/lisp.lsp
	nasm -f elf32 -g x86.asm -o x86.o

syslib.o: syslib.asm
	nasm -f elf32 -g syslib.asm -o syslib.o
