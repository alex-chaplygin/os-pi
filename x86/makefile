OBJS=x86.o idt.o isr.o irq.o timer.o keyboard.o mouse.o gdt.o mem_check.o
CFLAGS=-m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include

all: $(OBJS)

mem_check.o: mem_check.asm
	nasm -f elf32 mem_check.asm -o mem_check.o

x86.o: x86.asm
	nasm -f elf32 x86.asm -o x86.o

