OBJS=main.o console.o x86.o libc.o proc.o idt.o isr.o syscall.o irq.o timer.o keyboard.o mouse.o gdt.o mem.o mem_check.o
CFLAGS=-m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch

kernel: $(OBJS)
	ld -m elf_i386 -T link.ld -o kernel $(OBJS)

mem_check.o: mem_check.asm
	nasm -f elf32 mem_check.asm -o mem_check.o

x86.o: x86.asm
	nasm -f elf32 x86.asm -o x86.o
run:
	qemu-system-i386 -kernel kernel -m 4M
debug:
	qemu-system-i386 -kernel kernel -s -S
