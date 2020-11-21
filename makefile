OBJS=main.o console.o x86.o libc.o proc.o idt.o isr.o syscall.o irq.o timer.o keyboard.o mouse.o gdt.o mem.o mem_check.o
CFLAGS=-m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch
CC=gcc
INC=-I include

kernel: $(OBJS)
	ld -m elf_i386 -T link.ld -o bin/kernel $(OBJS)

mem_check.o:
	nasm -f elf32 x86/mem_check.asm -o mem_check.o

x86.o:
	nasm -f elf32 x86/x86.asm -o x86.o

main.o:
	${CC} ${CFLAGS} ${INC} -o main.o -c portable/main.c

console.o:
	${CC} ${CFLAGS} ${INC} -o console.o -c portable/console.c

libc.o:
	${CC} ${CFLAGS} ${INC} -o libc.o -c portable/libc.c

proc.o:
	${CC} ${CFLAGS} ${INC} -o proc.o -c portable/proc.c

idt.o:
	${CC} ${CFLAGS} ${INC} -o idt.o -c x86/idt.c

isr.o:
	${CC} ${CFLAGS} ${INC} -o isr.o -c x86/isr.c

syscall.o:
	${CC} ${CFLAGS} ${INC} -o syscall.o -c portable/syscall.c

irq.o:
	${CC} ${CFLAGS} ${INC} -o irq.o -c x86/irq.c

timer.o:
	${CC} ${CFLAGS} ${INC} -o timer.o -c x86/timer.c

keyboard.o:
	${CC} ${CFLAGS} ${INC} -o keyboard.o -c x86/keyboard.c

mouse.o:
	${CC} ${CFLAGS} ${INC} -o mouse.o -c x86/mouse.c

gdt.o:
	${CC} ${CFLAGS} ${INC} -o gdt.o -c x86/gdt.c

mem.o:
	${CC} ${CFLAGS} ${INC} -o mem.o -c portable/mem.c

run:
	qemu-system-i386 -kernel bin/kernel -m 4M

debug:
	qemu-system-i386 -kernel bin/kernel -s -S
