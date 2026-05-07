CODE=code.asm init.asm macro.inc prim.asm func.asm arith.asm gc.asm
FLAGS=-f elf32 -g -w-number-overflow

/tmp/code.o: $(CODE) /tmp/source
	nasm code.asm $(FLAGS) -o /tmp/code.o
