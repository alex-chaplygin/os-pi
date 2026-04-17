CODE=code.asm init.asm macro.inc prim.asm func.asm arith.asm

/tmp/code.o: $(CODE) /tmp/source
	nasm code.asm -f elf32 -g -o /tmp/code.o
