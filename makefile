SUBDIRS=x86 portable

bin/kernel: portable/*.o x86/*.o
	ld -m elf_i386 -T link.ld -o bin/kernel x86/*.o portable/*.o

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY:	/bin/kernel $(SUBDIRS)

run:
	qemu-system-i386 -kernel bin/kernel -m 4M

debug:
	qemu-system-i386 -kernel bin/kernel -s -S
