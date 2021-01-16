SUBDIRS=x86 portable

bin/kernel: $(SUBDIRS)
	ld -m elf_i386 -T link.ld -o bin/kernel x86/*.o portable/*.o

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY:	/bin/kernel $(SUBDIRS)

run:
	qemu-system-i386 -kernel bin/kernel

debug:
	qemu-system-i386 -kernel bin/kernel -s -S
clean:
	rm bin/kernel portable/*.o x86/*.o
tags:
	etags include/portable/*.h include/x86/*.h portable/*.c x86/*.c
