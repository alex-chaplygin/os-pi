SUBDIRS=portable x86

all: bin/kernel

bin/kernel: $(SUBDIRS)
	ld -m elf_i386 -T link.ld -o bin/kernel x86/*.o portable/*.o portable/lisp/*.o

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY:	/bin/kernel $(SUBDIRS)

run: bin/kernel
	qemu-system-i386 -hda disk.qcow2 -kernel bin/kernel

debug:
	qemu-system-i386 -hda test.img -kernel bin/kernel -s -S
clean:
	rm bin/kernel portable/*.o portable/lisp/*.o x86/*.o
tags:
	etags include/portable/*.h include/x86/*.h portable/*.c x86/*.c
