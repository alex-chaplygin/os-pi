OBJS=main.o libc.o init.o float.o
CFLAGS=-D X32 -D VM -g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -Wno-pointer-to-int-cast -I../include -I./lisp -fno-stack-protector -fstack-check=no
SUBDIRS=lisp

all: $(OBJS) lisp

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY:	$(SUBDIRS)
