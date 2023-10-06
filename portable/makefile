OBJS=main.o libc.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -I./lisp
SUBDIRS=lisp

all: $(OBJS) lisp

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY:	$(SUBDIRS)
