SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o array.o str.o pair.o predicates.o alloc.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -D OS -D X32 -fno-stack-protector -fstack-check=no
BASELISP=boot/lib.lsp boot/list.lsp boot/array.lsp boot/str.lsp boot/hash.lsp boot/oop.lsp
GRAPHICS=$(BASELISP) boot/bgr.lsp boot/graphics/matrix.lsp boot/graphics/backend.lsp boot/graphics/frontend.lsp
FS=$(BASELISP) boot/fs/block.lsp boot/fs/fs.lsp boot/fs/fat32fs.lsp boot/fs/fat.lsp boot/fs/dir.lsp boot/mbr.lsp boot/fs/fat32test.lsp

all: $(SRC) lisp

lisp: /tmp/graphics.lsp # здесь меняем набор библиотек
	cp $^ /tmp/lisp.lsp

/tmp/baselisp.lsp: $(BASELISP)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/graphics.lsp: $(GRAPHICS)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/fs.lsp: $(FS)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/inter: $(SRC)
	gcc $^ -o $@
