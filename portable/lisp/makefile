SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o array.o str.o pair.o predicates.o alloc.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -D OS -D X32
LISP=boot/lib.lsp boot/array.lsp boot/bgr.lsp boot/ata.lsp boot/hash.lsp boot/oop.lsp boot/fs/block.lsp boot/fs/fs.lsp boot/fs/fat32fs.lsp boot/mbr.lsp
#boot/hash.lsp boot/graphics/matrix.lsp boot/graphics/backend.lsp boot/graphics/frontend.lsp

all: $(SRC) /tmp/lisp.lsp

/tmp/lisp.lsp: $(LISP)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/inter: $(SRC)
	gcc $^ -o $@
