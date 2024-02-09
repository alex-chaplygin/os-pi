SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o array.o str.o pair.o predicates.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include
LISP=boot/lib.lsp boot/bgr.lsp boot/ata.lsp boot/mbr.lsp boot/fat32.lsp
#boot/hash.lsp boot/graphics/matrix.lsp boot/graphics/backend.lsp boot/graphics/frontend.lsp

all: $(SRC) /tmp/lisp.lsp

/tmp/lisp.lsp: $(LISP)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/inter: $(SRC)
	gcc $^ -o $@
