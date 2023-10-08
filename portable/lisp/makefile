SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include
LISP=boot/lib.lsp

all: $(SRC) /tmp/lisp

/tmp/lisp: $(LISP)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/inter: $(SRC)
	gcc $^ -o $@
