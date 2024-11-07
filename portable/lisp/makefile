SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o array.o str.o pair.o predicates.o alloc.o mpa.o error.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -D OS -D X32 -DDEBUG -fno-stack-protector -fstack-check=no
BASELISP=boot/lib.lsp boot/list.lsp boot/array.lsp boot/str.lsp boot/hash.lsp boot/oop.lsp boot/queue.lsp
GRAPHICS=$(BASELISP) boot/bgr.lsp boot/graphics/matrix.lsp boot/graphics/backend.lsp boot/graphics/frontend.lsp
TEST=boot/test.lsp
GRAPHICS_TEST=$(GRAPHICS) $(TEST) boot/graphics/test/backend.lsp
FS=$(BASELISP) boot/fs/block.lsp boot/fs/fs.lsp boot/fs/fat32fs.lsp boot/fs/fat.lsp boot/fs/dir.lsp boot/mbr.lsp boot/fs/fat32test.lsp
KEYBOARD=$(BASELISP) boot/sys/keyboard.lsp

all: $(SRC) lisp

lisp: /tmp/baselisp.lsp # здесь меняем набор библиотек
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

/tmp/key.lsp: $(KEYBOARD)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

graphics: $(GRAPHICS_TEST)
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp

lib-test: $(BASELISP) $(TEST) boot/test/lib.lsp
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp

list-test: $(BASELISP) $(TEST) boot/test/list.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

queue: $(BASELISP) $(TEST) boot/test/queue.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
