SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o array.o str.o pair.o predicates.o alloc.o mpa.o error.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -D OS -D X32 -DDEBUG -fno-stack-protector -fstack-check=no
BASELISP=boot/lib.lsp boot/list.lsp boot/array.lsp boot/str.lsp boot/hash.lsp boot/oop.lsp boot/queue.lsp
GRAPHICS=$(BASELISP) boot/math.lsp boot/bgr.lsp boot/graphics/matrix.lsp boot/graphics/backend.lsp boot/graphics/frontend.lsp
TEST=boot/test.lsp
GRAPHICS_TEST=$(GRAPHICS) $(TEST) boot/graphics/test/backend.lsp
ASCII_GRAPH=$(GRAPHICS) boot/graphics/ascii-graph.lsp
ASCII_GUI=$(GRAPHICS) boot/gui/ascii-backend.lsp
FS=$(BASELISP) boot/fs/block.lsp boot/fs/fs.lsp boot/fs/fat32fs.lsp boot/fs/fat.lsp boot/fs/dir.lsp boot/mbr.lsp boot/fs/fat32test.lsp
KEYBOARD=$(BASELISP) boot/sys/keyboard.lsp boot/sys/events.lsp

all: $(SRC) lisp

lisp: /tmp/baselisp.lsp # здесь меняем набор библиотек
	cp $^ /tmp/lisp.lsp

/tmp/baselisp.lsp: $(BASELISP)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/graphics.lsp: $(GRAPHICS)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/ascii-graph.lsp: $(ASCII_GRAPH) boot/graphics/test/ascii-graph.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/ascii-gui.lsp: $(ASCII_GUI) boot/gui/test/ascii-backend.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/fs.lsp: $(FS)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/key.lsp: $(KEYBOARD)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

graphics-test: $(GRAPHICS_TEST)
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp

lib-test: $(BASELISP) $(TEST) boot/test/lib.lsp
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp

list-test: $(BASELISP) $(TEST) boot/test/list.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

oop-test: $(BASELISP) $(TEST) boot/test/oop.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

queue-test: $(BASELISP) $(TEST) boot/test/queue.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

bin-tree-test: $(BASELISP) boot/bin-tree.lsp $(TEST) boot/test/bin-tree.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

key-test: $(BASELISP) boot/sys/keyboard.lsp boot/test/key.lsp boot/sys/events.lsp 
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp
