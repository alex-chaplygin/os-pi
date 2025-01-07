include boot/common.mak
SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o array.o str.o pair.o predicates.o alloc.o mpa.o error.o bind.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -MD -D OS -D X32 -DDEBUG -fno-stack-protector -fstack-check=no
FS=$(BASELISP) boot/ata.lsp boot/mbr.lsp boot/fs/block.lsp boot/fs/fs.lsp boot/fs/fat32fs.lsp boot/fs/fat.lsp boot/fs/dir.lsp boot/fs/fat32test.lsp

all: $(SRC) subdirs lisp

subdirs:
	$(MAKE) -C boot
	$(MAKE) -C boot/graphics
	$(MAKE) -C boot/game

include $(wildcard *.d) # автоматическая зависимость от заголовочных файлов

lisp: /tmp/baselisp.lsp # здесь меняем набор библиотек
	cp $^ /tmp/lisp.lsp

# файловая система
/tmp/fs.lsp: $(FS)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
# тесты jpeg
jpeg-test: $(BASELISP) boot/bin-reader.lsp boot/bin-tree.lsp boot/huffman.lsp boot/img/jpeg/data-unit.lsp boot/img/jpeg/main.lsp boot/test/jpeg.lsp
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты компилятора
compiler-test: $(COMPILER) $(TEST) boot/compiler/test-compiler.lsp
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты fat
fat-test: $(BASELISP) boot/fs/fat.lsp $(TEST) boot/fs/test/fat.lsp
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp
