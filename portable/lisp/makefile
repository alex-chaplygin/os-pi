include boot/common.mak
SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o array.o str.o pair.o predicates.o alloc.o mpa.o error.o bind.o vm.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -MD -D OS -D X32 -DVM -fno-stack-protector -fstack-check=no

all: $(SRC) subdirs vmlisp

subdirs:
	$(MAKE) -C boot
	$(MAKE) -C boot/graphics
	$(MAKE) -C boot/gui
	$(MAKE) -C boot/game
	$(MAKE) -C boot/fs
	$(MAKE) -C boot/sys

include $(wildcard *.d) # автоматическая зависимость от заголовочных файлов

lisp: /tmp/pci.lsp # здесь меняем набор библиотек
	cp $^ /tmp/lisp.lsp

# компиляция кода в ядро
vmlisp: /tmp/gui-ascii.lsp
	make -C test /tmp/test_comp
	cat $^ | head -n -1 >/tmp/compile
	cd boot/compiler; ./compile
	@/tmp/test_comp </tmp/bin >/tmp/1 || {\
             cat /tmp/1;\
             exit 1;\
	}
	cat /tmp/1 | sed -e "s/CONSTB/#(/" -e "s/CONSTE/)/" | tr '\n' ' ' >/tmp/lisp.lsp

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
