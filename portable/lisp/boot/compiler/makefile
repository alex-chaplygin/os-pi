include ../common.mak
VPATH := ..
COMPILER = $(BASELISP) macro.lsp compiler.lsp generator.lsp assembler.lsp
SOURCE = ../lib.lsp
TARGET = /tmp/compile

compiler-test: $(COMPILER) ../test.lsp test-compiler.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

test: $(COMPILER) ../test.lsp vm.lsp test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

.PHONY: target
target: $(SOURCE)
	echo "" >$(TARGET)
	echo "(dolist (x (block compiler (generate (compile '(progn" >>$(TARGET)
	cat $(SOURCE) >>$(TARGET)
	echo "))))) (print x))" >>$(TARGET)

compile-file: $(COMPILER) $(TARGET)
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

