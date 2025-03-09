include ../common.mak
VPATH := ..
COMPILER = $(BASELISP) macro.lsp compiler.lsp generator.lsp assembler.lsp
SOURCE = ../lib.lsp ../list.lsp ../test.lsp ../test/lib.lsp
TARGET = /tmp/compile

compiler-test: $(COMPILER) ../test.lsp test-compiler.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

test: $(COMPILER) ../test.lsp vm.lsp test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

.PHONY: target
$(TARGET): $(SOURCE)
	make target

target: $(SOURCE)
	echo "" >$(TARGET)
	echo "(print (block compiler (let ((comp (compile '(progn" >>$(TARGET)
	cat $(SOURCE) >>$(TARGET)
	echo ")))) (print comp) (let ((gen (generate comp))) (dolist (l gen) (print l)) (print (assemble gen))) *consts*)))" >>$(TARGET)

compile-file: $(COMPILER) $(TARGET)
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

