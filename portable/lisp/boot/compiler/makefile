include ../common.mak
VPATH := ..
COMPILER = $(BASELISP) common.lsp macro.lsp compiler.lsp generator.lsp assembler.lsp
SOURCE = ../lib.lsp ../list.lsp ../str.lsp ../hash.lsp ../fa/nfa.lsp ../regex.lsp ../test.lsp ../test/regex.lsp
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
	echo "(print (catch 'compiler (let ((comp (compile '(progn" >>$(TARGET)
	cat $(SOURCE) >>$(TARGET)
	echo ")))) (let ((gen (generate comp))) (print (assemble gen))) *consts*)))" >>$(TARGET)

compile-file: $(COMPILER) $(TARGET)
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

