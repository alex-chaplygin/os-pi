include ../common.mak
VPATH := ..
COMPILER = $(BASELISP) common.lsp macro.lsp compiler.lsp generator.lsp assembler.lsp
SOURCE = ../lib.lsp ../lib.lsp ../list.lsp ../hash.lsp ../test.lsp ../compiler/common.lsp ../compiler/macro.lsp ../compiler/compiler.lsp ../compiler/generator.lsp ../compiler/assembler.lsp
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
	echo ")))) (let* ((gen (assemble (generate comp))) (s (-- (array-size gen)))) (print s) (for i 0 s (print (aref gen i)))) *consts*))) (print *global-variables-count*)" >>$(TARGET)

compile-file: $(COMPILER) $(TARGET)
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp | sed -e "s/^(T/#(T/" | tr '\n' ' ' >/tmp/bin

