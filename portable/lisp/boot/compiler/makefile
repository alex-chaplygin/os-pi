include ../common.mak
VPATH := ..
COMPILER = $(BASELISP) macro.lsp compiler.lsp generator.lsp assembler.lsp

compiler-test: $(COMPILER) ../test.lsp test-compiler.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

test: $(COMPILER) ../test.lsp vm.lsp test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
