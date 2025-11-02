include ../common.mak

SHELL = bash
VPATH := ..

# графика
test-jpeg: $(BASELISP) $(PARSER) jpeg/jpeg.lsp jpeg/jpeg-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
