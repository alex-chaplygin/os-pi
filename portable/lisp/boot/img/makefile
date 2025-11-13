include ../common.mak

SHELL = bash
VPATH := ..

# тест JPEG
test-jpeg: $(BASELISP) $(PARSER) jpeg/jpeg.lsp jpeg/jpeg-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp


# тест PNG
test-png: $(BASELISP) $(PARSER) png.lsp png-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
