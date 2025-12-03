include ../common.mak

SHELL = bash
VPATH := ..

# тест JPEG
test-jpeg: $(BASELISP) $(PARSER) ../bin-tree.lsp ../huffman.lsp jpeg/huffman.lsp jpeg/jpeg.lsp jpeg/jpeg-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

# тест PNG
test-png: $(BASELISP) $(PARSER) ../deflate.lsp png.lsp png-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

# тест TIFF
test-tiff: $(BASELISP) $(PARSER) tiff.lsp tiff-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

# тест BMP
test-bmp: $(BASELISP) $(PARSER) bmp.lsp bmp-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
