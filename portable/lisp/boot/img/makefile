include ../common.mak
JPEG=$(HUFFMAN) jpeg/dct.lsp jpeg/data-unit.lsp jpeg/utils.lsp jpeg/jpeg.lsp
TIFF=$(HUFFMAN) t4.lsp tiff.lsp
SHELL = bash
VPATH := ..

# тест JPEG
test-jpeg: $(BASELISP) $(PARSER) $(JPEG) $(TEST) jpeg/jpeg-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

# тест PNG
test-png: $(BASELISP) $(PARSER) $(DEFLATE) png.lsp $(TEST) png-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

# тест TIFF
test-tiff: $(BASELISP) $(PARSER) $(TIFF) tiff-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

# тест BMP
test-bmp: $(BASELISP) $(PARSER) bmp.lsp bmp-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
