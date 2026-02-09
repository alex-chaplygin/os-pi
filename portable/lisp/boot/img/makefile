include ../common.mak
JPEG=$(HUFFMAN) jpeg/dct.lsp jpeg/data-unit.lsp jpeg/utils.lsp jpeg/jpeg.lsp
TIFF=$(HUFFMAN) t4.lsp tiff.lsp
SHELL = bash
VPATH := ..

# тест JPEG
jpeg-test: $(BASELISP) $(PARSER) $(JPEG) $(TEST) jpeg/jpeg-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ >/tmp/jpeg
	/tmp/lisp </tmp/jpeg

# тест PNG
png-test: $(BASELISP) $(PARSER) $(DEFLATE) png.lsp $(TEST) png-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ >/tmp/png
	/tmp/lisp </tmp/png

# тест TIFF
tiff-test: $(BASELISP) $(PARSER) $(TIFF) tiff-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

# тест BMP
bmp-test: $(BASELISP) $(PARSER) bmp.lsp bmp-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
