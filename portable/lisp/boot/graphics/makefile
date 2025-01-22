include ../common.mak common.mak

SHELL = bash
VPATH := ..

all: /tmp/graphics.lsp /tmp/ascii-graphics.lsp

# графика
/tmp/graphics.lsp: $(GRAPHICS) test/backend.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

# графика через ASCII в текстовом видеорежиме
/tmp/ascii-graphics.lsp: $(ASCII_GRAPHICS) test/ascii-backend.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

# модульные тесты графики
graphics-test: $(GRAPHICS) $(TEST) test/unit-backend.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

# модульные тесты изображений
image-test: $(GRAPHICS) $(TEST) test/image.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
