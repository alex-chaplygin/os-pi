include ../common.mak
SHELL = bash
VPATH := ..
COMPILER = $(BASELISP) common.lsp macro.lsp compiler.lsp generator.lsp assembler.lsp 
SOURCE = /tmp/regex
#../lib.lsp ../list.lsp ../hash.lsp ../array.lsp common.lsp macro.lsp compiler.lsp generator.lsp assembler.lsp /tmp/c
TARGET = /tmp/compile

compiler-test: $(COMPILER) ../test.lsp test-compiler.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

lexer-test: $(BASELISP) ../stream.lsp ../posstream.lsp ../parser.lsp lexer.lsp ../test.lsp test-lexer.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

parser-test: $(BASELISP) ../stream.lsp ../posstream.lsp ../parser.lsp lexer.lsp parser.lsp ../test.lsp test-parser.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

test: $(COMPILER) ../test.lsp vm.lsp test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

.PHONY: target compile compiler
$(TARGET): $(SOURCE)
	make target

# создания шаблона для компиляции
target: $(SOURCE)
	cat comp_begin >$(TARGET)
	cat $(SOURCE) >>$(TARGET)
	cat comp_end >>$(TARGET)

# компиляция через интерпретатор
# предварительно нужно сделать make target
compile-file: $(COMPILER) $(TARGET)
	make -C ../../test /tmp/lisp
	cat $^ >/tmp/comp
	cat $^ | /tmp/lisp >/tmp/1
	@[ "$$(head -n 1 /tmp/1)" == "\"Compiler error:\"" ] && cat /tmp/1 || cat /tmp/1 | sed -e "s/CONSTB/#(/" -e "s/CONSTE/)/" | tr '\n' ' ' >/tmp/bin

# компиляция через готовый компилятор и запуск готовой программы
compile-test: $(TARGET)
	make -C ../../test /tmp/test_comp
	cat $(SOURCE) > $(TARGET) 
	./compile bin/compiler
	@/tmp/test_comp </tmp/bin >/tmp/1 || {\
             cat /tmp/1;\
             exit 1;\
	}
	@[ "$$(head -n 1 /tmp/1)" == "\"Compiler error:\"" ] && cat /tmp/1 || cat /tmp/1 | sed -e "s/CONSTB/#(/" -e "s/CONSTE/)/" | sed -E 's/\\(w|d|D|s|S|\+|\^|\*|\.|\?)/\\\\\1/g' | sed -E 's/#\\\\(\.|\?|w|d|D|s|S|\^|\*|\+)/#\\\1/g' | tr '\n' ' ' >/tmp/res
	@[ "$$(head -n 1 /tmp/1)" == "\"Compiler error:\"" ] && echo "not compiled" || /tmp/test_comp </tmp/res

# компиляция через готовый компилятор и выдача отладочной информации
compile-debug: $(TARGET)
	make -C ../../test /tmp/test_comp
	make -C ../../test /tmp/test_comp_debug
	cat $(SOURCE) > $(TARGET) 
	./compile bin/debug
	@/tmp/test_comp </tmp/bin >/tmp/debug

# компиляция самого компилятора
compiler: $(COMPILER)
	make -C ../../test /tmp/test_comp
	cat $^ >$(TARGET)
	cat comp_begin >>$(TARGET)
	echo " *TARGET* " >>$(TARGET)
	cat comp_end >>$(TARGET)
	./compile bin/compiler
	/tmp/test_comp </tmp/bin | sed -e "s/CONSTB/#(/" -e "s/CONSTE/)/" | tr '\n' ' ' | sed -e "s/#( )/CONSTB CONSTE/" >/tmp/comp
	cat /tmp/comp | sed -e "s/\*NODEBUG\*/*DEBUG*/" >/tmp/debug
