SHELL=bash
SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o array.o str.o pair.o predicates.o alloc.o mpa.o error.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include -D OS -D X32 -DDEBUG -fno-stack-protector -fstack-check=no
BASELISP=boot/lib.lsp boot/list.lsp boot/array.lsp boot/str.lsp boot/hash.lsp boot/oop.lsp boot/queue.lsp boot/vec2.lsp
GRAPHICS=$(BASELISP) boot/math.lsp boot/bgr.lsp boot/graphics/matrix.lsp boot/graphics/backend.lsp boot/graphics/frontend.lsp
TEST=boot/test.lsp
ASCII_GRAPHICS=$(GRAPHICS) boot/graphics/ascii-backend.lsp
FS=$(BASELISP) boot/ata.lsp boot/mbr.lsp boot/fs/block.lsp boot/fs/fs.lsp boot/fs/fat32fs.lsp boot/fs/fat.lsp boot/fs/dir.lsp boot/fs/fat32test.lsp
KEYBOARD=$(BASELISP) boot/sys/keyboard.lsp boot/sys/events.lsp

all: $(SRC) lisp

lisp: /tmp/baselisp.lsp # здесь меняем набор библиотек
	cp $^ /tmp/lisp.lsp
# базовый набор
/tmp/baselisp.lsp: $(BASELISP) 
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
# графика
/tmp/graphics.lsp: $(GRAPHICS)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
# графика через ASCII в текстовом видеорежиме
/tmp/ascii-graphics.lsp: $(ASCII_GRAPHICS) boot/graphics/test/ascii-backend.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
# файловая система
/tmp/fs.lsp: $(FS)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
# тест клавиш
/tmp/key.lsp: $(KEYBOARD)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
# тест печати чисел с плавающей точкой
/tmp/float.lsp: $(BASELISP)
	cat $^ >$@ ;
	echo ""
	echo "(print 0.0)" >>$@ ;
	echo "(print (sin 0.0))" >>$@ ;
	echo "(print (cos 0.0))" >>$@ ;
	echo "(print (round 1.1))" >>$@ ;
	echo "(print (round 1.5))" >>$@ ;
	echo -e -n "\xff" >>$@
# модульные тесты графики
graphics-test: $(GRAPHICS) $(TEST) boot/graphics/test/backend.lsp
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты стандартной библиотеки
lib-test: $(BASELISP) $(TEST) boot/test/lib.lsp
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты функций списков
list-test: $(BASELISP) $(TEST) boot/test/list.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты ООП
oop-test: $(BASELISP) $(TEST) boot/test/oop.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты очередей
queue-test: $(BASELISP) $(TEST) boot/test/queue.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты двоичное дерево
bin-tree-test: $(BASELISP) boot/bin-tree.lsp $(TEST) boot/test/bin-tree.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты коды Хаффмана
huffman-test: $(BASELISP) boot/bin-tree.lsp boot/huffman.lsp $(TEST) boot/test/huffman.lsp
	make -C test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты цикла событий
key-test: $(BASELISP) boot/sys/keyboard.lsp boot/test/key.lsp boot/sys/events.lsp 
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты двумерного вектора
vec2-test: $(BASELISP) $(TEST) boot/test/vec2.lsp
	make -C test /tmp/lisp
	cat $^ | /tmp/lisp
