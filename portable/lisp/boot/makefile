include common.mak

SUBDIRS=graphics

# модульные тесты двумерного вектора
vec2-test: $(BASELISP) $(TEST) test/vec2.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp

# модульные тесты очередей
queue-test: $(BASELISP) $(TEST) test/queue.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

# модульные тесты парсера
parser-test: $(BASELISP) parser.lsp $(TEST) test/parser.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp

# модульные тесты цикла событий
key-test: $(BASELISP) sys/keyboard.lsp test/key.lsp sys/events.lsp 
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp

# модульные тесты коды Хаффмана
huffman-test: $(BASELISP) bin-reader.lsp bin-tree.lsp huffman.lsp $(TEST) test/huffman.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

# модульные тесты стандартной библиотеки
lib-test: $(BASELISP) $(TEST) test/lib.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты функций списков
list-test: $(BASELISP) $(TEST) test/list.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты ООП
oop-test: $(BASELISP) $(TEST) test/oop.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты строк
str-test: $(BASELISP) $(TEST) test/str.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты чтение двоичного потока данных
bin-reader: $(BASELISP) bin-reader.lsp $(TEST) test/bin-reader.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты двоичное дерево
bin-tree-test: $(BASELISP) bin-tree.lsp $(TEST) test/bin-tree.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

# базовый набор
/tmp/baselisp.lsp: $(BASELISP) 
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
