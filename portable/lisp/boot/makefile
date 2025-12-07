include common.mak
KEYBOARD=$(BASELISP) sys/keyboard.lsp
SHELL=bash
SUBDIRS=graphics

all: /tmp/baselisp.lsp /tmp/key.lsp

all-tests: lib-test list-test oop-test str-test array-test vec2-test queue-test parser-test huffman-test math-test bin-reader bin-tree-test regex-test

# модульные тесты двумерного вектора
vec2-test: $(BASELISP) math.lsp $(TEST) test/vec2.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp

# модульные тесты очередей
queue-test: $(BASELISP) $(TEST) test/queue.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp

# модульные тесты парсера
parser-test: $(BASELISP) stream.lsp parser.lsp $(TEST) test/parser.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp

# модульные тесты коды Хаффмана
huffman-test: $(BASELISP) bin-reader.lsp bin-tree.lsp stream.lsp huffman.lsp $(TEST) test/huffman.lsp
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
# модульные тесты хеш таблиц
hash-test: $(BASELISP) $(TEST) test/hash.lsp
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
# модульные тесты массивов
array-test: $(BASELISP) $(TEST) test/array.lsp
	make -C ../test /tmp/repl_lisp
	cat $^ | /tmp/repl_lisp
# модульные тесты математики
math-test: $(BASELISP) math.lsp $(TEST) test/math.lsp
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
# модульные тесты потока
stream-test: $(BASELISP) stream.lsp $(TEST) test/stream.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты потока вывода
ostream-test: $(BASELISP) ostream.lsp $(TEST) test/ostream.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты множества
set-test: $(BASELISP) set.lsp $(TEST) test/set.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты потока с позицией
posstream-test: $(BASELISP) stream.lsp posstream.lsp $(TEST) test/posstream.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты НКА
nfa-test: $(BASELISP) fa/nfa.lsp $(TEST) test/nfa.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты Deflate
deflate-test: $(BASELISP) $(PARSER) deflate.lsp $(TEST) test/deflate.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp
# модульные тесты двоичное дерево
regex-test: $(BASELISP) fa/nfa.lsp stream.lsp parser.lsp regex.lsp $(TEST) test/regex.lsp
	make -C ../test /tmp/lisp
	cat $^ | /tmp/lisp

# базовый набор
/tmp/baselisp.lsp: $(BASELISP) 
	cat $^ >$@
	@echo -e -n "\xff" >>$@
# тест клавиатуры
/tmp/key.lsp: $(KEYBOARD) test/key.lsp
	cat $^ >$@
	@echo -e -n "\xff" >>$@
