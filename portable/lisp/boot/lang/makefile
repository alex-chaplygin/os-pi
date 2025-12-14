include ../common.mak

SHELL = bash
VPATH := ..

# модульные тесты Lua
lua-test: $(BASELISP) stream.lsp parser.lsp lua-lib.lsp lua.lsp $(TEST) lua-test.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
