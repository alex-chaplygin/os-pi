include ../common.mak
AES=$(BASELISP) aes.lsp

SHELL = bash
VPATH := ..

aes-test: $(AES) $(TEST) test/aes.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
