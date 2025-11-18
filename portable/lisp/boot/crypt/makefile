include ../common.mak
AES=$(BASELISP) aes.lsp
BLOWFISH=$(BASELISP) blowfish.lsp

SHELL = bash
VPATH := ..

aes-test: $(AES) $(TEST) test/aes.lsp
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp

blowfish-test: $(BLOWFISH) $(TEST)
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
