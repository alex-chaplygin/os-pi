include ../common.mak

SHELL = bash
VPATH := ..

/tmp/pci.lsp: $(BASELISP) pci.lsp test-pci.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
