include ../common.mak

SHELL = bash
VPATH := ..

/tmp/pci.lsp: $(BASELISP) ostream.lsp pci.lsp video/vmware.lsp video/test.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
