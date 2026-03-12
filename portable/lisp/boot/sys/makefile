include ../common.mak
CONSOLE = $(BASELISP) $(PARSER) keyboard.lsp shell.lsp
SHELL = bash
VPATH := ..

all: /tmp/pci.lsp /tmp/console.lsp

/tmp/pci.lsp: $(BASELISP) ostream.lsp pci.lsp video/vmware.lsp video/test.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/console.lsp: $(CONSOLE)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
