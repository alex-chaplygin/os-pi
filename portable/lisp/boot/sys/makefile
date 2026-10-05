include ../common.mak
include ../fs/makefile
CONSOLE = $(BASELISP) $(PARSER) $(FSVFS) keyboard.lsp shell.lsp
NET = $(BASELISP) pci.lsp net/ethernet.lsp
SHELL = bash
VPATH := .. ../fs

all: /tmp/net.lsp /tmp/video.lsp /tmp/console.lsp

/tmp/video.lsp: $(BASELISP) ostream.lsp pci.lsp video/vmware.lsp video/test.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/net.lsp: $(NET) net/test/ethernet.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

/tmp/console.lsp: $(CONSOLE)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
