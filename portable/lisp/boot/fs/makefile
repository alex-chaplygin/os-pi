include ../common.mak
SHELL = bash
VPATH := ..
FS=$(BASELISP) ../sys/ata.lsp block.lsp fs.lsp fat32fs.lsp fat.lsp dir.lsp

all: /tmp/fs.lsp

# файловая система
/tmp/fs.lsp: $(FS) mbr.lsp fat32test.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
