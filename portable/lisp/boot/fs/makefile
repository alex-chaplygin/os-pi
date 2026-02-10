include ../common.mak
SHELL = bash
VPATH := ..
GRAPHICS=$(BASELISP) ../sys/bgr.lsp ../graphics/matrix.lsp ../graphics/image.lsp ../graphics/screen.lsp ../graphics/backend.lsp ../graphics/frontend.lsp
ASCII_GRAPHICS=$(GRAPHICS) ../graphics/ascii-backend.lsp
FSBASE=$(ASCII_GRAPHICS) $(PARSER) ../sys/ata.lsp block.lsp fs.lsp
FSTEST=$(FSBASE) ../test.lsp test/ata.lsp test/block.lsp test/fs.lsp test/final-test.lsp
FSISO=$(FSBASE) iso9660-file.lsp iso9660-fs.lsp
TESTISO=$(FSISO) ../test.lsp test/iso9660.lsp test/final-test.lsp
TESTISODISK=$(FSISO) test/iso9660-disk.lsp
FSFAT=$(FSBASE) fat32-file.lsp fat32-fs.lsp
TESTFAT=$(FSFAT) ../test.lsp test/fat32.lsp test/final-test.lsp

all: /tmp/fs.lsp

/tmp/fs.lsp: $(TESTFAT)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
