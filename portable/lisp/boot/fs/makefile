include ../common.mak
SHELL = bash
VPATH := ..
GRAPHICS=$(BASELISP) ../sys/bgr.lsp ../graphics/matrix.lsp ../graphics/image.lsp ../graphics/screen.lsp ../graphics/backend.lsp ../graphics/frontend.lsp
ASCII_GRAPHICS=$(GRAPHICS) ../graphics/ascii-backend.lsp
FSBASE=$(ASCII_GRAPHICS) $(PARSER) ../sys/ata.lsp ../sys/time.lsp block.lsp fs.lsp
FSTEST=$(FSBASE) ../test.lsp test/ata.lsp test/block.lsp test/fs.lsp test/final-test.lsp
FSISO=iso9660-file.lsp iso9660-fs.lsp
TESTISO=$(FSBASE) $(FSISO) ../test.lsp test/iso9660.lsp test/final-test.lsp
TESTISODISK=$(FSBASE) $(FSISO) test/iso9660-disk.lsp
FSFAT=fat32-class.lsp fat32-fat.lsp fat32-entry.lsp fat32-file.lsp fat32-fs.lsp
TESTFAT=$(FSBASE) $(FSFAT) ../test.lsp test/fat32.lsp test/final-test.lsp

all: /tmp/fs.lsp

/tmp/fs.lsp: $(TESTFAT)
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
