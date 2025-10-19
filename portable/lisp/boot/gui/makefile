include ../common.mak ../graphics/common.mak
GUI = element.lsp
SHELL = bash
VPATH := .. ../graphics/

# тест GUI
/tmp/gui-ascii.lsp: $(ASCII_GRAPHICS) $(GUI) test/ascii-backend.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
