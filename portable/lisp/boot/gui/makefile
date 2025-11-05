include ../common.mak ../graphics/common.mak
GUI = element.lsp text.lsp events.lsp
SHELL = bash
VPATH := .. ../graphics/ ../sys/

# тест GUI
/tmp/gui-ascii.lsp: $(ASCII_GRAPHICS) keyboard.lsp $(GUI) test/ascii-backend.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@
