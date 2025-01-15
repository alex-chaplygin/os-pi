include ../common.mak ../graphics/common.mak
SHELL = bash
VPATH := .. ../graphics

GAME=$(ASCII_GRAPHICS) graphics.lsp mob.lsp plat.lsp

all: /tmp/game-engine.lsp /tmp/game-graphics.lsp

# движок для платформера
/tmp/game-engine.lsp: $(GAME) test/plat.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

# графическая система для игр
/tmp/game-graphics.lsp: $(GAME) test/graphics.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

# модульные тесты мобильного объекта
mob-test: $(BASELISP) mob.lsp $(TEST) test/mob.lsp 
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
