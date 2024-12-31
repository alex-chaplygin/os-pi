include ../common.mak ../graphics/common.mak

VPATH := .. ../graphics

GAME=$(ASCII_GRAPHICS) graphics.lsp mob.lsp plat.lsp

# графическая система для игр
/tmp/game.lsp: $(GAME) test/graphics.lsp
	cat $^ >$@ ;
	echo -e -n "\xff" >>$@

# модульные тесты мобильного объекта
mob-test: $(BASELISP) mob.lsp $(TEST) test/mob.lsp 
	make -C ../../test /tmp/lisp
	cat $^ | /tmp/lisp
