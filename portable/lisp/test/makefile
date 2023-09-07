CFLAGS=-I. -I../

all: /tmp/test_lexer

/tmp/test_lexer: test_lexer.c ../lexer.c
	gcc $(CFLAGS) -o $@ $^
