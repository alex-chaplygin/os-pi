CFLAGS=-I. -I../

all: /tmp/test_lexer /tmp/test_eval /tmp/test_symbols

/tmp/test_lexer: test_lexer.c ../lexer.c
	gcc $(CFLAGS) -o $@ $^
/tmp/test_eval: test_eval.c ../eval.c
	gcc $(CFLAGS) -o $@ $^
/tmp/test_symbols: test_symbols.c ../symbols.c
	gcc $(CFLAGS) -o $@ $^
