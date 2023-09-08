CFLAGS=-I. -I../ -fprofile-arcs -ftest-coverage
LDFLAGS="-lgcov"

all: /tmp/test_lexer /tmp/test_eval /tmp/test_symbols

/tmp/test_lexer: test_lexer.c ../lexer.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_eval: test_eval.c ../eval.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_symbols: test_symbols.c ../symbols.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
