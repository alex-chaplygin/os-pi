CFLAGS=-I. -I../ -fprofile-arcs -ftest-coverage -g -Wno-pointer-to-int-cast
LDFLAGS="-lgcov"
TESTS=/tmp/test_lexer /tmp/test_eval /tmp/test_symbols /tmp/test_objects /tmp/test_parser

all: $(TESTS)

/tmp/test_lexer: test_lexer.c ../lexer.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_eval: test_eval.c ../eval.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_symbols: test_symbols.c ../symbols.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_objects: test_objects.c ../objects.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_parser: test_parser.c ../parser.c ../objects.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test: $(TESTS)
	for test in $(TESTS); do bash -c $$test ; done
