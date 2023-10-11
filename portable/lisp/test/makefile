SHELL=bash
CFLAGS=-I. -I../ --coverage -g -Wno-pointer-to-int-cast
LDFLAGS="-lgcov"
TESTS=/tmp/test_lexer /tmp/test_eval /tmp/test_symbols /tmp/test_objects /tmp/test_parser /tmp/test_eval_int

all: $(TESTS)

/tmp/test_lexer: test_lexer.c ../lexer.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_eval: test_eval.c ../eval.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_eval_int: test_eval_int.c ../eval.c ../symbols.c ../objects.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_symbols: test_symbols.c ../symbols.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_objects: test_objects.c ../objects.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_parser: test_parser.c ../parser.c ../objects.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^
/tmp/test_sys: test_sys.c ../eval.c ../symbols.c ../objects.c ../parser.c ../lexer.c ../arith.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test: $(TESTS) /tmp/test_sys
	for test in $(TESTS); do bash -c $$test ; done
	sh sys_all

coverage: $(TESTS) /tmp/test_sys
	lcov -c -i -d ../ -o /tmp/base.info
	for test in $(TESTS); do rm $$test; make $$test; bash -c $$test ; lcov -c -d ../ -o $$test.info ;	done
	./make_report
