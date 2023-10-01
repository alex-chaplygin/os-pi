SRC=parser.o lexer.o objects.o symbols.o eval.o arith.o
CFLAGS=-g -m32 -nostdlib -nodefaultlibs -Wno-builtin-declaration-mismatch -I../include

all: $(SRC)

/tmp/inter: $(SRC)
	gcc $^ -o $@
