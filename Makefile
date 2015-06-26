#
#  creator of lisp
# usage:
#    make          : creates lisp.
#    make install  : installs lisp.
#    make clean    : cleans all the created files except executables: lisp & sizes.
#    make sizes    : generates sizes executable, which informs about the basic data sizes.

COMPILER_FILES = compc1 compe compn1 compu compx1

all: lisp lispc LISP-INI

lisp : lisp1.o lisp2.o crc cri
	gcc -ggdb -O0 -o lisp lisp1.o lisp2.o -lm

lispc : lisp1c.o lisp2.o lispc1.o
	gcc -o $@ $^ -lm
	./cri comp

LISP-INI : $(COMPILER_FILES) cri
	./cri comp

lisp1.c : flags.l fnames.l types.l sysids.l sysid.l cr1 
	./cr1

lisp2.c : flags.l fnames.l types.l zfnames.l errors.l hd.l yylex.l lisp-zfn.l \
          lisp-fn.l type.l big-n.l sysid.l sysids.l cr2
	./cr2

lisp1c.c : flags.l fnames.l types.l sysids.l sysid.l cr1 $(COMPILER_FILES)
	./cr1 comp $@

lispc1.c : crc flags.l types.l type.l hd.l sysids.l zfnames.l $(COMPILER_FILES)
	./crc comp

lisp1.o : lisp1.c
	gcc -ggdb -O0 -Wall -pedantic -c lisp1.c

lisp2.o : lisp2.c
	gcc -ggdb -O0 -Wall -pedantic -Wno-parentheses -c lisp2.c

$(COMPILER_FILES): compiler.lsp lap.lsp lisp
	./compile-compiler

cr1 : cr1.c crfile.h
	gcc -Wall -pedantic -o cr1 cr1.c

cr2 : cr2.c crfile.h
	gcc -Wall -pedantic -o cr2 cr2.c

crc : crc.c crfile.h
	gcc -Wall -pedantic -o crc crc.c

cri : cri.c
	gcc -Wall -pedantic -o cri cri.c

clean:
	rm -f lisp*.c *.o *~ *.bak
	rm -f compc1 compe compn1 compu compx1

realclean: clean
	rm -f lisp lispc cr? LISP-INI

sizes :  sizes.c flags.l
	gcc -Wall -pedantic -o sizes sizes.c

install: lisp
	mv lisp /usr/local/bin

