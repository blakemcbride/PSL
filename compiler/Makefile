#
#  creator of lisp compiler
# usage:
#    make          : creates lispc.
#    make clean    : cleans all the created files except executables: lisp & sizes.
#    make sizes    : generates sizes executable, which informs about the basic data sizes.
clisp : lisp1.o lisp2.o lispc?.o
	gcc -ggdb -O0 -o clisp lisp1.o lisp2.o lispc?.o -lm

lisp1.c : flags.l fnames.l types.l sysids.l sysid.l cr1 compn? 
	./cr1

lisp2.c : flags.l fnames.l types.l zfnames.l errors.l hd.l yylex.l lisp-zfn.l lisp-fn.l \
		type.l big-n.l sysid.l sysids.l cr2
	./cr2

lispc?.c : compc? compn? flags.l fnames.l types.l sysids.l sysid.l crc cri compu compe compx?
	   ./crc
	   ./cri 

lisp1.o : lisp1.c Makefile  
	gcc -ggdb -O0 -Wall -pedantic -c lisp1.c

lisp2.o : lisp2.c Makefile
	 gcc -ggdb -O0 -Wall -pedantic -c lisp2.c

lispc?.o : lispc?.c Makefile
	   gcc -ggdb -O0 -Wall -pedantic -c lispc?.c

cr1 : cr1.c crfile.h
	 gcc -Wall -pedantic -o cr1 cr1.c

cr2 : cr2.c crfile.h
	 gcc -Wall -pedantic -o cr2 cr2.c

crc : crc.c crfile.h
	 gcc -Wall -pedantic -o crc crc.c

cri : cri.c
	 gcc -Wall -pedantic -o cri cri.c

clean:
	 rm -f lispc?.? lisp?.c cr? lisp?.o sizes.o *.o *~

sizes :  sizes.c flags.l
	 gcc -Wall -pedantic -o sizes sizes.c

install: clisp
	 mv clisp /usr/local/bin
	 mv LISP-INI /usr/local/bin/clisp.lispini

