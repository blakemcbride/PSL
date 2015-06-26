#
#  creator of lisp
# usage:
#    make          : creates lisp.
#    make install  : installs lisp.
#    make clean    : cleans all the created files except executables: lisp & sizes.
#    make sizes    : generates sizes executable, which informs about the basic data sizes.
lisp : lisp1.o lisp2.o
	gcc -ggdb -O0 -o lisp lisp1.o lisp2.o -lm
lisp1.c : flags.l fnames.l types.l sysids.l sysid.l cr1 
	./cr1
lisp2.c : flags.l fnames.l types.l zfnames.l errors.l hd.l yylex.l lisp-zfn.l lisp-fn.l \
		type.l big-n.l sysid.l sysids.l cr2
	./cr2
lisp1.o : lisp1.c Makefile  
	gcc -ggdb -O0 -Wall -pedantic -c lisp1.c
lisp2.o : lisp2.c Makefile
	 gcc -ggdb -O0 -Wall -pedantic -c lisp2.c
cr1 : cr1.c crfile.h
	 gcc -Wall -pedantic -o cr1 cr1.c
cr2 : cr2.c crfile.h
	 gcc -Wall -pedantic -o cr2 cr2.c
clean:
	 /bin/rm -f lisp?.c cr? lisp?.o sizes.o
sizes :  sizes.c flags.l
	 gcc -Wall -pedantic -o sizes sizes.c
install: lisp
	 mv lisp /usr/local/bin
	 
