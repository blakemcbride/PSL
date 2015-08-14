
Portable Utah Standard LISP
===========================

The bulk of the system is written in C, except the compiler which is
written in lisp.  This system is very portable.  It includes an
interpreter, a compiler to C, and documentation on the standard and
the implementation.  Note that this is not a Common Lisp lisp dialect.
It is a PSL (Portable Standard Lisp) dialect of Lisp.

This system was written by:

	Dr. Tugrul Yilmaz
	Dr. Gokturk Ucoluk
	Ersin Karabudak

It is available from:  https://github.com/blakemcbride/PSL

Through the years I (Blake McBride) contributed portability
adjustments, build adjustments, and bug fixes.  More recently,
I was given permission to release the system.  I performed
numerous adjustments to the system which (hopefully) will
make it easier to understand, build, and use.

The git repository you obtained this system from starts its repository
history with the code that has been distributed on their Internet site
( http://www.ceng.metu.edu.tr/~ucoluk/research/lisp/generalinfo.html )
then continues with the changes that were made by both them and me.
After the joint changes were applied, I numerous changes including
code re-formatting and code changes designed to simplify the build
process along with some minor corrections and enhancements.

BUILDING
--------

I have tested this system under 32 & 64 bit Linux and Mac.  It uses
gnu make but should be easy to change.  The system is very portable so
should be easy to get running under 32 or 64 bit Windows, especially
with Cygwin.

To build the interpreter just type:  make lisp

This will create the lisp executable.

The compiler is just the regular lisp interpreter with the compiler
(which is written in lisp) compiled into it.

To build the compiler type:  make lispc

Note that the process of building anything but the raw interpreter
builds an initialization file named LISP-INI.  This file is specific
to the related lisp executable created.  The means the "lisp" program
and the "lispc" program must use different LISP-INI files.  This also
means that "lisp" will not run after you create "lispc" because of the
different LISP-INI files.  (Actually "lisp", with no compiled code,
doesn't need any LISP-INI at all.)  If you compile in your own code,
you have yet another unique LISP-INI file, and so on.

COMPILING YOUR OWN PROGRAM
--------------------------

If you create a lisp file named "try.lsp", it can be compiled into the
system as follows:

    ./lispc
    (compilefile "try.lsp" 'try)
        ["anotherfile.lsp"]...                  be sure to use quotes
    end                                         -> tryc1 trye tryn1 tryu tryx1
    (quit)
    ./cr1 try try.c                             -> try.c
    ./crc try                                   -> lispc1.c
    ./cri try                                   -> LISP-INI
    gcc -o try lisp2.c try.c lispc1.c -lm       -> try

"try" will be the complete lisp system with your code compiled in.
An associated LSIP-INI file will also be created.

Notes:

lisp2.c has a constant name and content (ergo, lisp2.c is the same
regardless of what, if anything, is compiled)

lispc1.c has a constant name but the contents change based on what you
are compiling

Basically all of the lisp source files get compiled and combined into
a single executable.  You must compile all of the lisp files at the
same time.  Additional source files may be included in the compile by
typing their file names in double-quotes one after the other when
prompted by the compiler.  You type "end" (without the quotes when
done.


See the file "compiler-compile".  It shows compiling two files.


The implementation file that comes with the system (lispman) is a
little out-of-date.  I changed some things to streamline and simplify
the build process.  However, all of the internals documentation should
be correct.

The "sl.*" files in the "manual" directory contains the full
documentation on the PSL lisp dialect that this system conforms to.


    Blake McBride
    blake@mcbride.name
    blake1024@gmail.com
    blake@arahant.com
