
Portable Standard Lisp (PSL)
============================

A complete Lisp interpreter and compiler system. The bulk of the system
is written in C, except the compiler which is written in Lisp. This
system is very portable and includes an interpreter, a compiler that
generates C code, and documentation.

Note that this is not Common Lisp. It is a PSL (Portable Standard Lisp)
dialect of Lisp.

This system was written by:

	Dr. Tugrul Yilmaz
	Dr. Gokturk Ucoluk
	Ersin Karabudak

Available from: https://github.com/blakemcbride/PSL

BUILDING
--------

The system has been tested under 32 & 64 bit Linux and Mac. It uses GNU
make but should be easy to adapt. The system is portable and should work
on 32 or 64 bit Windows, especially with Cygwin.

To build the interpreter:

    make lisp

To build the compiler (includes the interpreter with the Lisp compiler
compiled in):

    make lispc

To build both:

    make

INITIALIZATION FILES
--------------------

Each executable looks for its own initialization file using this search
order:

1. Environment variable specified via `-e` command line option
2. `LISPINI` environment variable
3. `<executable>.lispini` (e.g., `lisp.lispini`, `lispc.lispini`)
4. `LISP-INI` (legacy fallback)

The base interpreter (`lisp`) does not require an initialization file.
The compiler (`lispc`) uses `lispc.lispini` which is created during the
build process.

When you compile your own code into a custom executable, you create a
corresponding `.lispini` file for it.

COMPILING YOUR OWN PROGRAM
--------------------------

If you create a Lisp file named "try.lsp", it can be compiled into the
system as follows:

    ./lispc
    (compilefile "try.lsp" 'try)
        ["anotherfile.lsp"]...                  be sure to use quotes
    end                                         -> tryc1 trye tryn1 tryu tryx1
    (quit)
    ./cr1 try try.c                             -> try.c
    ./crc try                                   -> lispc1.c
    ./cri try try.lispini                       -> try.lispini
    gcc -o try lisp2.c try.c lispc1.c -lm       -> try

"try" will be the complete Lisp system with your code compiled in. It
will automatically use `try.lispini` as its initialization file.

Notes:

- `lisp2.c` has a constant name and content (the same regardless of what
  is compiled)

- `lispc1.c` has a constant name but contents change based on what you
  compile

- All Lisp source files get compiled and combined into a single
  executable. You must compile all files at the same time.

- Additional source files may be included by typing their filenames in
  double-quotes when prompted by the compiler. Type "end" (without
  quotes) when done.

See the file "compile-compiler" for an example that compiles two files.

DOCUMENTATION
-------------

- `manual/lispman.*` - Implementation documentation (internals)
- `manual/sl.*` - Full documentation on the PSL Lisp dialect

LICENSE
-------

See LICENSE.txt for licensing information.
