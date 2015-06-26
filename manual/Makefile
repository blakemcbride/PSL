
all: sl.pdf lispman.pdf

sl.pdf : sl.tex sl.bib sl.bbl
	latex sl
	latex sl
	latex sl
	dvipdfm sl

lispman.pdf : lispman.tex
	latex lispman
	latex lispman
	latex lispman
	dvipdfm lispman

clean :
	rm -f *.aux *.dvi *.log *.toc *~ *.bak

realclean : clean
	rm -f *.pdf

