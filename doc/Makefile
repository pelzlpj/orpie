# Orpie documentation makefile
all:	manual.pdf manual.html post-build-cleanup

manual.ps: dvi
	dvips -tletterSize -Ppdf -G0 manual.dvi

dvi: manual.tex
	latex manual.tex
	latex manual.tex

manual.pdf: manual.ps
	ps2pdf manual.ps

manual.html: manual.tex
	hevea manual.tex

post-build-cleanup: manual.pdf manual.html
	rm -f *.aux *.log *.dvi *.ps

clean:
	rm -f *.aux *.log *.dvi *.ps *.pdf *.html


# arch-tag: DO_NOT_CHANGE_a5d62ea3-3a73-4de2-a2b3-a70bb310823f 
