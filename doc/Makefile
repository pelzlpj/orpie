# Orpie documentation makefile
all:	manual.pdf manual.html post-build-cleanup orpie.1 orpierc.5

manual.ps: dvi
	dvips -tletterSize -Ppdf -G0 manual.dvi

dvi: manual.tex
	latex manual.tex
	latex manual.tex

manual.tex: manual.tex.in
	latex2man -CLATEX -L manual.tex.in manual.tex

manual.pdf: manual.ps
	ps2pdf manual.ps

manual.html: manual.tex
	hevea manual.tex

manual.tex.stripped: manual.tex.in
	python remove-tt.py manual.tex.in manual.tex.stripped

orpie.1: manual.tex.stripped
	latex2man -M manual.tex.stripped orpie.1

orpierc.5: manual.tex.stripped
	latex2man -CORPIERC -M manual.tex.stripped orpierc.5

post-build-cleanup: manual.pdf manual.html orpie.1 orpierc.5
	rm -f *.aux *.log *.dvi *.ps *.stripped manual.tex

clean:
	rm -f manual.tex *.aux *.log *.dvi *.ps *.pdf *.html *.stripped orpie.1 orpierc.5


# arch-tag: DO_NOT_CHANGE_a5d62ea3-3a73-4de2-a2b3-a70bb310823f 
