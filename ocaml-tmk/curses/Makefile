OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLMKLIB=ocamlmklib
CFLAGS=-Wall -fPIC -DPIC
LFLAGS=
CPP=cpp

CURSES=ncurses

all: libmlcurses.a mlcurses.cma mlcurses.cmxa

ml_curses.o: ml_curses.c functions.c
	$(OCAMLC) -ccopt "$(CFLAGS)" -c $<

libmlcurses.a: ml_curses.o
	$(OCAMLMKLIB) -o mlcurses $< -l$(CURSES)

mlcurses.cma: curses.cmo
	$(OCAMLMKLIB) -o mlcurses -linkall $^

mlcurses.cmxa: curses.cmx
	$(OCAMLMKLIB) -o mlcurses -linkall $^

curses.cmi: curses.mli
	$(OCAMLC) -c $^

curses.cmo: curses.ml curses.cmi functions.c keys.ml
	$(OCAMLC) -pp $(CPP) -c $<

curses.cmx: curses.ml curses.cmi functions.c keys.ml
	$(OCAMLOPT) -pp $(CPP) -c $<

test: test.ml mlcurses.cma libmlcurses.a
	$(OCAMLC) -o $@ mlcurses.cma $<

clean:
	rm -f *.cm* *.o *.a dll*.so test
