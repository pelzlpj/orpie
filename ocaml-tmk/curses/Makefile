OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLMKLIB=ocamlmklib
CFLAGS=-Wall -fPIC -DPIC
LFLAGS=
CPP=cpp

CURSES=ncurses

OCAMLLIB=/usr/lib/ocaml/3.06
INSTALLDIR=$(OCAMLLIB)/curses
PREFIX=/usr/local
LIBDIR=$(PREFIX)/lib

all: libmlcurses.a mlcurses.cma mlcurses.cmxa

install: all
	install -d $(INSTALLDIR)
	install -m 644 *.a *.o *.so *.ml *.mli *.cmi *.cmo *.cma *.cmx *.cmxa $(INSTALLDIR)
	install -m 644 libmlcurses.a $(LIBDIR)
	ln -f -s $(INSTALLDIR)/dllmlcurses.so $(OCAMLLIB)/stublibs
	/sbin/ldconfig

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

test: test.ml
	$(OCAMLC) -o $@ -I $(INSTALLDIR) mlcurses.cma $<

clean:
	rm -f *.cm* *.o *.a dll*.so test
