LIBS=/usr/lib/ocaml/3.06/gsl/

all: rpc2

rpc2: rpc_stack.cmx utility.cmx add.cmx sub.cmx mult.cmx div.cmx inv.cmx pow.cmx rpc_calc.cmx main.cmx
	ocamlopt.opt -o rpc2 -I $(LIBS) bigarray.cmxa gsl.cmxa nums.cmxa rpc_stack.cmx utility.cmx \
	add.cmx sub.cmx mult.cmx div.cmx inv.cmx pow.cmx rpc_calc.cmx main.cmx;
	strip rpc2

rpc_stack.cmx: rpc_stack.ml
	ocamlopt.opt -c -I $(LIBS) rpc_stack.ml

utility.cmx: utility.ml
	ocamlopt.opt -c -I $(LIBS) utility.ml

add.cmx: add.ml
	ocamlopt.opt -c -I $(LIBS) add.ml

sub.cmx: sub.ml
	ocamlopt.opt -c -I $(LIBS) sub.ml

mult.cmx: mult.ml
	ocamlopt.opt -c -I $(LIBS) mult.ml

div.cmx: div.ml
	ocamlopt.opt -c -I $(LIBS) div.ml

inv.cmx: inv.ml
	ocamlopt.opt -c -I $(LIBS) inv.ml

pow.cmx: pow.ml
	ocamlopt.opt -c -I $(LIBS) pow.ml

rpc_calc.cmx: rpc_calc.ml
	ocamlopt.opt -c -I $(LIBS) rpc_calc.ml

main.cmx: main.ml
	ocamlopt.opt -c -I $(LIBS) main.ml

clean:
	rm -f rpc2 *.o *.cmx *.cmo *.cmi *~

# arch-tag: DO_NOT_CHANGE_ceed92a3-91ab-4283-8529-d60a163bccb1 
