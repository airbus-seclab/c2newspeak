.PHONY: all check clean 
MLFLAGS=-I +getopt -I $(NEWSPEAK) -w Ae -warn-error Ae
EXTRALIB=getopt.cma newspeak.cma
EXEC=solver
OBJ=options.cmo range.cmo cfg.cmo prog.cmo pcomp.cmo mkcfg.cmo fixpoint.cmo \
		tap.cmo test.cmo solver.cmo

all: $(EXEC)

solver: $(OBJ)
	ocamlc $(MLFLAGS) -o $@ $(EXTRALIB) $+

%.cmo: %.ml %.cmi
	ocamlc $(MLFLAGS) -c $<

%.cmi: %.mli
	ocamlc $(MLFLAGS) -c $<

check:
	prove

clean:
	rm -f $(EXEC) *.cmo *.cmi

pcomp.cmi: prog.cmi
mkcfg.cmi: cfg.cmi prog.cmi range.cmi pcomp.cmi
solver.cmi: mkcfg.cmi fixpoint.cmi options.cmi test.cmi
cfg.cmi: range.cmi
fixpoint.cmi: range.cmi
test.cmi: tap.cmi
