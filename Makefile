.PHONY: all check clean 
MLFLAGS=-I +getopt -I $(NEWSPEAK) -w A -warn-error A
EXTRALIB=getopt.cma newspeak.cma
EXEC=solver
OBJ=range.cmo cfg.cmo prog.cmo mkcfg.cmo pcomp.cmo fixpoint.cmo solver.cmo

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
mkcfg.cmi: cfg.cmi prog.cmi range.cmi
solver.cmi: mkcfg.cmi
cfg.cmi: range.cmi
solver.cmi: fixpoint.cmi
fixpoint.cmi: range.cmi
