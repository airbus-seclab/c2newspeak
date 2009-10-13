.PHONY: all check clean 
MLFLAGS=-I +getopt -I $(NEWSPEAK)
EXTRALIB=getopt.cma newspeak.cma
EXEC=solver
OBJ=prog.cmo pcomp.cmo solver.cmo

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
