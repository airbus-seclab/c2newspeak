.PHONY: all check clean 
MLFLAGS=-I +getopt -I $(NEWSPEAK)
EXTRALIB=getopt.cma newspeak.cma
EXEC=solver
OBJ=solver.cmo

all: $(EXEC)

solver: $(OBJ)
	ocamlc $(MLFLAGS) -o $@ $(EXTRALIB) $+

%.cmo: %.ml
	ocamlc $(MLFLAGS) -c $<

check:
	prove

clean:
	rm -f $(EXEC) *.cmo *.cmi
