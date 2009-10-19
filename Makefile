.PHONY: all check clean 
MLFLAGS=-I $(NEWSPEAK) -w Ae -warn-error Ae
EXTRALIB=newspeak.cma
EXEC=npksolver
OBJ=options.cmo utils.cmo range.cmo cfg.cmo prog.cmo pcomp.cmo mkcfg.cmo fixpoint.cmo \
		tap.cmo test.cmo npksolver.cmo

all: $(EXEC)

npksolver: $(OBJ)
	ocamlc $(MLFLAGS) -o $@ $(EXTRALIB) $+

%.cmo: %.ml %.cmi
	ocamlc $(MLFLAGS) -c $<

%.cmi: %.mli
	ocamlc $(MLFLAGS) -c $<

check:
	prove t/*.t

clean:
	rm -f $(EXEC) *.cmo *.cmi

pcomp.cmi: prog.cmi utils.cmi options.cmi
mkcfg.cmi: cfg.cmi prog.cmi range.cmi pcomp.cmi
npksolver.cmi: mkcfg.cmi fixpoint.cmi options.cmi test.cmi
cfg.cmi: range.cmi
fixpoint.cmi: range.cmi options.cmi utils.cmi
test.cmi: tap.cmi
