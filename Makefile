CP=cp
RM=rm -f
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLC=ocamlc -w Ael -warn-error Ael
OCAMLOPT=ocamlopt -w Ael -warn-error Ael -inline 10 -noassert -unsafe
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc

VERSION=1.1
DIR=src bin tests

INCLUDE=-I $(CIL) -I src/ -I src/newspeak/
LIB=nums.cma
LIBX=unix.cmxa str.cmxa nums.cmxa $(CIL)/cil.cmxa

C2NFILES:=config cilutils params npkcontext newspeak \
         pp_syntax pp_lexer pp_parser \
         csyntax cir synthack lexer parser \
         npkil npkutils cilenv \
         cilfirstpass cilcompiler \
         firstpass compiler \
         link c2newspeak
C2NFILES:=version $(addprefix newspeak/, $(C2NFILES))
C2NFILES:=$(addprefix src/, $(C2NFILES))
C2NCMX=$(addsuffix .cmx, $(C2NFILES))

MLI=$(addsuffix .mli, $(C2NFILES))
ML=$(addsuffix .ml, $(C2NFILES))

NPKFILES=src/newspeak/newspeak.cmi newspeak.cma newspeak.cmxa newspeak.a

.PHONY: $(DIR) all clean check install tar doc

install: c2newspeak src doc npkstrip newspeak

newspeak: $(NPKFILES)
	$(CP) $(NPKFILES) bin

c2newspeak: $(C2NCMX)
	$(OCAMLOPT) $(INCLUDE) $(LIBX) $(C2NCMX) -o bin/c2newspeak

#TODO: clean up this
npkstrip: src/version.cmx src/newspeak/newspeak.cmx src/npkstrip/npkstrip.cmx
	$(OCAMLOPT) $(INCLUDE) $(LIBX) src/version.cmx src/newspeak/newspeak.cmx src/npkstrip/npkstrip.cmx -o bin/npkstrip

#Version number generation
src/version.ml:
	hg parents --template 'let date = "{date|shortdate}"\n' > src/version.ml
	hg parents --template 'let version = "$(VERSION)"\n' >> src/version.ml
	hg parents --template 'let revision = "{node|short}"\n' >> src/version.ml

#TODO: clean up this
newspeak.cma: src/version.cmi src/version.cmx src/newspeak/newspeak.ml
	$(OCAMLC) $(INCLUDE) $(LIB) -a src/version.ml src/newspeak/newspeak.ml -o newspeak.cma

#TODO: clean up this
newspeak.a newspeak.cmxa: src/version.cmi src/version.cmx src/newspeak/newspeak.ml
	$(OCAMLOPT) $(INCLUDE) -a src/version.cmx src/newspeak/newspeak.ml -o newspeak.cmxa

#Implicit rules
%.cmi: %.mli
	$(OCAMLC) $(INCLUDE) $(LIB) -c $<

%.cmo: %.ml
	$(OCAMLC) $(INCLUDE) $(LIB) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(INCLUDE) $(LIBX) -c $<

%.ml: %.mll
	$(OCAMLLEX) $<

%.mli %.ml: %.mly
	$(OCAMLYACC) -v $<

check: 
	$(MAKE) -C tests

$(DIR):
	$(MAKE) -C $@ $(MAKECMDGOALS)

doc:
	$(OCAMLDOC) src/newspeak/newspeak.mli -html -d doc -css-style newspeak.css -t "Newspeak - doubleplussimple minilang for static analysis (v. $(VERSION))" -intro doc/npkintro.mldoc -colorize-code

clean: $(DIR)
	$(RM) *~ .depend
	$(RM) *.a *.cma *.cmxa
	$(RM) bin/*.a bin/*.cma bin/*.cmxa
	$(RM) src/newspeak/*.a src/newspeak/*.cma src/newspeak/*.cmxa
	$(RM) src/npkstrip/*.cmi src/npkstrip/*.cmx src/npkstrip/*.o
	$(RM) src/npkstrip/npkstrip src/npkstrip/npkstrip.exe
	$(RM) src/newspeak/*.o src/newspeak/*.cmi src/newspeak/*.cmx
	$(RM) src/newspeak/*~
	$(RM) src/newspeak/parser.ml src/newspeak/parser.mli
	$(RM) src/newspeak/parser.output src/newspeak/lexer.ml
	$(RM) src/newspeak/pp_parser.ml src/newspeak/pp_parser.mli
	$(RM) src/newspeak/pp_parser.output src/newspeak/pp_lexer.ml
	$(RM) src/version.ml
	$(RM) doc/*.html doc/*~

tar:
	make clean
	cd ..; tar -czf c2newspeak.tgz c2newspeak-ref

.depend: src/version.ml $(ML)
	$(OCAMLDEP) $(INCLUDE) $(MLI) $(ML) > .depend

include .depend