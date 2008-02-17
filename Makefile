CP=cp
RM=rm -f
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLC=ocamlc -w Ae -warn-error Ae
OCAMLOPT=ocamlopt -w Ae -warn-error Ae -inline 10 -noassert -unsafe
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc

VERSION=1.1
DIR=src bin tests

INCLUDE=-I src/newspeak

.PHONY: $(DIR) all clean check install tar doc

install: src doc npkstrip lib

lib: src/newspeak/newspeak.cmi src/newspeak/newspeak.cma src/newspeak/newspeak.cmxa
	$(CP) src/newspeak/newspeak.cmi bin
	$(CP) src/newspeak/newspeak.cma bin
	$(CP) src/newspeak/newspeak.a bin
	$(CP) src/newspeak/newspeak.cmxa bin

npkstrip: src/newspeak/newspeak.cmx src/npkstrip/npkstrip.cmx
	$(OCAMLOPT) $(INCLUDE) nums.cmxa src/newspeak/newspeak.cmx src/npkstrip/npkstrip.cmx -o bin/npkstrip

#Implicit rules
%.cmi: %.mli
	$(OCAMLC) $(INCLUDE) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(INCLUDE) -c $<

%.cma: %.ml
	$(OCAMLC) $(INCLUDE) -a $< -o $@

%.cmxa: %.ml
	$(OCAMLOPT) $(INCLUDE) -a $< -o $@

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
	$(RM) doc/*.html doc/*~

tar:
	make clean
	cd ..; tar -czf c2newspeak.tgz c2newspeak-ref

.depend:
	$(OCAMLDEP) src/newspeak/newspeak.ml > .depend

include .depend