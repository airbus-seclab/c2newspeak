RM=rm -f
OCAMLDOC=ocamldoc

VERSION=1.1
DIR=src bin tests

.PHONY: $(DIR) all clean check install tar doc

install: src doc

check: 
	$(MAKE) -C tests

$(DIR):
	$(MAKE) -C $@ $(MAKECMDGOALS)

doc:
	$(OCAMLDOC) src/newspeak/newspeak.mli -html -d doc -css-style newspeak.css -t "Newspeak - doubleplussimple minilang for static analysis (v. $(VERSION))" -intro doc/npkintro.mldoc

clean: $(DIR)
	$(RM) *~
	$(RM) doc/*.html doc/*~

tar:
	make clean
	cd ..; tar -czf c2newspeak.tgz c2newspeak-ref
