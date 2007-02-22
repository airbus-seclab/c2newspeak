DIR = newspeak npkstats

#DOC_DIR= ../doc/ocamldoc/
.PHONY: $(DIR) all doc depend clean initdepend

all: $(DIR)

$(DIR):
	$(MAKE) -C $@ $(MAKECMDGOALS)

doc: $(DIR)
#	cp newspeak/doc/* $(DOC_DIR)

initdepend:
	for i in $(DIR); do touch $$i/.depend; done

depend: initdepend $(DIR)

clean: $(DIR)
	$(RM) *~

test: newspeak npkstats