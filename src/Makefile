DIR = newspeak npkstats
DEP = $(addsuffix /.depend,$(DIR))

#DOC_DIR= ../doc/ocamldoc/
.PHONY: $(DIR) all doc depend clean initdepend

all: $(DIR)

$(DIR):
	$(MAKE) -C $@ $(MAKECMDGOALS)

doc: $(DIR)
#	cp newspeak/doc/* $(DOC_DIR)

$(DEP):
	touch $@

clean: $(DIR)
	$(RM) *~

test: newspeak npkstats