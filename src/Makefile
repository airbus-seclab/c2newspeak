DIR = newspeak npkstats npkstrip npksimplify npk2bytesz
DEP = $(addsuffix /.depend,$(DIR))

#DOC_DIR= ../doc/ocamldoc/
.PHONY: $(DIR) all doc depend clean

install: $(DIR)

all: $(DIR)

$(DIR):
	$(MAKE) -C $@ $(MAKECMDGOALS)

doc: $(DIR)
#	cp newspeak/doc/* $(DOC_DIR)

$(DEP):
	touch $@

clean: $(DIR)
	$(RM) *~
	$(RM) date.ml

