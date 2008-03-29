# commands
RM = rm -f
C2NEWSPEAK=../../bin/c2newspeak
NPKCHECK=../../bin/npkcheck

TESTS= 000
TESTS.SPEC= $(addsuffix .spec, $(TESTS))

.SILENT: $(TESTS)
.PHONY: $(TESTS.SPEC)

check: $(TESTS)

$(TESTS):%: %.c
	eval $(C2NEWSPEAK) $*.c
	eval $(NPKCHECK) `[ -e $*.in ] && cat $*.in` a.npk &> $*.bak; true
	dos2unix $*.bak
	if [ -e $*.spec ]; \
	then \
	  if diff $*.spec $*.bak > result; \
            then true; \
            else cat result; false; \
	  fi; \
	else \
	  echo "$*.spec does not exist; printing output instead of diffing"; \
	  cat $*.bak; false; \
	fi;
	$(RM) $*.bak result
	touch $*

$(TESTS.SPEC):%.spec: %.c
	eval $(C2NEWSPEAK) $*.c
	eval $(NPKCHECK) `[ -e $*.in ] && cat $*.in` a.npk &> $*.spec; true
	dos2unix $*.spec
	cat $*.spec

