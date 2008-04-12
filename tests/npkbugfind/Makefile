# commands
RM = rm -f
NPKBUGFIND=../../bin/npkbugfind
C2NEWSPEAK=../../bin/c2newspeak --experimental

TESTS= 000 001 002 003 004
TESTS.SPEC= $(addsuffix .spec, $(TESTS))

.SILENT: $(TESTS)
.PHONY: $(TESTS.SPEC)

check: $(TESTS)

$(TESTS):%: %.c
	eval $(C2NEWSPEAK) $*.c &> /dev/null
	eval $(NPKBUGFIND) `[ -e $*.in ] && cat $*.in` a.npk &> $*.bak
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
	eval $(C2NEWSPEAK) $*.c &> /dev/null
	eval $(NPKBUGFIND) `[ -e $*.in ] && cat $*.in` a.npk &> $*.spec
	dos2unix $*.spec
	cat $*.spec
