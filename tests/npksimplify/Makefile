# commands
RM = rm -f
C2NEWSPEAK = ../../bin/c2newspeak --experimental
NPKSIMPLIFY = ../../bin/npksimplify --newspeak

TESTS = 000 001 002 003 004 005 006 007 008
TESTS.SPEC = $(addsuffix .spec, $(TESTS))
CHECK=$(addsuffix .checked, $(TESTS))

.SILENT: check $(CHECK)

check: $(CHECK)

%.checked: %.c
	eval $(C2NEWSPEAK) $*.c
	eval $(NPKSIMPLIFY) `[ -e $*.in ] && cat $*.in` a.npk &> $*.bak
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
	touch $*.checked

%.spec: %.c
	eval $(C2NEWSPEAK) $*.c
	eval $(NPKSIMPLIFY) `[ -e $*.in ] && cat $*.in` a.npk &> $*.spec
	dos2unix $*.spec
	cat $*.spec
