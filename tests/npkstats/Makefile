# commands
RM = rm -f
C2N = ../../bin/c2newspeak
NPKSTATS=../../bin/npkstats
C2NEWSPEAK=$(C2N) --experimental

TESTS= 000
TESTS.SPEC= $(addsuffix .spec, $(TESTS))
CHECK=$(addsuffix .checked, $(TESTS))

.SILENT: $(TESTS)
.PHONY: $(TESTS.SPEC)

check: $(TESTS)

$(TESTS):%: $(C2N) %.c
	eval $(C2NEWSPEAK) $*.c
	eval $(NPKSTATS) `[ -e $*.in ] && cat $*.in` a.npk &> $*.bak
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

$(TESTS.SPEC):%.spec: $(C2N) %.c
	eval $(C2NEWSPEAK) $*.c
	eval $(NPKSTATS) `[ -e $*.in ] && cat $*.in` a.npk &> $*.spec
	dos2unix $*.spec
	cat $*.spec

../../bin/c2newspeak: 
	make -C ../.. install

