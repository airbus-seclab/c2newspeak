include ../Makefile.common
# commands
C2NEWSPEAK = ../../bin/c2newspeak --experimental
NPK2BYTESZ = ../../bin/npk2bytesz --newspeak

TESTS = 000 001 002 003 004 005 006 007 008 009
TESTS.SPEC = $(addsuffix .spec, $(TESTS))
CHECK=$(addsuffix .checked, $(TESTS))

.SILENT: check $(CHECK)
.PHONY: $(TESTS.SPEC)

check: $(CHECK)

%.checked: ../../bin/c2newspeak %.c
	eval $(C2NEWSPEAK) `[ -e $*.in ] && cat $*.in` $*.c &> $*.1.bak
	eval $(NPK2BYTESZ) a.npk &> $*.bak
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
	$(RM) $*.1.bak $*.bak result
	touch $*.spec

%.spec: ../../bin/c2newspeak %.c
	eval $(C2NEWSPEAK) `[ -e $*.in ] && cat $*.in` $*.c
	eval $(NPK2BYTESZ) a.npk &> $*.spec
	dos2unix $*.spec
	cat $*.spec

$(C2NEWSPEAK): 
	make -C ../.. install

