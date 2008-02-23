# commands
RM = rm -f
C2NEWSPEAK = ../../bin/c2newspeak --experimental --newspeak

TESTS = 000 001 002
TESTS.SPEC = $(addsuffix .spec, $(TESTS))
CHECK=$(addsuffix .checked, $(TESTS))

.SILENT: check $(CHECK)
.PHONY: $(TESTS.SPEC)

check: $(CHECK)

%.checked: ../../bin/c2newspeak %-a.c %-b.c
	eval $(C2NEWSPEAK) $*-a.c $*-b.c &> $*.bak
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

%.spec: ../../bin/c2newspeak %-a.c %-b.c
	eval $(C2NEWSPEAK) $*-a.c $*-b.c &> $*.spec
	dos2unix $*.spec
	cat $*.spec

../../bin/c2newspeak: 
	make -C ../.. install

