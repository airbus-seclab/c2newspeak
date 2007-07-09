# commands
RM = rm -f
C2NEWSPEAK = ../../bin/c2newspeak
NPKSIMPLIFY = ../../bin/npksimplify --newspeak

TESTS = 000 001 002 003 004

TESTS.SPEC = $(addsuffix .spec, $(TESTS))

.SILENT: check $(TESTS)
.PHONY: $(TESTS.SPEC)

check: $(C2NEWSPEAK) $(TESTS)

$(TESTS): $(C2NEWSPEAK) $@
	eval $(C2NEWSPEAK) `[ -e $@.in ] && cat $@.in` $@.c
	eval $(NPKSIMPLIFY) `[ -e $@.in ] && cat $@.in` a.npk &> $@.bak
	dos2unix $@.bak
	if [ -e $@.spec ]; \
	then \
	  if diff $@.spec $@.bak > result; \
            then true; \
            else cat result; false; \
	  fi; \
	else \
	  echo "$@.spec does not exist; printing output instead of diffing"; \
	  cat $@.bak; false; \
	fi;
	$(RM) $@.bak result

$(TESTS.SPEC): $(C2NEWSPEAK)
	eval $(C2NEWSPEAK) `[ -e $(basename $@).in ] && cat $(basename $@).in` $(basename $@).c
	eval $(NPKSIMPLIFY) `[ -e $(basename $@).in ] && cat $(basename $@).in` a.npk &> $@
	dos2unix $@
	cat $@

$(C2NEWSPEAK): 
	make -C ../.. install

clean:
	$(RM) *~ *.bak result *.no *.npk

