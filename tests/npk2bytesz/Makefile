include ../Makefile.common
# commands
C2NEWSPEAK = ../../bin/c2newspeak --experimental
NPK2BYTESZ = ../../bin/npk2bytesz --newspeak

TESTS = 000 001 002 003 004 005 006 007 008

TESTS.SPEC = $(addsuffix .spec, $(TESTS))

.SILENT: check $(TESTS)
.PHONY: $(TESTS.SPEC)

check: $(TESTS)

$(TESTS): ../../bin/c2newspeak $@
	eval $(C2NEWSPEAK) `[ -e $@.in ] && cat $@.in` $@.c &> $@.1.bak
	eval $(NPK2BYTESZ) a.npk &> $@.bak
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
	$(RM) $@.1.bak $@.bak result
	touch $@

$(TESTS.SPEC): ../../bin/c2newspeak
	eval $(C2NEWSPEAK) `[ -e $(basename $@).in ] && cat $(basename $@).in` $(basename $@).c
	eval $(NPK2BYTESZ) a.npk &> $@
	dos2unix $@
	cat $@

$(C2NEWSPEAK): 
	make -C ../.. install

clean:
	$(RM) $(TESTS)
	$(RM) *~ *.bak result *.no *.npk

