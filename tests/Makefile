# commands
RM = rm -f
C2NEWSPEAK = ../bin/c2newspeak --newspeak

TESTS = 000 001 002 003 004 005 006 007 008 009 010 \
	011 012 013 014 015 016 017 018

TESTS.SPEC = $(addsuffix .spec, $(TESTS))

.SILENT: check $(TESTS)
.PHONY: $(TESTS.SPEC)

check: ../bin/c2newspeak $(TESTS)
	make -C npksimplify check

$(TESTS): ../bin/c2newspeak $@
	eval $(C2NEWSPEAK) `[ -e $@.in ] && cat $@.in` $@.c &> $@.bak
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
	touch $@

$(TESTS.SPEC): ../bin/c2newspeak
	eval $(C2NEWSPEAK) `[ -e $(basename $@).in ] && cat $(basename $@).in` $(basename $@).c &> $@
	dos2unix $@
	cat $@

../bin/c2newspeak: 
	make -C .. install

clean:
	$(RM) $(TESTS)
	make -C npksimplify clean
	$(RM) *~ *.bak result *.no *.npk

