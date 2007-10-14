# commands
RM = rm -f
C2NEWSPEAK = ../bin/c2newspeak --newspeak #--experimental

TESTS = 000 001 002 003 004 005 006 007 008 009 \
        010 011 012 013 014 015 016 017 018 019 \
	020 021 022 023 024 025 026 027 028 029 \
	030 031 032 033 034 035 036

TESTS.SPEC = $(addsuffix .spec, $(TESTS))

.SILENT: check $(TESTS)
.PHONY: $(TESTS.SPEC)

check: ../bin/c2newspeak $(TESTS)
	make -C npksimplify check

001: 026 028
000: 030 031
003: 032
018: 035
021: 036
026: 027

$(TESTS): $@
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

