# commands
RM = rm -f
EXPERIMENTAL = 1
# TODO: find cleaner way to do these conditionals in Makefile
ifeq ($(EXPERIMENTAL),1)
FLAG = --experimental
else
FLAG =
endif
C2NEWSPEAK = ../bin/c2newspeak --newspeak $(FLAG)

TESTS = 000 001 002 003 004 005 006 007 008 009 \
        010 011 012 013 014 015 016 017 018 019 \
	020 021 022 023 024 025 026 027 028 029 \
	030 031 032 033 034 035 036 037 038 039 \
	040 041 042 043 044 045 046 047 048 049 \
	050 051 052 053 054 055 056 057 058 059 \
	060 061 062 063 064 065 066 067 068

TESTS.SPEC = $(addsuffix .spec, $(TESTS))

.SILENT: check $(TESTS)
.PHONY: $(TESTS.SPEC)

check: ../bin/c2newspeak $(TESTS)
	make -C npksimplify check

001: 026 028
000: 030 031
003: 032
011: 064
018: 035
021: 036 037
022: 038
025: 058
026: 027
033: 039 040 041 042
050: 051
063: 066

$(TESTS): $@
	eval $(C2NEWSPEAK) `[ -e $@.in ] && cat $@.in` $@.c &> $@.bak
	dos2unix $@.bak
	SPEC=$@.spec; \
	if [ $(EXPERIMENTAL) == 1 ]; \
	then if [ -e $@.spec2 ]; then SPEC=$@.spec2; fi; fi; \
	if [ -e $$SPEC ]; \
	then \
	  if diff $$SPEC $@.bak > result; \
	  then true; \
	  else cat result; false; \
	  fi; \
	else \
	  echo "$@.spec does not exist; printing output instead of diffing"; \
	  cat $@.bak; false; \
	fi
	$(RM) $@.bak result
	touch $@

$(TESTS.SPEC): ../bin/c2newspeak
	OUT=$@; \
	if [ $(EXPERIMENTAL) == 1 ]; then OUT=$@2; fi; \
	eval $(C2NEWSPEAK) `[ -e $(basename $@).in ] && cat $(basename $@).in` $(basename $@).c &> $$OUT; \
	dos2unix $$OUT; \
	cat $$OUT \

../bin/c2newspeak: 
	make -C .. install

clean:
	$(RM) $(TESTS)
	make -C npksimplify clean
	$(RM) *~ *.bak result *.no *.npk

