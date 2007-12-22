# commands
# TODO: defining functions/macros ?
RM = rm -f
FLAG = --experimental
FLAG.CIL = 
C2N = ../bin/c2newspeak --newspeak
C2NEWSPEAK = $(C2N) $(FLAG)
C2NEWSPEAK.CIL = $(C2N) $(FLAG.CIL)

TESTS = 000 001 002 003 004 005 006 007 008 009 \
        010 011 012 013 014 015 016 017 018 019 \
	020 021 022 023 024 025 026 027 028 029 \
	030 031 032 033 034 035 036 037 038 039 \
	040 041 042 043 044 045 046 047 048 049 \
	050 051 052 053 054 055 056 057 058 059 \
	060 061 062 063 064 065 066 067 068 069 \
	070 071 072 073 074 075 076 077 078 079 \
	080 081 082 083 084 085 086 087 088 089 \
	090 091 092 093 094 095 096 097 098 099 \
	100 101 102 103 104 105 106 107 108 109 \
	110 111 112 113 114 115 116 117 118 119 \
	120 121 122 123 124 125 126 127 128 129 \
	130 131 132 133 134 135 136 137 138 139 \
	140 141 142 143 144 145 146 147 148 149 \
	150 151 152

TESTS.SPEC = $(addsuffix .spec, $(TESTS))

TESTS.CIL = $(addsuffix .cil, $(TESTS))

TESTS.CIL.SPEC = $(addsuffix .spec, $(TESTS.CIL))

.SILENT: check $(TESTS)
.PHONY: $(TESTS.SPEC) $(TESTS.CIL.SPEC)

check: ../bin/c2newspeak $(TESTS)
	make -C npksimplify check

001: 026 028
000: 030 031
003: 032
011: 064
018: 035
021: 036 037
022: 038
025: 058 145
026: 027
033: 039 040 041 042
050: 051
052: 126
063: 066
066: 080
068: 110
076: 146
114: 115

$(TESTS): OPT=$@.in
$(TESTS): SRC=$@.c
$(TESTS): TMP=$@.bak
$(TESTS): SPEC=$@.spec
$(TESTS): CIL_SPEC=$(if $(shell [ -e $@.cil.spec ] && echo true), \
	  $@.cil.spec,$@.spec)
$(TESTS): $@
	eval $(C2NEWSPEAK) `[ -e $(OPT) ] && cat $(OPT)` $(SRC) &> $(TMP)
	dos2unix $(TMP) 2&> /dev/null; true
	if [ -e $(SPEC) ]; \
	then \
	  if diff $(SPEC) $(TMP) > result; \
	  then true; \
	  else cat result; false; \
	  fi; \
	else \
	  echo "$(SPEC) does not exist; printing output instead of diffing"; \
	  cat $(TMP); false; \
	fi
	$(RM) $(TMP) result
	eval $(C2NEWSPEAK.CIL) `[ -e $(OPT) ] && cat $(OPT)` $(SRC) &> $(TMP);
	dos2unix $(TMP) 2&> /dev/null; true 
	if [ -e $(CIL_SPEC) ]; \
	then \
	  if diff $(CIL_SPEC) $(TMP) > result; \
	  then true; \
	  else cat result; false; \
	  fi; \
	else \
	  echo "$(CIL_SPEC) does not exist; printing output instead of diffing"; \
	  cat $(TMP); false; \
	fi;
	$(RM) $(TMP) result
	touch $@
	echo $@

$(TESTS.SPEC): ../bin/c2newspeak
	BASE=$(basename $(basename $@)); \
	OPT=$$BASE.in; \
	SRC=$$BASE.c; \
	OUT=$$BASE.spec; \
	eval $(C2NEWSPEAK) `[ -e $$OPT ] && cat $$OPT` $$SRC &> $$OUT; \
	dos2unix $$OUT; \
	cat $$OUT

$(TESTS.CIL.SPEC): ../bin/c2newspeak
	BASE=$(basename $(basename $@)); \
	OPT=$$BASE.in; \
	SRC=$$BASE.c; \
	OUT=$$BASE.cil.spec; \
	eval $(C2NEWSPEAK.CIL) `[ -e $$OPT ] && cat $$OPT` $$SRC &> $$OUT; \
	dos2unix $$OUT; \
	cat $$OUT


../bin/c2newspeak: 
	make -C .. install

clean:
	$(RM) $(TESTS)
	make -C npksimplify clean
	$(RM) *~ *.bak result *.no *.npk

