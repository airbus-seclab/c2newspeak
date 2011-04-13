#  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
#  well-suited for static analysis.
#  Copyright (C) 2007, 2011  Charles Hymans, Olivier Levillain, Sarah Zennou
#  
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#  
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#  
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, 
#  MA  02110-1301  USA
#
#  Charles Hymans
#  email: charles.hymans@penjili.org
#
#  Sarah Zennou
#  sarah(dot)zennou(at)eads(dot)net

#commands
RM=rm -f
WC=wc -c
C2NEWSPEAK=../../bin/c2newspeak
ADA2NEWSPEAK=../../bin/ada2newspeak
NPKSTATS=../../bin/npkstats
NPKSTRIP=../../bin/npkstrip
NPKCHECK=../../bin/npkcheck
SIMPLEAI=../../bin/simpleai
NPKSOLVER=../../bin/npksolver
NPKMERGER=../../bin/npkmerger

ifeq ($(strip $(DIFF)),)
DIFF=diff $*.spec $*.bak
endif

TESTS.SPEC=$(addsuffix .spec, $(TESTS))
CLEANFILES+=$(TESTS) $(addsuffix .bak, $(TESTS)) *.no \
            result *~ a.npk b.npk *-a.npk *-b.npk *.cmi *.cmo bisect*.out

.SILENT: $(TESTS)
.PHONY: $(TESTS.SPEC)

check: $(TESTS)

$(TESTS): %: $(PREREQ)
	$(COMMAND) >$*.bak 2>&1; true
	if [ -e $*.spec ]; \
	then \
	  if $(DIFF) > result; \
            then true; \
            else cat result; false; \
	  fi; \
	else \
	  echo "$*.spec does not exist; printing output instead of diffing"; \
	  cat $*.bak; false; \
	fi
	$(RM) $*.bak result
	touch $*
	echo $*

$(TESTS.SPEC): %.spec: $(PREREQ)
	$(COMMAND) > $*.spec 2>&1; true
	cat $*.spec

clean:
	@$(RM) $(CLEANFILES)
