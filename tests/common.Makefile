#  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
#  well-suited for static analysis.
#  Copyright (C) 2007  Charles Hymans, Olivier Levillain
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
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
#  Charles Hymans
#  EADS Innovation Works - SE/CS
#  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
#  email: charles.hymans@penjili.org

#commands
RM=rm -f
WC=wc -c
C2NEWSPEAK=../../bin/c2newspeak --experimental
NPKSTATS=../../bin/npkstats
NPKSTRIP=../../bin/npkstrip --newspeak
NPK2BYTESZ = ../../bin/npk2bytesz --newspeak
NPKBUGFIND=../../bin/npkbugfind
NPKCHECK=../../bin/npkcheck
NPKSIMPLIFY = ../../bin/npksimplify --newspeak

TESTS.OK= $(addsuffix .ok, $(TESTS))
TESTS.SPEC= $(addsuffix .spec, $(TESTS))

.SILENT: $(TESTS.OK)
.PHONY: $(TESTS.SPEC)

check: $(TESTS.OK)

$(TESTS.OK): %.ok: $(PREREQ)
	$(COMMAND) &> $*.bak; true
	dos2unix $*.bak &> /dev/null
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
	touch $*.ok
	echo $*

$(TESTS):
	make $@.ok


$(TESTS.SPEC): %.spec: $(PREREQ)
	$(COMMAND) &> $*.spec; true
	dos2unix $*.spec
	cat $*.spec
