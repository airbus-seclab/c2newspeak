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
C2NEWSPEAK=../../bin/c2newspeak --experimental
NPKSTATS=../../bin/npkstats
NPKSTRIP=../../bin/npkstrip --newspeak
NPK2BYTESZ = ../../bin/npk2bytesz --newspeak

.SILENT: $(TESTS.OK)
.PHONY: $(TESTS.SPEC)

TESTS.OK= $(addsuffix .ok, $(TESTS))
TESTS.SPEC= $(addsuffix .spec, $(TESTS))

check: $(TESTS.OK)

$(TESTS.OK): %.ok: %.c
	eval $(C2NEWSPEAK) `[ -e $*.in1 ] && cat $*.in1` $*.c &> /dev/null
	eval $(COMMAND) `[ -e $*.in2 ] && cat $*.in2` a.npk &> $*.bak
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


$(TESTS.SPEC): %.spec: %.c
	eval $(C2NEWSPEAK) $*.c
	eval $(COMMAND) `[ -e $*.in ] && cat $*.in` a.npk &> $*.spec
	dos2unix $*.spec
	cat $*.spec
