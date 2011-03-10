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

C2NEWSPEAK=../../bin/c2newspeak

COMMAND=$(MAKE) -s $*.CMD
TESTS=000 001 002
.SILENT: $(TESTS)

check: $(TESTS)

000:
	if $(C2NEWSPEAK) -I 000.c >/dev/null 2>&1; then echo failed; false; else cat 000.spec > 000.bak; true; fi
	touch 000

001:
	$(C2NEWSPEAK) -c 000.c && $(C2NEWSPEAK) 000.no
	touch 001

002: tmp
	$(C2NEWSPEAK) -c -o tmp/000.no 000.c
	if [ -f tmp/000.no ]; then true; else echo "Failure: c2newspeak should have created output file in subdirectory tmp"; false; fi
	touch 002

tmp:
	mkdir tmp

clean:
	rm -rf *.bak *.no *.npk $(TESTS) tmp