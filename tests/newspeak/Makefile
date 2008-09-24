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

COMMAND=./$*_check $*.npk
PREREQ=%_check %.npk
TESTS=000 001 002 003 004 005 006
EXEC=$(addsuffix _check, $(TESTS))
CLEANFILES=002.npk 003.npk 005.npk *_check *.exe 006.npk

include ../common.Makefile

.SILENT: $(EXEC)

$(EXEC): %_check: %.ml
	ocamlc -I ../../bin ../../bin/newspeak.cma $*.ml -o $*_check

%.npk: %.c
	@$(C2NEWSPEAK) $*.c -o $*.npk
