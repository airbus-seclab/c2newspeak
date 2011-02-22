#  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
#  well-suited for static analysis.
#  Copyright (C) 2011 Sarah Zennou
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
#  Sarah Zennou
#  EADS Innovation Works - SE/IS
#  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
#  email: sarah(dot)zennou(at)eads(dot)net

COMMAND=$(C2NEWSPEAK) `[ -e $*.in0 ] && cat $*.in0` $*-a.c -o $*-a.npk > /dev/null 2>&1; \
	$(C2NEWSPEAK) `[ -e $*.in0 ] && cat $*.in0` $*-b.c -o $*-b.npk > /dev/null 2>&1; \
        $(NPKMERGER) --print -o $*.npk `[ -e $*.in ] && cat $*.in` $*-a.npk $*-b.npk 
PREREQ=%-a.c %-b.c
TESTS=000 001
CLEANFILES=000.npk
include ../common.Makefile
