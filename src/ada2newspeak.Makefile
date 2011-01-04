#
# C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
# well-suited for static analysis.
# Copyright (C) 2007  Charles Hymans, Olivier Levillain
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
#
# Charles Hymans
# EADS Innovation Works - SE/CS
# 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
# email: charles.hymans@penjili.org
#

TARGET=ada2newspeak
DIRS=$(CILDIR) utils newspeak ada2newspeak
LIBX=unix.cmxa str.cmxa nums.cmxa 

newspeak.FILES=\
	temps config eBigInt newspeak npkcontext lowspeak npk2lpk \
	npkil cir cir2npkil linker x2newspeak

ada2newspeak.FILES=\
	params adaSyntax adaTypes symboltbl ada_utils print_syntax_ada \
	ast typecheck eval parser lexer	file_parse normalize firstpass \
	ada2newspeak

FILES=version utils/listUtils utils/tree utils/standardApplication \
      $(addprefix newspeak/, $(newspeak.FILES)) \
      $(addprefix ada2newspeak/, $(ada2newspeak.FILES))

LIBX=unix.cmxa str.cmxa nums.cmxa $(CIL)

CLEANFILES:=parser lexer

CLEANFILES:=$(addsuffix .ml, $(CLEANFILES)) \
	    $(addsuffix .mli, $(CLEANFILES)) \
	    parser.output \
        lexer.mll.cmp parser.mly.cmp
CLEANFILES:=$(addprefix ada2newspeak/,$(CLEANFILES))

include common.Makefile
