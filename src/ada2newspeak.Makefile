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
DIRS=$(CILDIR) newspeak/ ada2newspeak/
newspeak.FILES=\
	config newspeak npkcontext \
	npkil cir \
	cir2npkil link
ada2newspeak.FILES=\
	params \
	syntax_ada print_syntax_ada ada_utils \
	parser lexer ada_parse \
	ada_normalize firstpass compiler \
	ada2newspeak
FILES=version \
      $(addprefix newspeak/, $(newspeak.FILES)) \
      $(addprefix ada2newspeak/, $(ada2newspeak.FILES))
LIBX=unix.cmxa str.cmxa nums.cmxa $(CIL)
CLEANFILES:=parser lexer
CLEANFILES:=$(addsuffix .ml, $(CLEANFILES)) \
	    $(addsuffix .mli, $(CLEANFILES)) \
	    parser.output
CLEANFILES:=$(addprefix ada2newspeak/,$(CLEANFILES))

include common.Makefile
