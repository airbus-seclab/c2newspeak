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

TARGET=c2newspeak
DIRS=$(CILDIR) utils/ newspeak/ c2newspeak/
LIBX=unix.cmxa str.cmxa nums.cmxa $(CIL)

newspeak.FILES=\
	config newspeak npkcontext \
	npkil cir \
	cir2npkil link

c2newspeak.FILES=\
        pp_syntax pp_lexer pp_parser \
        csyntax synthack lexer parser \
        spec_lexer spec_parser \
        cilutils npkutils cilenv cilfirstpass cilcompiler \
        firstpass compiler \
	params \
        c2newspeak

FILES=version utils/list_utils \
      $(addprefix newspeak/, $(newspeak.FILES)) \
      $(addprefix c2newspeak/, $(c2newspeak.FILES))

c2newspeak.CLEANFILES:=parser lexer pp_parser pp_lexer \
                       spec_parser spec_lexer
c2newspeak.CLEANFILES:=$(addsuffix .ml, $(c2newspeak.CLEANFILES)) \
                       $(addsuffix .mli, $(c2newspeak.CLEANFILES)) \
                       parser.output pp_parser.output spec_parser.output
c2newspeak.CLEANFILES:=$(addprefix c2newspeak/,$(c2newspeak.CLEANFILES))
CLEANFILES=$(c2newspeak.CLEANFILES)

include common.Makefile
