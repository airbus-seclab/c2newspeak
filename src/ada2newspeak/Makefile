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
# Jasmine Duchon
# email : jasmine . duchon AT free . fr
#



#utils
CP=cp
RM=rm -rf
OCAMLC=ocamlc -w Ael -warn-error Ael
OCAMLOPT=ocamlopt -w Ael -warn-error Ael -inline 100 -noassert -unsafe
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

#includes and libraries
CILDIR=../../cil/obj
CIL=$(CILDIR)/cil.cmxa
DIRS=$(addsuffix /, ../newspeak $(CILDIR))
INCLUDE=$(addprefix -I ,$(DIRS))
LIB=nums.cma
LIBX=unix.cmxa str.cmxa nums.cmxa $(CIL)

version.FILE=../newspeak/version

newspeak.FILES:=\
	config cilutils newspeak npkcontext \
	npkil npkutils cir \
	cir2npkil link
newspeak.FILES:=$(addprefix ../newspeak/, $(newspeak.FILES))

ada2newspeak.FILES=\
	$(newspeak.FILES) \
	params \
	print_syntax_ada ada_utils \
	parser lexer ada_parse \
	ada_normalize firstpass compiler \
	ada2newspeak


ada2newspeak.CLEANFILES:=parser lexer
ada2newspeak.CLEANFILES:=$(addsuffix .ml, $(ada2newspeak.CLEANFILES)) \
	                 $(addsuffix .mli, $(ada2newspeak.CLEANFILES)) \
	                 parser.output


EXEC = ../../bin/ada2newspeak

MLI=syntax_ada.mli
ML=$(addsuffix .ml, $(ada2newspeak.FILES))
OBJS=$(addsuffix .cmx, $(version.FILE)) \
     $(addsuffix .cmx, $(ada2newspeak.FILES)) \

CLEANFILES=*~ .depend \
	$(addsuffix *~,$(DIRS)) \
	*.a *.cma *.cmxa \
	$(addsuffix .cmi,$(ada2newspeak.FILES)) \
	$(MLI:.mli=.cmi) \
	$(addsuffix .cmx,$(ada2newspeak.FILES)) \
	$(addsuffix .o,$(ada2newspeak.FILES)) \
	$(EXEC) \
	$(ada2newspeak.CLEANFILES)


all: $(EXEC)

$(EXEC): $(OBJS)
	$(OCAMLOPT) $(INCLUDE) $(LIBX) $(OBJS) -o $(EXEC)

.SUFFIXES: .ml .mli .cmi .cmx .mll .mly


.mli.cmi:
	$(OCAMLC) $(INCLUDE) $(LIB) -c $<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDE) $(LIBX) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) $<

clean:
	$(RM) $(CLEANFILES)

.depend: $(ML) $(MLI)
	 $(OCAMLDEP) $(INCLUDE) $(ML) $(MLI) $(version.FILE).ml > .depend	

include .depend
