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

#utils
CP=cp
RM=rm -rf
OCAMLC=ocamlc -w Ael -warn-error Ael
OCAMLOPT=ocamlopt -w Ael -warn-error Ael -inline 100 -noassert -unsafe
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

CILDIR=../cil/obj
CIL=$(CILDIR)/cil.cmxa
INCLUDE=$(addprefix -I ,$(DIRS))

CMX=$(addsuffix .cmx,$(FILES))
ML=$(addsuffix .ml,$(FILES))
MLI=$(addsuffix .mli,$(FILES))

CLEANFILES+=\
	$(TARGET).depend $(addsuffix *~,$(DIRS)) \
	$(addsuffix .cmi,$(FILES)) \
	$(addsuffix .cmx,$(FILES)) $(addsuffix .o,$(FILES))

../bin/$(TARGET): $(CIL) $(CMX)
	@echo "Linking                     "$(TARGET)
	@$(OCAMLOPT) $(INCLUDE) $(LIBX) $(CMX) -o $@

$(TARGET).depend: $(ML)
	@echo "Computing dependencies for  "$(TARGET)
	@$(OCAMLDEP) $(INCLUDE) $(MLI) $(ML) > $@

clean:
	@echo "Cleaning files for          "$(TARGET)
	@$(RM) $(CLEANFILES)

#automatic rules
%.cmi: %.mli
	@echo "Compiling interface    u     "$<
	@$(OCAMLC) $(INCLUDE) $(LIBA) -c $<

%.cmo: %.ml
	@echo "Compiling source  f          "$<
	@$(OCAMLC) $(INCLUDE) $(LIBA) -c $<

%.cmx: %.ml
	@echo "Compiling source   t         "$<
	@$(OCAMLOPT) $(INCLUDE) $(LIBX) -c $<

%.mli %.ml: %.mly
	@echo "Compiling parser            "$<
	@$(OCAMLYACC) -q -v $< &> /dev/null

%.ml: %.mll
	@echo "Compiling lexer             "$<
	@$(OCAMLLEX) -q $<

-include $(TARGET).depend
