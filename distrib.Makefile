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

#TODO: simplify more by using vpath
VERSION=1.2

#utils
CP=cp
RM=rm -rf
OCAMLC=ocamlc -w Ael -warn-error Ael
OCAMLOPT=ocamlopt -w Ael -warn-error Ael -inline 100 -noassert -unsafe
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

#Makefile setup
.SECONDEXPANSION:
vpath %.cmi src:src/newspeak

#FILES
CILDIR=cil/obj
CIL=$(CILDIR)/cil.cmxa

DIRS:=newspeak npkstrip npkstats npksimplify npk2bytesz npkcheck
DIRS:= $(CILDIR) src/ $(addsuffix /,$(addprefix src/,$(DIRS)))
INCLUDE=$(addprefix -I ,$(DIRS))
LIB=nums.cma
LIBX=unix.cmxa str.cmxa nums.cmxa $(CIL)

INSTALL.FILES=src/newspeak/newspeak.cmi newspeak.cma newspeak.cmxa \
              newspeak.a lib/*

c2newspeak.FILES:=\
        config cilutils params newspeak npkcontext \
        pp_syntax pp_lexer pp_parser \
        csyntax cir synthack lexer parser \
        spec_lexer spec_parser \
        npkil npkutils cilenv \
        cilfirstpass cilcompiler \
        firstpass compiler \
        link c2newspeak
c2newspeak.FILES:=version $(addprefix newspeak/, $(c2newspeak.FILES))
c2newspeak.FILES:=$(addprefix src/,$(c2newspeak.FILES))

npkstrip.FILES:=version newspeak/newspeak npkstrip/npkstrip
npkstrip.FILES:=$(addprefix src/,$(npkstrip.FILES))

npkstats.FILES:=version newspeak/newspeak npkstats/stackcount npkstats/npkstats
npkstats.FILES:=$(addprefix src/,$(npkstats.FILES))

npksimplify.FILES:=normalize store copy_propagation inline \
                   var_hoist npksimplify
npksimplify.FILES:=version newspeak/newspeak \
                   $(addprefix npksimplify/,$(npksimplify.FILES))
npksimplify.FILES:=$(addprefix src/,$(npksimplify.FILES))

npk2bytesz.FILES:=version newspeak/newspeak npk2bytesz/npk2bytesz
npk2bytesz.FILES:=$(addprefix src/,$(npk2bytesz.FILES))

npkcheck.FILES:=version newspeak/newspeak npkcheck/npkcheck
npkcheck.FILES:=$(addprefix src/,$(npkcheck.FILES))

FILES=$(c2newspeak.FILES) $(npkstrip.FILES) $(npkstats.FILES) \
      $(npksimplify.FILES) $(npk2bytesz.FILES) $(npkcheck.FILES)
ML=$(addsuffix .ml,$(FILES))

COMPONENTS:=c2newspeak npkstrip npkstats npksimplify npk2bytesz npkcheck
COMPONENTS:=$(addprefix bin/,$(COMPONENTS))

NEWSPEAK=src/version.ml src/newspeak/newspeak.ml

c2newspeak.CLEANFILES:=parser lexer pp_parser pp_lexer spec_parser spec_lexer
c2newspeak.CLEANFILES:=$(addsuffix .ml, $(c2newspeak.CLEANFILES)) \
                       $(addsuffix .mli, $(c2newspeak.CLEANFILES)) \
                       parser.output pp_parser.output spec_parser.output
c2newspeak.CLEANFILES:=$(addprefix src/newspeak/,$(c2newspeak.CLEANFILES))
CLEANFILES=*~ .depend \
	*.a *.cma *.cmxa \
        bin/* src/*~ \
        doc/*.html doc/*~ \
	lib/*~ lib/sys/*~ \
	src/version.cmo src/newspeak/newspeak.cmo \
	$(addsuffix .cmi,$(FILES)) $(addsuffix .cmx,$(FILES)) \
	$(addsuffix .o,$(FILES)) \
	$(c2newspeak.CLEANFILES)

#functions
suffix.cmx=$(addsuffix .cmx,$(1))

#rules
.PHONY: all clean install doc

install: $(COMPONENTS) bin/newspeak.cmxa doc

$(CIL):
	cd cil; tar xzf cil-1.3.5.tar.gz
	cd cil/cil; ./configure
	$(CP) cil/machdep.ml cil/cil/obj/x86_WIN32/
	$(CP) cil/machdep.ml cil/cil/obj/x86_LINUX/
	cd cil/cil; make
	-$(CP) cil/cil/obj/x86_WIN32/* $(CILDIR)
	-$(CP) cil/cil/obj/x86_LINUX/* $(CILDIR)

bin/newspeak.cmxa: $(INSTALL.FILES)
	$(CP) -r $(INSTALL.FILES) bin

newspeak.cma: version.cmi newspeak.cmi $(NEWSPEAK)
	$(OCAMLC) $(INCLUDE) $(LIB) -a $(NEWSPEAK) -o newspeak.cma

newspeak.a newspeak.cmxa: version.cmi newspeak.cmi $(NEWSPEAK)
	$(OCAMLOPT) $(INCLUDE) -a $(NEWSPEAK) -o newspeak.cmxa

%.cmi: %.mli
	$(OCAMLC) $(INCLUDE) $(LIB) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(INCLUDE) $(LIBX) -c $<

%.mli %.ml: %.mly
	$(OCAMLYACC) -v $<

%.ml: %.mll
	$(OCAMLLEX) $<

doc: doc/index.html

doc/index.html: src/version.cmi src/newspeak/newspeak.cmi
	$(OCAMLDOC) -I src -I src/newspeak src/newspeak/newspeak.mli src/newspeak/newspeak.ml -html -d doc -css-style newspeak.css -t "Newspeak - doubleplussimple minilang for static analysis (v. $(VERSION))" -intro doc/npkintro.mldoc -colorize-code

clean-all: clean
	$(RM) -r cil/cil $(CILDIR)

clean:
	$(RM) $(CLEANFILES)

.depend: $(MLI) $(ML)
	@mkdir bin 2> /dev/null; true
	@mkdir $(CILDIR) 2> /dev/null; true
	@$(OCAMLDEP) $(INCLUDE) $(MLI) $(ML) > $(TARGET).depend

$(COMPONENTS): bin/%: $(CIL) $$(call suffix.cmx,$$($$*.FILES))
	$(OCAMLOPT) $(INCLUDE) $(LIBX) $(call suffix.cmx,$($*.FILES)) -o bin/$*

include .depend

