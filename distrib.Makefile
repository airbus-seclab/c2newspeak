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

VERSION=1.3

#utils
CP=cp
RM=rm -rf
OCAMLDOC=ocamldoc

CILDIR=cil/obj
CIL=$(CILDIR)/cil.cmxa

#FILES
COMPONENTS=newspeak c2newspeak npkstrip npkstats npksimplify npk2bytesz \
           npkcheck npkbugfind npkdiff ada2newspeak npkpointer

CLEANFILES=*~ bin/* lib/*~ lib/sys/*~ doc/*.html doc/*~ src/version.cmo

#rules
.PHONY: clean doc

all: bin $(CIL) $(COMPONENTS) doc
	@echo "Installing libraries in     "bin/
	@$(CP) -r lib/* bin

bin:
	mkdir bin

$(COMPONENTS): $(CILDIR) src/version.ml
	@$(MAKE) -s -C src -f $@.Makefile $(MAKECMDGOALS)

$(CIL): $(CILDIR)
	cd cil; tar xzf cil-1.3.5.tar.gz
	cd cil/cil; patch Makefile.in ../Makefile.in.patch
	cd cil/cil; ./configure
	for i in cil/cil/obj/*; do $(CP) cil/machdep.ml $$i; done
	cd cil/cil; make
	for i in cil/cil/obj/*; do $(CP) $$i/* $(CILDIR); done

$(CILDIR):
	mkdir $(CILDIR)

doc: doc/index.html

doc/index.html:
	@echo "Generating documentation in "doc/
	@$(OCAMLDOC) -I src -I src/newspeak src/newspeak/newspeak.mli src/newspeak/newspeak.ml -html -d doc -css-style newspeak.css -t "Newspeak - doubleplussimple minilang for static analysis (v. $(VERSION))" -intro doc/npkintro.mldoc -colorize-code

clean: $(COMPONENTS)
	@echo "Cleaning files installed in "bin/, doc/
	@$(RM) $(CLEANFILES)

clean-all:
	$(MAKE) clean
	$(RM) -r cil/cil $(CILDIR)