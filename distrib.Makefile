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

VERSION=1.7

PREFIX=/usr

#utils
CP=cp
RM=rm -rf
OCAMLDOC=ocamldoc

#FILES
EXE=c2newspeak npkstrip npkstats npkalc \
    npkcheck npkbugfind npkdiff ada2newspeak npkpointer npkflow \
    npkfuns npktests npknull simpleai npksolver npkmerger
COMPONENTS=newspeak $(EXE)

CLEANFILES=*~ bin/* lib/*~ lib/sys/*~ doc/*.html doc/*~ src/version.cmo src/*~ \
           bisect-report.xml bisect-report/*

#rules
.PHONY: clean doc lib bisect-report

all: bin $(COMPONENTS) doc lib

# for bisect support you may need the latest version
#   darcs get http://bisect.x9c.fr
coverage:
	@echo "Generating coverage report"
	$(MAKE) WITH_BISECT=1
	$(MAKE) check
	cd src/ ; bisect-report `find ../tests/ -name bisect*` \
                    -html ../../cov/$(shell date +%Y%m%d%H%M)
	ln -nsf ./$(shell date +%Y%m%d%H%M) ../cov/LATEST

install:
	@echo "Installing programs in      "$(PREFIX)/bin
	@cd bin; install $(EXE) $(PREFIX)/bin

uninstall:
	@echo "Removing programs from      "$(PREFIX)/bin
	@$(RM) $(addsuffix *,$(addprefix $(PREFIX)/bin/,$(EXE)))

lib: bin bin/lib/assert.h

bin/lib/assert.h: 
	@-mkdir bin/lib
	@echo "Copying libraries in        "bin/lib
	@$(CP) -r lib/* bin/lib

bin:
	@mkdir bin

#WARNING: do not remove the $(MAKECMDGOALS) variable, it is useful to propagate
#the rule.
#For instance, both rules 'clean' and 'all' have the prerequisite $(COMPONENTS)
#When doing make clean, this rule will be called with $(MAKECMDGOALS) 
#equal to clean
#Whereas, when doing make all, this rule will be called with $(MAKECMDGOALS) 
#equal to all
$(COMPONENTS): src/version.ml
	@$(MAKE) -s -C src -f $@.Makefile $(MAKECMDGOALS)

bisect-report:
	cd src; bisect-report `find ../tests/ -name "bisect*.out"` -xml-emma ../bisect-report.xml -html ../bisect-report

doc: doc/index.html

doc/index.html: src/newspeak/newspeak.mli
	@echo "Generating documentation in "doc/
	@$(OCAMLDOC) -I src -I src/newspeak src/newspeak/newspeak.mli src/newspeak/newspeak.ml -html -d doc -css-style newspeak.css -t "Newspeak - doubleplussimple minilang for static analysis (v. $(VERSION))" -intro doc/npkintro.mldoc -colorize-code

clean: $(COMPONENTS)
	@echo "Cleaning files installed in "bin/, doc/
	@$(RM) $(CLEANFILES)

